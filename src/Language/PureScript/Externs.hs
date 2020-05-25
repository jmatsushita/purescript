{-# LANGUAGE TemplateHaskell #-}

-- |
-- This module generates code for \"externs\" files, i.e. files containing only
-- foreign import declarations.
--
module Language.PureScript.Externs
  ( ExternsFile(..)
  , ExternsImport(..)
  , ExternsFixity(..)
  , ExternsTypeFixity(..)
  , ExternsDeclaration(..)
  , ExternsSourceAnn(..)
  , externsFileDeclarations
  , externsIsCurrentVersion
  , moduleToExternsFile
  , applyExternsFileToEnvironment
  , externsFileName
  , sourceAnnToExterns
  , sourceAnnFromExterns
  ) where

import Prelude.Compat

import Codec.Serialise (Serialise)
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.List (foldl', find)
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NEL

import Language.PureScript.AST
import Language.PureScript.Comments
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.Roles
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

import Paths_purescript as Paths

-- | The data which will be serialized to an externs file
data ExternsFile = ExternsFile
  -- NOTE: Make sure to keep `efVersion` as the first field in this
  -- record, so the derived Serialise instance produces CBOR that can
  -- be checked for its version independent of the remaining format
  { efVersion :: Text
  -- ^ The externs version
  , efModuleName :: ModuleName
  -- ^ Module name
  , efExports :: [DeclarationRef]
  -- ^ List of module exports
  , efImports :: [ExternsImport]
  -- ^ List of module imports
  , efFixities :: [ExternsFixity]
  -- ^ List of operators and their fixities
  , efTypeFixities :: [ExternsTypeFixity]
  -- ^ List of type operators and their fixities
  , efDeclarations :: [ExternsDeclaration ExternsSourceAnn]
  -- ^ List of type and value declaration
  , efSourceSpan :: SourceSpan
  -- ^ Source span for error reporting
  } deriving (Show, Generic)

instance Serialise ExternsFile

-- | A module import in an externs file
data ExternsImport = ExternsImport
  {
  -- | The imported module
    eiModule :: ModuleName
  -- | The import type: regular, qualified or hiding
  , eiImportType :: ImportDeclarationType
  -- | The imported-as name, for qualified imports
  , eiImportedAs :: Maybe ModuleName
  } deriving (Show, Generic)

instance Serialise ExternsImport

-- | A fixity declaration in an externs file
data ExternsFixity = ExternsFixity
  {
  -- | The associativity of the operator
    efAssociativity :: Associativity
  -- | The precedence level of the operator
  , efPrecedence :: Precedence
  -- | The operator symbol
  , efOperator :: OpName 'ValueOpName
  -- | The value the operator is an alias for
  , efAlias :: Qualified (Either Ident (ProperName 'ConstructorName))
  } deriving (Show, Generic)

instance Serialise ExternsFixity

-- | A type fixity declaration in an externs file
data ExternsTypeFixity = ExternsTypeFixity
  {
  -- | The associativity of the operator
    efTypeAssociativity :: Associativity
  -- | The precedence level of the operator
  , efTypePrecedence :: Precedence
  -- | The operator symbol
  , efTypeOperator :: OpName 'TypeOpName
  -- | The value the operator is an alias for
  , efTypeAlias :: Qualified (ProperName 'TypeName)
  } deriving (Show, Generic)

instance Serialise ExternsTypeFixity

-- | A type or value declaration appearing in an externs file.
data ExternsDeclaration a =
  -- | A type declaration
    EDType
      { edTypeName                :: ProperName 'TypeName
      , edTypeKind                :: Type a
      , edTypeDeclarationKind     :: TypeKind
      }
  -- | A role declaration
  | EDRole
      { edRoleTypeName            :: ProperName 'TypeName
      , edRoleRoles               :: [Role]
      }
  -- | A type synonym
  | EDTypeSynonym
      { edTypeSynonymName         :: ProperName 'TypeName
      , edTypeSynonymArguments    :: [(Text, Maybe (Type a))]
      , edTypeSynonymType         :: Type a
      }
  -- | A data constructor
  | EDDataConstructor
      { edDataCtorName            :: ProperName 'ConstructorName
      , edDataCtorOrigin          :: DataDeclType
      , edDataCtorTypeCtor        :: ProperName 'TypeName
      , edDataCtorType            :: Type a
      , edDataCtorFields          :: [Ident]
      }
  -- | A value declaration
  | EDValue
      { edValueName               :: Ident
      , edValueType               :: Type a
      }
  -- | A type class declaration
  | EDClass
      { edClassName               :: ProperName 'ClassName
      , edClassTypeArguments      :: [(Text, Maybe (Type a))]
      , edClassMembers            :: [(Ident, Type a)]
      , edClassConstraints        :: [Constraint a]
      , edFunctionalDependencies  :: [FunctionalDependency]
      , edIsEmpty                 :: Bool
      }
  -- | An instance declaration
  | EDInstance
      { edInstanceClassName       :: Qualified (ProperName 'ClassName)
      , edInstanceName            :: Ident
      , edInstanceForAll          :: [(Text, Type a)]
      , edInstanceKinds           :: [Type a]
      , edInstanceTypes           :: [Type a]
      , edInstanceConstraints     :: Maybe [Constraint a]
      , edInstanceChain           :: [Qualified Ident]
      , edInstanceChainIndex      :: Integer
      }
  deriving (Show, Generic, Functor)

instance Serialise a => Serialise (ExternsDeclaration a)

-- | Like SourceAnn, except that it doesn't include the file name. This way we
-- avoid having multiple copies of the file name inside an externs file,
-- which cuts down on file size and parsing time.
data ExternsSourceAnn = ExternsSourceAnn
  { esaStart :: SourcePos
  , esaEnd :: SourcePos
  , esaComments :: [Comment]
  }
  deriving (Show, Generic)

instance Serialise ExternsSourceAnn

sourceAnnToExterns :: SourceAnn -> ExternsSourceAnn
sourceAnnToExterns (ss, comments) =
  ExternsSourceAnn
    { esaStart = spanStart ss
    , esaEnd = spanEnd ss
    , esaComments = comments
    }

-- | We need to provide a source file name to reconstruct a SourceAnn.
sourceAnnFromExterns :: String -> ExternsSourceAnn -> SourceAnn
sourceAnnFromExterns spanName esa =
  ( SourceSpan
      { spanName = spanName
      , spanStart = esaStart esa
      , spanEnd = esaEnd esa
      }
  , esaComments esa
  )

-- | Check whether the version in an externs file matches the currently running
-- version.
externsIsCurrentVersion :: ExternsFile -> Bool
externsIsCurrentVersion ef =
  T.unpack (efVersion ef) == showVersion Paths.version

-- | Convert an externs file back into a module
applyExternsFileToEnvironment :: ExternsFile -> Environment -> Environment
applyExternsFileToEnvironment ef@ExternsFile{..} =
  flip (foldl' applyDecl) $ externsFileDeclarations ef
  where
  applyDecl :: Environment -> ExternsDeclaration SourceAnn -> Environment
  applyDecl env (EDType pn kind tyKind) = env { types = M.insert (qual pn) (kind, tyKind) (types env) }
  applyDecl env (EDRole pn roles) = env { roleDeclarations = M.insert (qual pn) roles (roleDeclarations env) }
  applyDecl env (EDTypeSynonym pn args ty) = env { typeSynonyms = M.insert (qual pn) (args, ty) (typeSynonyms env) }
  applyDecl env (EDDataConstructor pn dTy tNm ty nms) = env { dataConstructors = M.insert (qual pn) (dTy, tNm, ty, nms) (dataConstructors env) }
  applyDecl env (EDValue ident ty) = env { names = M.insert (Qualified (Just efModuleName) ident) (ty, External, Defined) (names env) }
  applyDecl env (EDClass pn args members cs deps tcIsEmpty) = env { typeClasses = M.insert (qual pn) (makeTypeClassData args members cs deps tcIsEmpty) (typeClasses env) }
  applyDecl env (EDInstance className ident vars kinds tys cs ch idx) =
    env { typeClassDictionaries =
            updateMap
              (updateMap (M.insertWith (<>) (qual ident) (pure dict)) className)
              (Just efModuleName) (typeClassDictionaries env) }
    where
    dict :: NamedDict
    dict = TypeClassDictionaryInScope ch idx (qual ident) [] className vars kinds tys cs

    updateMap :: (Ord k, Monoid a) => (a -> a) -> k -> M.Map k a -> M.Map k a
    updateMap f = M.alter (Just . f . fold)

  qual :: a -> Qualified a
  qual = Qualified (Just efModuleName)

-- | Generate an externs file for all declarations in a module
moduleToExternsFile :: Module -> Environment -> ExternsFile
moduleToExternsFile (Module _ _ _ _ Nothing) _ = internalError "moduleToExternsFile: module exports were not elaborated"
moduleToExternsFile (Module ss _ mn ds (Just exps)) env = ExternsFile{..}
  where
  efVersion       = T.pack (showVersion Paths.version)
  efModuleName    = mn
  efExports       = exps
  efImports       = mapMaybe importDecl ds
  efFixities      = mapMaybe fixityDecl ds
  efTypeFixities  = mapMaybe typeFixityDecl ds
  efDeclarations  = map toExterns $ concatMap toExternsDeclaration efExports ++ mapMaybe roleDecl ds
  efSourceSpan    = ss

  toExterns :: Functor f => f SourceAnn -> f ExternsSourceAnn
  toExterns = fmap sourceAnnToExterns

  fixityDecl :: Declaration -> Maybe ExternsFixity
  fixityDecl (ValueFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsFixity assoc prec op name)) (find (findOp getValueOpRef op) exps)
  fixityDecl _ = Nothing

  typeFixityDecl :: Declaration -> Maybe ExternsTypeFixity
  typeFixityDecl (TypeFixityDeclaration _ (Fixity assoc prec) name op) =
    fmap (const (ExternsTypeFixity assoc prec op name)) (find (findOp getTypeOpRef op) exps)
  typeFixityDecl _ = Nothing

  findOp :: (DeclarationRef -> Maybe (OpName a)) -> OpName a -> DeclarationRef -> Bool
  findOp g op = maybe False (== op) . g

  importDecl :: Declaration -> Maybe ExternsImport
  importDecl (ImportDeclaration _ m mt qmn) = Just (ExternsImport m mt qmn)
  importDecl _ = Nothing

  roleDecl :: Declaration -> Maybe (ExternsDeclaration SourceAnn)
  roleDecl (RoleDeclaration (RoleDeclarationData _ name roles)) = Just (EDRole name roles)
  roleDecl _ = Nothing

  toExternsDeclaration :: DeclarationRef -> [ExternsDeclaration SourceAnn]
  toExternsDeclaration (TypeRef _ pn dctors) =
    case Qualified (Just mn) pn `M.lookup` types env of
      Nothing -> internalError "toExternsDeclaration: no kind in toExternsDeclaration"
      Just (kind, TypeSynonym)
        | Just (args, synTy) <- Qualified (Just mn) pn `M.lookup` typeSynonyms env -> [ EDType pn kind TypeSynonym, EDTypeSynonym pn args synTy ]
      Just (kind, ExternData) -> [ EDType pn kind ExternData ]
      Just (kind, tk@(DataType _ tys)) ->
        EDType pn kind tk : [ EDDataConstructor dctor dty pn ty args
                            | dctor <- fromMaybe (map fst tys) dctors
                            , (dty, _, ty, args) <- maybeToList (Qualified (Just mn) dctor `M.lookup` dataConstructors env)
                            ]
      _ -> internalError "toExternsDeclaration: Invalid input"
  toExternsDeclaration (ValueRef _ ident)
    | Just (ty, _, _) <- Qualified (Just mn) ident `M.lookup` names env
    = [ EDValue ident ty ]
  toExternsDeclaration (TypeClassRef _ className)
    | let dictName = dictSynonymName . coerceProperName $ className
    , Just TypeClassData{..} <- Qualified (Just mn) className `M.lookup` typeClasses env
    , Just (kind, ExternData) <- Qualified (Just mn) (coerceProperName className) `M.lookup` types env
    , Just (synKind, TypeSynonym) <- Qualified (Just mn) dictName `M.lookup` types env
    , Just (synArgs, synTy) <- Qualified (Just mn) dictName `M.lookup` typeSynonyms env
    = [ EDType (coerceProperName className) kind ExternData
      , EDType dictName synKind TypeSynonym
      , EDTypeSynonym dictName synArgs synTy
      , EDClass className typeClassArguments typeClassMembers typeClassSuperclasses typeClassDependencies typeClassIsEmpty
      ]
  toExternsDeclaration (TypeInstanceRef _ ident)
    = [ EDInstance tcdClassName ident tcdForAll tcdInstanceKinds tcdInstanceTypes tcdDependencies tcdChain tcdIndex
      | m1 <- maybeToList (M.lookup (Just mn) (typeClassDictionaries env))
      , m2 <- M.elems m1
      , nel <- maybeToList (M.lookup (Qualified (Just mn) ident) m2)
      , TypeClassDictionaryInScope{..} <- NEL.toList nel
      ]
  toExternsDeclaration _ = []

-- | Gets all the declarations in an externs file together with expanded source
-- annotations.
externsFileDeclarations :: ExternsFile -> [ExternsDeclaration SourceAnn]
externsFileDeclarations ExternsFile{..} = map fromExterns efDeclarations
  where
  fromExterns :: Functor f => f ExternsSourceAnn -> f SourceAnn
  fromExterns = fmap (sourceAnnFromExterns (spanName efSourceSpan))

externsFileName :: FilePath
externsFileName = "externs.cbor"
