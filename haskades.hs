module Main (main) where

import Data.List
import Control.Error
import Control.Arrow
import Control.Applicative
import System.Environment (getArgs)
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map as M

import Records
import MustacheTemplates

data Type = TInt | TUTCTime | TDouble | TString | TText | TLText deriving (Show, Ord, Eq)
data RType = RPure Type | RIOUnit | RIO Type deriving (Show, Eq)

importPrefix :: HsImportDecl -> String
importPrefix (HsImportDecl {importQualified = True, importAs = Just (Module m)}) = m
importPrefix (HsImportDecl {importQualified = True, importAs = Nothing, importModule = (Module m)}) = m
importPrefix _ = ""

doParse :: String -> (String, [(String, Module)], [HsDecl])
doParse moduleSrc = (mod, importEnv, decls)
	where
	importEnv = map (importPrefix &&& importModule) imports
	ParseOk (HsModule _ (Module mod) _ imports decls) = parseModule moduleSrc

dataDeclNamed :: HsName -> HsDecl -> Bool
dataDeclNamed n (HsDataDecl _ _ name _ _ _) | n == name = True
dataDeclNamed _ _ = False

extractTypeFromBangType :: HsBangType -> HsType
extractTypeFromBangType (HsBangedTy t) = t
extractTypeFromBangType (HsUnBangedTy t) = t

flatTyFun :: HsType -> HsType -> ([HsType], HsType)
flatTyFun a (HsTyFun b c) = first (a:) (flatTyFun b c)
flatTyFun a b = ([a], b)

findType :: [(String, Module)] -> Module -> [(Module, Type)] -> Either String Type
findType i (Module m) q =
	headErr ("No " ++ show q ++ " imports find, but one used as type.") matches
	where
	matches = mapMaybe (`lookup` q) $ filter (`elem` moduls) ns
	moduls = map fst q
	ns = map snd $ filter ((==m).fst) i

findTextType :: [(String, Module)] -> Module -> Either String Type
findTextType i m = findType i m [
		(Module "Data.Text", TText),
		(Module "Data.Text.Lazy", TLText)
	]

findUTCTimeType :: [(String, Module)] -> Module -> Either String Type
findUTCTimeType i m = findType i m [
		(Module "Data.Time", TUTCTime),
		(Module "Data.Time.Clock", TUTCTime)
	]

resolveType :: [(String, Module)] -> HsType -> Either String Type
resolveType _ (HsTyCon (UnQual (HsIdent "Int"))) = Right TInt
resolveType _ (HsTyCon (UnQual (HsIdent "Double"))) = Right TDouble
resolveType _ (HsTyCon (UnQual (HsIdent "String"))) = Right TString
resolveType i (HsTyCon (UnQual (HsIdent "Text"))) = findTextType i (Module "")
resolveType i (HsTyCon (Qual m (HsIdent "Text"))) = findTextType i m
resolveType i (HsTyCon (UnQual (HsIdent "UTCTime"))) = findUTCTimeType i (Module "")
resolveType i (HsTyCon (Qual m (HsIdent "UTCTime"))) = findUTCTimeType i m
resolveType _ t = Left $ "Type not yet supported: " ++ show t

resolveRType :: [(String, Module)] -> HsType -> Either String RType
resolveRType _ (HsTyCon (Special HsUnitCon)) = Left "A pure function that returns () ?"
resolveRType _ (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) (HsTyCon (Special HsUnitCon))) = return RIOUnit
resolveRType i (HsTyApp (HsTyCon (UnQual (HsIdent "IO"))) t) = RIO <$> resolveType i t
resolveRType i t = RPure <$> resolveType i t

getSlots :: [(String, Module)] -> [HsDecl] -> Either String [(String, [Type], RType)]
getSlots i decls = slotsDecl >>= slotsFrom >>= mapM slotField
	where
	slotField ([HsIdent fname], t) = case extractTypeFromBangType t of
		(HsTyFun a b) -> let (args, r) = flatTyFun a b in
			(,,) <$> pure fname <*> mapM (resolveType i) args <*> resolveRType i r
		x -> (,,) <$> pure fname <*> pure [] <*> resolveRType i x
	slotField (x, _) = Left $ "Expected field name, found: " ++ show x
	slotsFrom (HsDataDecl _ _ _ _ [HsRecDecl _ (HsIdent "Slots") fields] _) = Right fields
	slotsFrom _ = Left "Type \"Slots\" must have a single record constructor named \"Slots\"."
	slotsDecl = note "No record named \"Slots\" found." $
		find (dataDeclNamed (HsIdent "Slots")) decls

flattenSum :: HsConDecl -> Either String (String, [HsType])
flattenSum (HsConDecl _ (HsIdent n) ts) = Right (n, map extractTypeFromBangType ts)
flattenSum (HsRecDecl _ (HsIdent n) ts) = Right (n, map (extractTypeFromBangType.snd) ts)
flattenSum x = Left $ "Cannot process sum type constructor: " ++ show x

getSignals :: [(String, Module)] -> [HsDecl] -> Either String [(String, [Type])]
getSignals i decls = signalDecl >>= signalsFrom >>= mapM signalConstr
	where
	signalConstr (n, ts) = (,) <$> pure n <*> mapM (resolveType i) ts
	signalsFrom (HsDataDecl _ _ _ _ cons _) = mapM flattenSum cons
	signalsFrom x = Left $ "Could not process signal near: " ++ show x
	signalDecl = note "No sum type named \"Signal\" found." $
		find (dataDeclNamed (HsIdent "Signal")) decls

mapQtType :: Type -> String
mapQtType TInt = "int"
mapQtType TUTCTime = "QDateTime"
mapQtType TDouble = "double"
mapQtType TString = "QString"
mapQtType TText = "QString"
mapQtType TLText = "QString"

mapLowCType :: Type -> String
mapLowCType TInt = "int"
mapLowCType TUTCTime = "uint" -- Not time_t because of Qt
mapLowCType TDouble = "double"
mapLowCType TString = "const char *"
mapLowCType TText = "const char *"
mapLowCType TLText = "const char *"

mapCType :: Type -> String
mapCType TInt = "CInt"
mapCType TUTCTime = "CUInt" -- Not CTime because of Qt
mapCType TDouble = "CDouble"
mapCType TString = "CString"
mapCType TText = "CString"
mapCType TLText = "CString"

-- We always live in IO on the C side
mapCRType :: RType -> String
mapCRType RIOUnit = "IO ()"
mapCRType (RPure t) = "IO " ++ mapCType t
mapCRType (RIO t) = "IO " ++ mapCType t

wrapTypeFromC :: Type -> String -> String
wrapTypeFromC TInt arg = "(return $ fromIntegral " ++ arg ++ ")"
wrapTypeFromC TUTCTime arg = "(return $ posixSecondsToUTCTime $ realToFrac " ++ arg ++ ")"
wrapTypeFromC TDouble arg = "(return $ realToFrac " ++ arg ++ ")"
wrapTypeFromC TString arg = "(fmap (Text.unpack . Text.decodeUtf8) (ByteString.packCString " ++ arg ++ "))"
wrapTypeFromC TText arg = "(fmap Text.decodeUtf8 (ByteString.packCString " ++ arg ++ "))"
wrapTypeFromC TLText arg = "(fmap (LText.fromStrict . Text.decodeUtf8) (ByteString.packCString " ++ arg ++ "))"
--wrapTypeFromC _ arg = "(return " ++ arg ++ ")"

wrapTypeToC :: Type -> String -> String
wrapTypeToC TInt arg = "(flip ($) (fromIntegral " ++ arg ++ "))"
wrapTypeToC TUTCTime arg = "(flip ($) (fromInteger $ floor $ utcTimeToPOSIXSeconds " ++ arg ++ "))"
wrapTypeToC TDouble arg = "(flip ($) (realToFrac " ++ arg ++ "))"
wrapTypeToC TString arg = "(ByteString.useAsCString (Text.encodeUtf8 $ Text.pack " ++ arg ++ "))"
wrapTypeToC TText arg = "(ByteString.useAsCString (Text.encodeUtf8 " ++ arg ++ "))"
wrapTypeToC TLText arg = "(ByteString.useAsCString (Text.encodeUtf8 $ LText.toStrict " ++ arg ++ "))"

wrapTypeFromQt :: Type -> String -> String
wrapTypeFromQt TUTCTime arg = "(" ++ arg ++ ").toTime_t()"
wrapTypeFromQt TString arg = "(" ++ arg ++ ").toUtf8().constData()"
wrapTypeFromQt TText arg = "(" ++ arg ++ ").toUtf8().constData()"
wrapTypeFromQt TLText arg = "(" ++ arg ++ ").toUtf8().constData()"
wrapTypeFromQt _ arg = "(" ++ arg ++ ")"

wrapTypeToQt :: Type -> String -> String
wrapTypeToQt TUTCTime arg = "(QDateTime::fromTime_t(" ++ arg ++ "))"
wrapTypeToQt TString arg = "(QString::fromUtf8(" ++ arg ++ "))"
wrapTypeToQt TText arg = "(QString::fromUtf8(" ++ arg ++ "))"
wrapTypeToQt TLText arg = "(QString::fromUtf8(" ++ arg ++ "))"
wrapTypeToQt _ arg = "(" ++ arg ++ ")"

defTypeToC :: Type -> String
defTypeToC TInt = "(flip ($) 0)"
defTypeToC TUTCTime = "(flip ($) 0)"
defTypeToC TDouble = "(flip ($) 0)"
defTypeToC TString = "(flip ($) nullPtr)"
defTypeToC TText = "(flip ($) nullPtr)"
defTypeToC TLText = "(flip ($) nullPtr)"

templateSlot :: (String, [Type], RType) -> Slot
templateSlot (fname, args, rtype) = Slot {
		name = fname,
		monadic = case rtype of
			RPure _ -> "(return " ++ fname ++ ")"
			RIOUnit -> "(" ++ fname ++ ")"
			RIO   _ -> "(" ++ fname ++ ")",
		hasArgs = not (null args),
		args = zipWith (\i a -> SlotArg {
				firstarg = i == 0,
				aname = "arg" ++ show i,
				ctype = mapCType a,
				lowctype = mapLowCType a,
				qttype = mapQtType a,
				cwrap = wrapTypeFromC a ("arg" ++ show i),
				qtunwrap = wrapTypeFromQt a ("arg" ++ show i)
			}) [(0::Int)..] args,
		crtype = mapCRType rtype,
		crwrap = "(return)" -- TODO
	}

sigCWrap' :: [(Int, Type)] -> [(Int, Type)] -> ([(Int, Int)], [String])
sigCWrap' [] _ = ([], [])
sigCWrap' ((ei,e):es) ts
	| null sameType = -- No more of this type in this signal
		second (("(.) " ++ defTypeToC e) :) (sigCWrap' es ts)
	| otherwise = -- Use up one of this type
		let (idxs, r) = sigCWrap' es (tail sameType ++ rest) in
		((i, ei) : idxs, ("(.) " ++ wrapTypeToC e ("arg" ++ show i)) : r)
	where
	(i, _) = head sameType
	(sameType, rest) = partition ((==e).snd) ts

sigCWrap :: [Type] -> [(Int, Type)] -> ([(Int, Int)], String)
sigCWrap es ts = (,) idxs $ case wraps of
		[] -> ""
		[x] -> drop 4 x
		(x:xs) -> intercalate " $ (.) $ " (reverse xs) ++ " $ " ++ x
	where
	(idxs, wraps) = second reverse $ sigCWrap' (zip [0..] es) ts

templateSignal :: [Type] -> Integer -> (String, [Type]) -> Signal
templateSignal signalTypes i (name, ts) = Signal {
	signame = name,
	sigargs = zipWith (\i t -> SignalArg {
			sigargfirst = i == 0,
			siganame = "arg" ++ show i,
			qtsigargtype = mapQtType t,
			sigargsigtypename = let Just x = lookup i idxs in "arg" ++ show x
		}) [0..] ts,
	sigevent = i,
	sigcwrap = cwrap
}
	where
	(idxs, cwrap) = sigCWrap signalTypes $ zip [0..] ts

parseArgs :: [String] -> Either String [String]
parseArgs [a,b] = return [a,b]
parseArgs _ =
	Left "Usage: haskades HaskadesBinding.hs haskades_run.cpp < Types.hs"

main :: IO ()
main = runScript $ do
	[hsout, cppout] <- hoistEither . parseArgs =<< scriptIO getArgs

	(mod, importEnv, decls) <- fmap doParse (scriptIO getContents)
	slots <- hoistEither $ getSlots importEnv decls
	signals <- hoistEither $ getSignals importEnv decls
	let signalTypes = concatMap (\(t,c) -> replicate c t) $ M.toAscList $
		M.unionsWith max $ map (M.fromListWith (+) . (`zip` [1,1..]) . snd) signals
	let sigs = zipWith (templateSignal signalTypes) [1..] signals
	let sigTypes = zipWith (\i t -> SignalType {
			csigtype = mapCType t,
			qtsigtype = mapQtType t,
			qtwrapsig = wrapTypeToQt t ("arg" ++ show i),
			lowcsigtype = mapLowCType t,
			sigtypename = "arg" ++ show i
		}) [(0::Int)..] signalTypes
	let template = Template {
			modul = mod,
			slots = map templateSlot slots,
			signalTypes = sigTypes,
			signals = sigs
		}

	scriptIO $ TL.writeFile hsout $ toLazyText $ haskadesBindinghs id template
	scriptIO $ TL.writeFile cppout $ toLazyText $ haskades_runcpp id template
