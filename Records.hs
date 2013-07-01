module Records where

data Template = Template {
	headerPath :: String,
	modul :: String,
	toUItypes :: [SignalType],
	toUI :: [Signal],
	fromUI :: [Signal]
} deriving (Show)

data SignalType = SignalType {
	csigtype :: String,
	qtsigtype :: String,
	qtwrapsig :: String,
	lowcsigtype :: String,
	sigtypename :: String
} deriving (Show)

data Signal = Signal {
	sigfirst :: Bool,
	signame :: String,
	sigargs :: [SignalArg],
	sigevent :: Integer,
	sigcwrap :: String
} deriving (Show)

data SignalArg = SignalArg {
	sigargfirst :: Bool,
	siganame :: String,
	qtsigargtype :: String,
	csigargtype :: String,
	sigargfromc :: String,
	sigargsigtypename :: String
} deriving (Show)
