module Records where

data Type = TInt | TDouble | TString | TText | TLText | TUnit deriving (Show, Ord, Eq)
data RType = RPure Type | RIO Type deriving (Show, Eq)

data Template = Template {
	modul :: String,
	slots :: [Slot],
	signalTypes :: [SignalType],
	signals :: [Signal]
} deriving (Show)

data Slot = Slot {
	name :: String,
	monadic :: String,
	args :: [SlotArg],
	crtype :: String,
	crwrap :: String
} deriving (Show)

data SlotArg = SlotArg {
	aname :: String,
	ctype :: String,
	cwrap :: String
} deriving (Show)

data SignalType = SignalType {
	csigtype :: String
} deriving (Show)

data Signal = Signal {
	signame :: String,
	sigargs :: [SignalArg],
	sigevent :: Integer,
	sigcwrap :: String
} deriving (Show)

data SignalArg = SignalArg {
	siganame :: String
} deriving (Show)
