module Configuration.AppConfiguration where

-- * App Configuration

data MiniMl = MiniMl Command Debug
  deriving (Show)

data Command
  = CmdRun Run
  | CmdCompile Compile
  deriving (Show)

data Debug
  = Yes
  | No
  deriving (Show, Eq)

-- ** Run Configuration

newtype Run = Run Input
  deriving (Show)

-- ** Compile Configuration

data Compile = Compile Input CompilationTarget Output
  deriving (Show)

data CompilationTarget
  = TargetBinary
  | TargetLlvmIr
  deriving (Enum, Bounded)

instance Show CompilationTarget where
  show TargetBinary = "binary"
  show TargetLlvmIr = "llvm-ir"

data Output
  = FileOutput FilePath
  | AutoFileOutput
  deriving (Show)

-- ** Common Configuration

data Input
  = StdInput
  | FileInput FilePath
  deriving (Show)
