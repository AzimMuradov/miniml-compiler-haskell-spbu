module Configuration.AppConfiguration where

-- * App Configuration

data MiniMl = MiniMl Command Debug
  deriving (Show)

data Command
  = CmdCompile Compile
  | CmdRun Run
  | CmdVerify Verify
  deriving (Show)

data Debug
  = Yes
  | No
  deriving (Show, Eq)

-- ** Compile Configuration

data Compile = Compile Input CompilationTarget Output
  deriving (Show)

data CompilationTarget
  = CompileToLlvmIr
  | CompileToBinary
  deriving (Enum, Bounded)

instance Show CompilationTarget where
  show CompileToLlvmIr = "llvm-ir"
  show CompileToBinary = "binary"

data Output
  = FileOutput FilePath
  | AutoFileOutput
  deriving (Show)

-- ** Run Configuration

data Run = Run Input RunnerBackend
  deriving (Show)

data RunnerBackend
  = BackendLlvm
  | BackendInterpreter
  deriving (Enum, Bounded)

instance Show RunnerBackend where
  show BackendLlvm = "llvm"
  show BackendInterpreter = "interpreter"

-- ** Verify Configuration

newtype Verify = Verify Input
  deriving (Show)

-- ** Common Configuration

data Input
  = StdInput
  | FileInput FilePath
  deriving (Show)
