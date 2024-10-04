module Configuration.AppConfiguration where

-- * App Configuration

data MiniMl = MiniMl Command Debug
  deriving (Show)

data Command
  = CmdRun Run
  | CmdCompile Compile
  | CmdPrintCRuntime PrintCRuntime
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
  | TargetRiscVAsm
  deriving (Enum, Bounded)

instance Show CompilationTarget where
  show TargetBinary = "binary"
  show TargetLlvmIr = "llvm-ir"
  show TargetRiscVAsm = "risc-v-asm"

-- ** Print C Runtime Configuration

newtype PrintCRuntime = PrintCRuntime Output
  deriving (Show)

-- ** Common Configuration

data Input
  = StdInput
  | FileInput FilePath
  deriving (Show)

data Output
  = FileOutput FilePath
  | AutoFileOutput
  deriving (Show)
