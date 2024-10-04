module CodeGen.RiscV.Lib.Types where

import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Txt

data CodeLine
  = LabeledDirectiveCodeLine Label Directive
  | LabelCodeLine Label
  | InstructionCodeLine Instruction
  | DirectiveCodeLine Directive

instance Show CodeLine where
  show (LabeledDirectiveCodeLine l d) = spaceSep [show l, show d]
  show (LabelCodeLine l) = show l
  show (InstructionCodeLine i) = "    " ++ show i
  show (DirectiveCodeLine d) = show d

newtype Label = Label Text

instance Show Label where
  show (Label txt) = Txt.unpack txt ++ ":"

data Instruction = Instruction OpCode [Operand]

instance Show Instruction where
  show (Instruction opCode args) = spaceSep [show opCode, commaSep $ show <$> args]

data Directive
  = DirText
  | DirData
  | DirDWord Int64
  | DirGlobl Text

instance Show Directive where
  show DirText = ".section .text"
  show DirData = ".section .data"
  show (DirDWord initVal) = spaceSep [".dword", show initVal]
  show (DirGlobl name) = spaceSep [".globl", Txt.unpack name]

data OpCode
  = Add
  | Sub
  | Mul
  | And
  | Or
  | Not
  | Seqz
  | Snez
  | Slt
  | Ld
  | Sd
  | Addi
  | Li
  | La
  | Neg
  | Ret
  | Jal
  | Beqz
  | J

instance Show OpCode where
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show And = "and"
  show Or = "or"
  show Not = "not"
  show Seqz = "seqz"
  show Snez = "snez"
  show Slt = "slt"
  show Ld = "ld"
  show Sd = "sd"
  show Addi = "addi"
  show Li = "li"
  show La = "la"
  show Neg = "neg"
  show Ret = "ret"
  show Jal = "jal"
  show Beqz = "beqz"
  show J = "j"

data Operand
  = Immediate Int64
  | Register Register
  | Memory Offset
  | RegisterWithOffset Register Offset
  | Symbol Text
  deriving (Eq, Ord)

instance Show Operand where
  show (Immediate i) = show i
  show (Register r) = show r
  show (Memory o) = show (dword * o) ++ "(sp)"
  show (RegisterWithOffset r o) = show (dword * o) ++ "(" ++ show r ++ ")"
  show (Symbol t) = Txt.unpack t

type Offset = Int64

data Register
  = Zero -- zero == x0
  | Ra -- ra == x1
  | Sp -- sp == x2
  | Gp -- gp == x3
  | Tp -- tp == x4
  | T0 -- t0 == x5
  | T1 -- t1 == x6
  | T2 -- t2 == x7
  | S0 -- s0 == x8
  | S1 -- s1 == x9
  | A0 -- a0 == x10
  | A1 -- a1 == x11
  | A2 -- a2 == x12
  | A3 -- a3 == x13
  | A4 -- a4 == x14
  | A5 -- a5 == x15
  | A6 -- a6 == x16
  | A7 -- a7 == x17
  | S2 -- s2 == x18
  | S3 -- s3 == x19
  | S4 -- s4 == x20
  | S5 -- s5 == x21
  | S6 -- s6 == x22
  | S7 -- s7 == x23
  | S8 -- s8 == x24
  | S9 -- s9 == x25
  | S10 -- s10 == x26
  | S11 -- s11 == x27
  | T3 -- t3 == x28
  | T4 -- t4 == x29
  | T5 -- t5 == x30
  | T6 -- t6 == x31
  deriving (Eq, Ord)

instance Show Register where
  show Zero = "zero"
  show Ra = "ra"
  show Sp = "sp"
  show Gp = "gp"
  show Tp = "tp"
  show T0 = "t0"
  show T1 = "t1"
  show T2 = "t2"
  show S0 = "s0"
  show S1 = "s1"
  show A0 = "a0"
  show A1 = "a1"
  show A2 = "a2"
  show A3 = "a3"
  show A4 = "a4"
  show A5 = "a5"
  show A6 = "a6"
  show A7 = "a7"
  show S2 = "s2"
  show S3 = "s3"
  show S4 = "s4"
  show S5 = "s5"
  show S6 = "s6"
  show S7 = "s7"
  show S8 = "s8"
  show S9 = "s9"
  show S10 = "s10"
  show S11 = "s11"
  show T3 = "t3"
  show T4 = "t4"
  show T5 = "t5"
  show T6 = "t6"

dword :: Int64
dword = 8

commaSep :: [String] -> String
commaSep = intercalate ", " . filterOutBlankStrings

spaceSep :: [String] -> String
spaceSep = unwords . filterOutBlankStrings

filterOutBlankStrings :: [String] -> [String]
filterOutBlankStrings = filter (not . all isSpace)
