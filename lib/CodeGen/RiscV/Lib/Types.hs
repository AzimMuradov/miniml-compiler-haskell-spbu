{-# LANGUAGE PatternSynonyms #-}

module CodeGen.RiscV.Lib.Types where

import Control.Arrow ((>>>))
import Data.Char (toLower)
import Data.Int (Int64)
import Data.Text (Text)
import Prettyprinter (Pretty (pretty), colon, comma, hsep, indent, layoutCompact, parens, punctuate, vsep, (<+>))
import Prettyprinter.Render.Text (renderStrict)

ppCodeLines :: [CodeLine] -> Text
ppCodeLines =
  map pretty
    >>> vsep
    >>> layoutCompact
    >>> renderStrict

data CodeLine
  = LabeledDirectiveCodeLine Label Directive
  | LabelCodeLine Label
  | InstructionCodeLine Instruction
  | DirectiveCodeLine Directive

instance Pretty CodeLine where
  pretty (LabeledDirectiveCodeLine l d) = pretty l <+> pretty d
  pretty (LabelCodeLine l) = pretty l
  pretty (InstructionCodeLine i) = indent 4 (pretty i)
  pretty (DirectiveCodeLine d) = pretty d

newtype Label = Label Text

instance Pretty Label where
  pretty (Label txt) = pretty txt <> colon

data Instruction = Instruction OpCode [Operand]

instance Pretty Instruction where
  pretty (Instruction opCode args) = hsep $ pretty opCode : punctuate comma (pretty <$> args)

data Directive
  = DirText
  | DirData
  | DirDWord Int64
  | DirGlobl Text

instance Pretty Directive where
  pretty DirText = pretty ".section .text"
  pretty DirData = pretty ".section .data"
  pretty (DirDWord initVal) = pretty ".dword" <+> pretty initVal
  pretty (DirGlobl name) = pretty ".globl" <+> pretty name

data OpCode
  = And
  | Or
  | Add
  | Sub
  | Mul
  | Neg
  | Seqz
  | Snez
  | Slt
  | Sd
  | Ld
  | Li
  | La
  | Addi
  | Beqz
  | J
  | Call
  | Ret
  deriving (Show)

instance Pretty OpCode where
  pretty opCode = pretty $ toLower <$> show opCode

data Operand
  = Immediate Int64
  | Register Register
  | RegisterWithOffset Register Offset
  | Symbol Text
  deriving (Eq, Ord)

pattern Memory :: Offset -> Operand
pattern Memory offset = RegisterWithOffset Sp offset

instance Pretty Operand where
  pretty (Immediate i) = pretty i
  pretty (Register r) = pretty r
  pretty (RegisterWithOffset r o) = pretty (dword * o) <> parens (pretty r)
  pretty (Symbol t) = pretty t

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
  deriving (Eq, Ord, Show)

instance Pretty Register where
  pretty opCode = pretty $ toLower <$> show opCode

dword :: Int64
dword = 8
