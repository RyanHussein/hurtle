module Hurtle.Types where

-- Standard Library Imports
import Text.Megaparsec ( Parsec )
import Data.Void ( Void )

{-
    This module defines the core types for the Hurtle programming language,
    used for controlling turtle graphics. It includes:

    - 'HogoCode': Data type for turtle commands (e.g., movement, pen control).
    - 'HogoProgram': List of 'HogoCode' instructions.
    - 'Parser': Type alias for Megaparsec parser.
-}

-- | A Hogo program is a list of HogoCode instructions.
type HogoProgram = [HogoCode]

data HogoCode
  -- | Movement Commands
  = GoForward Float
  | GoBackward Float
  | TurnLeft Float
  | TurnRight Float
  | GoHome
  | JumpTo Float Float -- X, Y coordinates
  -- | Pen Commands
  | PenUp
  | PenDown
  | ClearScreen
  -- | Control Flow
  | Repeat Int HogoProgram
  deriving (Show,Read,Eq)

-- | This is an alias for the Megaparsec parser type; the "Void" tells it that we don't have any custom error type, and the "string" tells it that we're parsing strings.
type Parser = Parsec Void String
