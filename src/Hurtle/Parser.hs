module Hurtle.Parser where

-- Project-Specific Imports
import Hurtle.Types (HogoCode(..), Parser)

-- Third-Party Library Imports
import Text.Megaparsec
    ( (<|>), empty, MonadParsec(eof, try),
      between, choice, many )
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

{-
    Utility Functions: This section includes essential helper functions that streamline the parsing process. The spaceConsumer function is designed to 
    clear away whitespace and comments, ensuring that these do not interfere with the parsing of meaningful syntax elements. The symbol function simplifies the 
    parsing of specific symbols or keywords by automatically consuming any leading spaces, making the parser less sensitive to formatting discrepancies in the input. 
    The number function robustly parses numerical values, supporting both integers and floating-point numbers, including those with negative signs.
-}

-- Consumes whitespace and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

-- Parses symbols, skipping any preceding space.
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Parses numbers, which can be either floats or integers, including negative values.
number :: Parser Float
number = L.signed spaceConsumer (try L.float <|> fmap (fromIntegral :: Integer -> Float) L.decimal)

{-
    Main Parsing Function: The parseHogo function serves as the core of the Hurtle language parser. It's designed to process an entire input file or text stream 
    from start to end (EOF), transforming it into a structured list of HogoCode commands. The function works by applying the spaceConsumer to ignore irrelevant 
    whitespace and comments, ensuring that the parsing focuses on actual language commands. It then repeatedly applies parseStatement to the input, accumulating 
    a list of HogoCode commands that represent the parsed Hurtle program.
-}

-- Parses the entire input until EOF, producing a list of 'HogoCode' commands.
parseHogo :: Parser [HogoCode]
parseHogo = between spaceConsumer eof (many parseStatement)

{-
    Command Specific Parsers: This section comprises a collection of parsers, each tailored to recognise and parse a specific command in the Hurtle language. 
    These commands encompass basic movement instructions like GoForward and GoBackward, rotational commands such as TurnLeft and TurnRight, pen control commands 
    including PenUp and PenDown, and environment control commands like ClearScreen and GoHome. Additionally, there are commands for repeating a set of instructions 
    (Repeat) and for moving the turtle to a specific position without drawing (JumpTo). Each parser function utilises the symbol utility to identify the command's 
    keyword in the input and then applies appropriate parsing logic to extract any necessary parameters (like distances, angles, or repetition counts) from the text.
-}

-- Parses the GoForward command, which makes the turtle go forward.
goForward :: Parser HogoCode
goForward = GoForward <$> (symbol "forward" *> number)

-- Parses the GoBackward command, which makes the turtle go backwards.
goBackward :: Parser HogoCode
goBackward = GoBackward <$> (symbol "back" *> number)

-- Parses the TurnLeft command, which increases the turtles relative angle.
turnLeft :: Parser HogoCode
turnLeft = TurnLeft <$> (symbol "left" *> number)

-- Parses the TurnRight command, which decreases the turtles relative angle.
turnRight :: Parser HogoCode
turnRight = TurnRight <$> (symbol "right" *> number)

-- Parses the PenUp command, which disables drawing.
penUp :: Parser HogoCode
penUp = PenUp <$ symbol "penup"

-- Parses the PenDown command, which enables drawing.
penDown :: Parser HogoCode
penDown = PenDown <$ symbol "pendown"

-- Parses the ClearScreen command for resetting the drawing surface.
clear :: Parser HogoCode
clear = ClearScreen <$ symbol "clearscreen"

-- Parses the GoHome command to reset the position of the pen.
goHome :: Parser HogoCode
goHome = GoHome <$ symbol "home"

-- Parses the Repeat command, which includes a block of commands to repeat.
repeatCmd :: Parser HogoCode
repeatCmd = do
    _ <- symbol "repeat"
    n <- L.decimal
    _ <- spaceConsumer
    _ <- symbol "["
    cmds <- many parseStatement
    _ <- symbol "]"
    return $ Repeat n cmds

-- Parses the JumpTo command, which moves the turtle to a specific position without drawing, regardless of the pen state.
jumpTo :: Parser HogoCode
jumpTo = JumpTo <$> (symbol "jumpto" *> number) <*> (spaceConsumer *> number)

{-
    The parseStatement function acts as the integration point for all command-specific parsers within the Hurtle language parser framework. 
    By leveraging Megaparsec's choice function, it attempts to match the input against each available command parser in sequence, effectively parsing any 
    recognised command into its corresponding HogoCode representation. This approach ensures that the parser is comprehensive and capable of understanding any 
    command defined in the Hurtle language. The use of <* spaceConsumer at the end signifies that after successfully parsing a command, any trailing whitespace 
    is ignored, preparing the parser for the next command. This mechanism enables the parser to seamlessly navigate and interpret a continuous stream of commands 
    from the input, facilitating the translation of raw text into structured, executable code.
-}

-- Combines the individual parsers into a single parser that tries each until one succeeds.
parseStatement :: Parser HogoCode
parseStatement = choice [goForward, goBackward, turnLeft, turnRight, penUp, penDown, clear, repeatCmd, goHome, jumpTo] <* spaceConsumer
