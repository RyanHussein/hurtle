module Main where

-- Standard Library Imports
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad (unless)

-- Third-Party Library Imports
import Graphics.Gloss
    ( black, white, animate, makeColorI, loadBMP,
      Display(InWindow), Color, Picture )
import Text.Megaparsec (parse, errorBundlePretty)

-- Project-Specific Imports
import Hurtle.Parser (parseHogo)
import Hurtle.Types (HogoCode(..))
import TurtleGraphics
    ( TurtleState(position, angle, penDown),
      initialTurtleState, drawTurtle, moveTurtle )

-- Data type for representing the state of the application.
data AppState = AppState {
    turtleState :: TurtleState,  -- Represents the turtle's current state.
    commands :: [HogoCode],      -- A list of commands for the turtle to execute.
    penColor :: Color,           -- Current pen colour for drawing.
    width :: Float               -- Width of the pen stroke.
}

{-
    The main function serves as the entry point for the application. It begins by loading .hogo files from the 'examples/passing' directory, 
    using the listHogoFiles function to specifically retrieve files with a .hogo extension. This initial step ensures that the application is 
    immediately ready for processing Hogo code files. Upon successfully listing available files, it checks to ensure the list is not empty. 
    If there are files present, it proceeds to display them along with indices, providing a user-friendly interface for file selection. 
    This enumeration of files simplifies the selection process for the user and transitions smoothly into the user interaction phase, which is 
    managed by the fileSelectionLoop.
-}
main :: IO ()
main = do
    putStrLn "\nLoading files from 'examples/passing'..."
    files <- listHogoFiles "examples/passing"

    unless (null files) $ do
        putStrLn "\nAvailable files:"
        -- Displaying files with indices for user selection.
        mapM_ putStrLn $ zipWith (\i f -> show (i :: Int) ++ ": " ++ f) [0..] files
        fileSelectionLoop files

{-
    The fileSelectionLoop function is designed to interactively handle user input for selecting a file from a list displayed by the main function. 
    It continuously prompts the user to select a file by entering the corresponding number or to quit the program by typing 'q'. This function enhances 
    usability by allowing users to either proceed with a valid selection or exit gracefully at any point. Upon receiving input, it evaluates the response: 
    if the user opts to quit ('q'), the program terminates successfully; for any other input, it attempts to parse the input as an integer. 
    A successful parse leads to the processing of the selected file through processFileSelection, whereas a failure (invalid input or a parse error) 
    triggers a recursive call to fileSelectionLoop, thereby allowing the user another attempt to make a valid selection.
-}
fileSelectionLoop :: [FilePath] -> IO ()
fileSelectionLoop files = do
    putStrLn "\nSelect a file by number (or 'q' to quit):"
    fileNumStr <- getLine
    case fileNumStr of
        "q" -> putStrLn "Exiting..." >> exitSuccess
        _ -> case readMaybe fileNumStr of
            Just i -> processFileSelection files i
            Nothing -> putStrLn "Invalid input. Please enter a number." >> fileSelectionLoop files

{-
    The processFileSelection function takes a list of file paths and an integer index as input, facilitating the processing of the user's file selection 
    based on the index provided. This function validates the user's choice, ensuring it falls within the valid range of the available files. If the index 
    is within the acceptable range, the function constructs the full path to the selected file and transitions to processing the file by calling proceedWithFile. 
    This next step involves additional user interactions for setting up drawing parameters and eventually parsing and executing the Hogo commands in the selected file. 
    Conversely, if the input index is outside the valid range, indicating an invalid selection, the function informs the user of the "Selection out of range" error 
    and redirects back to the fileSelectionLoop for another attempt.
-}
processFileSelection :: [FilePath] -> Int -> IO ()
processFileSelection files i
    | i >= 0 && i < length files = do
        let filePath = "examples/passing" </> files !! i
        putStrLn $ "You selected: " ++ filePath
        proceedWithFile filePath
    | otherwise = putStrLn "Selection out of range. Please try again." >> fileSelectionLoop files

{-
    The listHogoFiles function is designed to filter and list files with a '.hogo' extension within a specified directory, ensuring 
    that the application focuses solely on relevant Hogo code files. Upon receiving a directory path (dir) as input, it first retrieves
    a list of all files within that directory using listDirectory. It then applies a filter to this list, using takeExtension to extract 
    file extensions and compare them against the ".hogo" string. This process effectively removes any files that do not end with the .hogo 
    extension. The resulting list of files is then returned, providing a clean, targeted collection of files for further processing.
-}
listHogoFiles :: FilePath -> IO [FilePath]
listHogoFiles dir = do
    files <- listDirectory dir
    return $ filter ((== ".hogo") . takeExtension) files

{-
    The proceedWithFile function marks the transition from file selection to the actual processing of the selected file. It orchestrates the 
    collection of user inputs for customising the drawing parameters, specifically the pen colour and width, which is needed for the representation 
    of the Hogo commands. Upon invocation with the path of the selected file, it first prompts the user to input a pen colour through getPenColour, 
    and then a pen width through getPenWidth. These inputs directly influence the aesthetics of the drawing produced by the turtle graphics, allowing 
    users a degree of customisation. Additionally, it loads a BMP image of a turtle from the assets directory to visually represent the turtle in the
    graphical window. With these parameters set, the function calls processFile, passing along the file path, selected pen colour, pen width, and 
    the turtle image.
-}
proceedWithFile :: FilePath -> IO ()
proceedWithFile path = do
    penColour <- getPenColour
    width <- getPenWidth
    turtlePic <- loadBMP "assets/turtle.bmp"
    processFile path penColour width turtlePic

{-
    The getPenColour function interacts with the user to specify the pen colour for drawing operations in the application. It requests the user to 
    enter the colour components (red, green, blue, alpha) in a space-separated format without parentheses. This input is then passed to the parseColour 
    function. The design choice to allow users to input colour components directly offers a high degree of flexibility and customisation for the 
    drawing output. The function defaults to black if the parsing fails, ensuring that the application remains robust and error-tolerant by providing 
    a fallback colour.
-}
getPenColour :: IO Color
getPenColour = do
    putStrLn "\nEnter pen colour (r g b a) without parentheses, space-separated:"
    parseColour <$> getLine

{-
    The getPenWidth function is tasked with collecting user input for the width of the pen used in drawing operations. This function prompts the user 
    to enter a numerical value representing the pen's width. Utilising readMaybe, it attempts to parse the input into a Float. If the parsing is successful, 
    the parsed value is used as the pen width; otherwise, the function defaults to a width of 1. This defaulting behavior ensures the application's resilience 
    to invalid inputs, maintaining a smooth user experience by preventing errors from halting or crashing the drawing process.
-}
getPenWidth :: IO Float
getPenWidth = do
    putStrLn "\nEnter pen width:"
    fromMaybe 1 . readMaybe <$> getLine

{-
    The processFile function reads a file's content, parses it to a list of Hogo commands, and then animates these commands. It starts by reading the file from 
    the given path and tries to parse this content. If the parsing fails, it shows an error and stops the program. If it succeeds, it takes the list of commands, 
    expands any repeat commands, and prepares them for animation. It then uses these commands to animate the turtle's movement in a window, showing how the turtle 
    would draw the commands in the file. The function uses the pen colour and width chosen by the user and a turtle picture for the animation.
-}
processFile :: FilePath -> Color -> Float -> Picture -> IO ()
processFile path penColor width turtlePic = do
    content <- readFile path
    case parse parseHogo path content of
        Left e -> putStrLn (errorBundlePretty e) >> exitWith (ExitFailure 1)
        Right codes -> let expandedCodes = expandCommands codes in
                       animate (InWindow "Hurtle" (800, 600) (10, 10)) white
                       (animationFunc (AppState initialTurtleState expandedCodes penColor width) turtlePic)

{-
    The parseColour function converts a user-input string into a Color value. It expects the input to be in the format of 
    four space-separated numbers (representing the red, green, blue, and alpha channels, respectively) and tries to parse these 
    numbers from the string. If successful, it uses makeColorI to create a Color value from these integer components, allowing for 
    precise colour specification. However, if the input doesn't match the expected format or contains invalid numbers 
    (anything that can't be read as an integer), the function defaults to black.
-}
parseColour :: String -> Color
parseColour input = case mapM readMaybe (words input) of
    Just [r, g, b, a] -> makeColorI r g b a
    _ -> black -- Defaults to black if parsing fails.

{-
    The expandCommands function is responsible for transforming Repeat commands within a list of HogoCode instructions into their expanded form. 
    This is achieved by recursively unfolding Repeat commands into their specified number of repetitions, thereby converting a potentially nested 
    and compact set of instructions into a flat, sequential list of commands ready for execution. For each Repeat command encountered, it replicates 
    its subcommands n times (where n is the repetition count) and applies expandCommands to these subcommands to ensure that nested Repeats are also expanded. 
    Non-Repeat commands are left unchanged. This approach simplifies the animation of Repeat commands. Instead of the turtle jumping to the last point of the
    repeat command, it is able to show each step.
-}
expandCommands :: [HogoCode] -> [HogoCode]
expandCommands = concatMap expand
  where
    expand (Repeat n subCmds) = concat $ replicate n (expandCommands subCmds)
    expand cmd = [cmd]

{-
    The animationFunc function is designed to animate the turtle's drawing based on the current application state, which includes the list of Hogo commands 
    to execute, the pen colour and the pen width. This function is called repeatedly by the graphics library with increasing time values, allowing it to animate 
    the drawing process over time. It calculates how many commands to execute at each time step based on the total number of commands and a speed adjustment factor, 
    ensuring that the animation's pace is reasonable and scalable with the command count. Commands are applied sequentially to update the turtle's state, which 
    includes its position, direction, and whether the pen is up or down. The updated state is then used to draw the current frame of the animation, including 
    the path drawn by the turtle so far and the turtle's current position, represented by the turtle picture. This approach provides a dynamic visual representation 
    of the turtle executing the Hogo commands.
-}
animationFunc :: AppState -> Picture -> Float -> Picture
animationFunc appState turtlePic time =
    let totalCommands = length (commands appState)
        speedAdjustment = max 2 (fromIntegral (totalCommands `div` 20)) -- Adjust speed of turtle depending on number of commands.
        steps = take (floor (time * speedAdjustment)) (commands appState)
        finalState = foldl applyHogoCode (turtleState appState) steps
    in drawTurtle (width appState) (penColor appState) finalState turtlePic

{-
    The applyHogoCode function updates the turtle's state based on a single HogoCode command. It interprets the command, whether it's moving forward or backward, 
    turning, changing the pen state, clearing the screen, returning to the origin, repeating a set of commands, or jumping to a specific location. 
    The function adjusts the turtle's position, direction, and pen status accordingly. For movement commands, it uses moveTurtle to calculate the new position. 
    For turning, it adjusts the angle directly. Pen up and down commands simply toggle the pen's drawing state. The ClearScreen command resets the turtle to its 
    initial state, and GoHome moves the turtle back to the origin without affecting its orientation or pen state. The Repeat command is handled by recursively 
    applying the set of commands the specified number of times, ensuring that complex patterns can be executed efficiently. JumpTo changes the turtle's position 
    without drawing. This function encapsulates the core logic for executing drawing commands, making the turtle's behavior directly correspond to the Hogo language 
    specifications.
-}
applyHogoCode :: TurtleState -> HogoCode -> TurtleState
applyHogoCode state cmd = case cmd of
    GoForward d -> moveTurtle d state
    GoBackward d -> moveTurtle (-d) state
    TurnLeft d -> state { angle = angle state + d }
    TurnRight d -> state { angle = angle state - d }
    PenUp -> state { penDown = False }
    PenDown -> state { penDown = True }
    ClearScreen -> initialTurtleState
    GoHome -> state { position = (0, 0) }
    Repeat n codes -> foldl applyHogoCode state (concat $ replicate n codes)
    JumpTo x y -> state { position = (x, y) }
