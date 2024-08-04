module TurtleGraphics where

-- Third-Party Library Imports
import Graphics.Gloss
    ( color,
      pictures,
      polygon,
      rotate,
      scale,
      translate,
      Color,
      Picture )

-- Data type for representing the state of the turtle.
data TurtleState = TurtleState {
    position :: (Float, Float),                            -- The current position (x, y) of the turtle.
    angle :: Float,                                        -- The current angle of the turtle in degrees relative to the positive x-axis.
    penDown :: Bool,                                       -- Whether the pen is down (True) and drawing or up (False) and not drawing.
    linesDrawnSoFar :: [((Float, Float), (Float, Float))]  -- A list of line segments drawn by the turtle, start and end points.
}

{-
    Initial state of the turtle, positioned at the origin (0,0), facing north (90 degrees), with the pen down, and having drawn no lines.
-}
initialTurtleState :: TurtleState
initialTurtleState = TurtleState (0, 0) 90 True []

{-
    The drawTurtle function visually represents the current state of the turtle, including both its path (the lines it has drawn) and its current position 
    (indicated by a turtle image). The function takes the pen's width and colour as parameters to accurately depict the path as it would appear based on 
    the user's inputs. It constructs the turtle's image by translating and rotating it to match the turtle's current orientation and position on the screen. 
    This ensures that the turtle's direction is visually accurate. The path the turtle has drawn is recreated by mapping over the list of line segments stored 
    in the turtle's state, using the drawThickLine function with the specified pen width and colour to ensure each line segment is correctly represented. 
    The resulting images of the turtle and its path are then combined into a single picture, effectively providing a snapshot of the current state of the 
    drawing process.
-}
drawTurtle :: Float -> Color -> TurtleState -> Picture -> Picture
drawTurtle width penColor state turtlePic =
    let (x, y) = position state
        -- Rotates and translates to its current position. The image is scaled down for appropriate representation.
        turtleImage = translate x y $ rotate (-angle state + 90) $ scale 0.07 0.07 turtlePic
        -- Draws all the lines the turtle has produced so far, using the specified pen width and colour.
        drawnLines = map (drawThickLine width penColor) (linesDrawnSoFar state)
    in pictures (turtleImage : drawnLines)

{-
    The drawThickLine function creates a visual representation of a line segment with a specified thickness and colour between two points. 
    It first calculates the angle of the line to ensure the thickness is applied perpendicular to the line's direction, providing a consistent and accurate 
    visual width regardless of the line's orientation. The function then constructs a polygon that represents the thick line by calculating offset points 
    around the start and end points based on the line's angle and the specified thickness. By colouring this polygon with the specified colour and using these 
    calculated points, it achieves the effect of a line with thickness, rather than a simple line.
-}
drawThickLine :: Float -> Color -> ((Float, Float), (Float, Float)) -> Picture
drawThickLine w c (start, end) = color c $ polygon [start, addWidth start w angle, addWidth end w angle, end]
    where 
        -- Calculates the angle of the line for correct thickness orientation.
        angle = atan2 (snd end - snd start) (fst end - fst start)

{-
    The addWidth function calculates offset points to simulate line thickness in the drawing. Given a point (x, y), a width (w) and an angle, it computes 
    a new point that is offset from the original by half the width in the direction perpendicular to the angle of the line. This calculation is based on 
    trigonometric functions, where cos and sin are used to find the offsets in the x and y directions, respectively. The angle is adjusted by 90 degrees
    to ensure the offset is applied perpendicularly to the original line direction.
-}
addWidth :: (Float, Float) -> Float -> Float -> (Float, Float)
addWidth (x, y) w angle = (x + w / 2 * cos (angle + pi / 2), y + w / 2 * sin (angle + pi / 2))

{-
    The moveTurtle function updates the turtle's state to reflect movement in its current facing direction by a specified distance. It begins by converting 
    the turtle's current angle from degrees to radians, facilitating the use of trigonometric functions to calculate the change in the x and y coordinates 
    (dx and dy) for the movement. The new position (newPos) is determined by adding these changes to the turtle's current position. If the turtle's pen is down, 
    the movement results in a new line segment being added to the list of lines drawn so far (newLines). This segment connects the turtle's previous position 
    to its new position, visually representing the turtle's path. The turtle's state is then updated to reflect this new position and, if applicable, the addition 
    of the new line segment.
-}
moveTurtle :: Float -> TurtleState -> TurtleState
moveTurtle dist state =
    let rad = angle state * (pi / 180) -- Converts the current angle to radians for trigonometric calculations.
        dx = dist * cos rad -- Change in the x-direction.
        dy = dist * sin rad -- Change in the y-direction.
        newPos = (fst (position state) + dx, snd (position state) + dy) -- Determines the new position based on the movement.
        newLines = if penDown state 
                then linesDrawnSoFar state ++ [(position state, newPos)] 
                else linesDrawnSoFar state
    in state { position = newPos, linesDrawnSoFar = newLines } -- Returns the updated state with the new position and possibly new lines.
