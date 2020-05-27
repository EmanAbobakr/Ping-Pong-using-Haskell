module Play where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataType
import Graphics.Gloss.Data.ViewPort



-- Update the game by moving the ball and bouncing off walls. (function composition)
updatePlay ::  Float -> PongGame -> PongGame
updatePlay  seconds = wallBounce . moveBall seconds


-- | handle the playmode keys.
handlePlayKeys::Event->PongGame->PongGame
-- For an 's' keypress, reset the ball to the center.
handlePlayKeys (EventKey (Char 'r') Down _ _) game = game { ballLoc = (0, 0) }
handlePlayKeys (EventKey (Char 'q') _ _ _) game  = game {gamemode = Menu}
handlePlayKeys (EventKey (Char 'p') Down _ _) game = game {gamemode = Pause}
handlePlayKeys evnent game = game


-- | Convert a game state into a picture.
renderPlay :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
renderPlay game =
  pictures [ball, walls,
            mkPaddle rose (fromIntegral width / 2 - 15) $ player1 game,
            mkPaddle orange (fromIntegral (-width) / 2 + 15) $ player2 game,
            translate 0 0 $ color ballColor $ circle 15,
            translate 0 0 $ color ballColor $ rectangleSolid 1 (fromIntegral height)]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 15
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid (fromIntegral width) 10

    wallColor = greyN 0.5
    walls = pictures [wall (fromIntegral height/2 - 5), wall (fromIntegral (-height) / 2 + 5)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)


-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds * 100
    y' = y + vy * seconds * 100



type Radius = Float 
type Position = (Float, Float)

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >=  fromIntegral height / 2


wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy


