module DataType where
import Graphics.Gloss


-- | Game constants int type
width, height, offset:: Int
width = 1000
height = 700
offset = 100

-- | Data describing the state of the pong game. 
data PongGame = Game
  { gamemode :: GameMode
  , wherefrom :: GameMode
  , ballLoc :: (Float, Float)  --  Pong ball (x, y) location.
  , ballVel :: (Float, Float)  --  Pong ball (x, y) velocity. 
  , player1 :: Float           --  Right player paddle height.                          
  , player2 :: Float           --  left player paddle height.
  , score1  :: Float		   --  Right player Score.
  , score2  :: Float		   --  Left player Score.

  } deriving Eq

-- | all possible game modes 
data GameMode = Menu | Pause | EndGame | Play deriving Eq  -- deriving Eq so the gamemodes will be comparable