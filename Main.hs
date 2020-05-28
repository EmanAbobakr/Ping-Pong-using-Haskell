module Main(main) where
import DataType
import Play
import Pause
import Menu
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
--import data

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = white

-- | Number of frames to show per second.
fps :: Int
fps = 60


main :: IO ()
-- | Run a finite-time-step simulation in a window.
--simulate :: Display -- ^ How to display the game.
--         -> Color   -- ^ Background color.
--         -> Int     -- ^ Number of simulation steps to take per second of real time.
--         -> a       -- ^ The initial game state. 
--         -> (a -> Picture) -- ^ A function to render the game state to a picture. 
--         -> (ViewPort -> Float -> a -> a) -- ^ A function to step the game once. 
--        -> IO ()
main = play window background fps initialState render handleKeys update

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { gamemode = Menu
  , wherefrom = Menu
  , ballLoc = (40, 0)
  , ballVel = (4, 0)
  , player1 = 40    --right
  , player2 = 80      --left
  , score1  = 0
  , score2  = 0
  }


-- | Render the game according to the Game Mode.
render :: PongGame -> Picture 
render game 
            | (gamemode game) == Menu = menuRender game
            | (gamemode game) == Pause = pauseRender game
            | otherwise = renderPlay game

            -- | Handle the key events according to the Game Mode.
handleKeys :: Event -> PongGame -> PongGame   -- Event -> currentGame -> updatedGame
handleKeys event game 
                    | (gamemode game) == Menu = handleMenuKeys event game  
                    | (gamemode game) == Pause = handlePauseKeys event game        
                    | otherwise = handlePlayKeys event game


update :: Float -> PongGame -> PongGame                
update seconds game 
                | (gamemode game) == Menu = updateMenu seconds game initialState
                | (gamemode game) == Pause = updatePause seconds game
                | otherwise = updatePlay seconds game


