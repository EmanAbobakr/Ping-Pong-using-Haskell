module Pause where
import DataType
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Update the pause screen.
updatePause :: Float -> PongGame -> PongGame 
updatePause seconds game = game{wherefrom = Pause}

-- Handle the keys on the pause screen.
handlePauseKeys :: Event -> PongGame -> PongGame
handlePauseKeys (EventKey (Char 'q') _ _ _) game  = game {gamemode = Menu}                                 
handlePauseKeys (EventKey (Char 'p') Down _ _) game  = game {gamemode = Play}
handlePauseKeys _ game = game

-- Display the pause screen
pauseRender ::  PongGame -> Picture
pauseRender game = color black (pictures
 [
   translate (-350) 200 (text "Paused"),
   translate (-350) 120 (text "----"),
   scale (0.4) (0.4) (translate (-800) (100) (text "(p) Resume")),
   scale (0.4) (0.4) (translate (-800) (-100) (text "(q) Return to Menu"))
 ])