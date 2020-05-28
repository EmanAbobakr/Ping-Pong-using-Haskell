module End where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataType
import Graphics.Gloss.Data.ViewPort
import Data.Char
import Data.List

-- Update the pause screen.
updateEnd :: Float -> PongGame -> PongGame 
updateEnd seconds game = game{wherefrom = Endgame}

--
handleEndKeys:: Event -> PongGame -> PongGame
handleEndKeys (EventKey (Char 'q') _ _ _) game  = game {gamemode = Menu}
handleEndKeys _ game = game  

-- Display the winner screen
endRender ::  PongGame -> Picture
endRender game = color black (pictures
 [
   scale (0.5) (0.5) (translate (-800) 0 (text "The winner is ")),
   scale (0.6) (0.6) (translate (200)  0 $ color (choseColor winner) (text (winner))),
   scale (0.3) (0.3) (translate (-650) (-550) (text "(q) Back to Menu"))
 ])
 where winner = getWinner game

-- To who is the winner
getWinner :: PongGame -> String
getWinner game 
              | (score1 game) > 21 = "Player 1"
              | otherwise          = "Player 2"

choseColor :: String -> Color
choseColor "Player 1" = rose
choseColor _          = orange