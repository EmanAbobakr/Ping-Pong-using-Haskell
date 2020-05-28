module Menu where
import DataType
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Update the menu screen.
updateMenu :: Float -> PongGame -> PongGame -> PongGame  -- seconds -> current -> intialstate -> updatedGame
updateMenu seconds game initial = initial

-- Handle the key events on the menu like play
handleMenuKeys :: Event -> PongGame -> PongGame
handleMenuKeys (EventKey (Char '1') _ _ _) game =  game {gamemode = Play}                          
handleMenuKeys _ game = game

-- Display the menu with some options.
menuRender :: PongGame -> Picture
menuRender game = color black (pictures
 [
   translate (-400) 290 (text "-------"),
   translate (-420) 200 (text "|"),
   translate (-370) 200 $ color (light blue) (text "Ping"),
   translate (-150) 200 $ color orange (text " Pong"),
   translate (-400) 110 (text "-------"),
   translate (300) 200 (text "|"),
   scale (0.5) (0.5) (translate (-550) (-50) (text "(1) To Play")),
   scale (0.5) (0.5) (translate (-550) (-250) (text "(q) Back to Menu")),
   scale (0.5) (0.5) (translate (-550) (-450) (text "(p) To Pause"))

 ])
