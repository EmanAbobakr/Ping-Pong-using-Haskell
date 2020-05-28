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
   translate (-300) 200 (text "Ping Pong"),
   translate (-300) 110 (text "-------"),
   scale (0.5) (0.5) (translate (-450) (-50) (text "(1) To Play")),
   scale (0.5) (0.5) (translate (-450) (-250) (text "(q) back to MENU")),
   scale (0.5) (0.5) (translate (-450) (-450) (text "(P) To Pause"))

 ])
