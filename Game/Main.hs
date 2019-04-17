module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Data.Char
import System.IO

width, height, offset :: Int
width = 600
height = 600
offset = 100  

data MyGame = Game
  { quadLoc :: (Float, Float)  
  , quadVel :: (Float)   
  , paredeTopoLoc::(Float,Float)
  , paredeTopo2Loc::(Float,Float)
  ,espacoLoc::(Float,Float)
  ,pos::[Float] 
  ,pontos::(Int)
  ,menu::(Bool)
  ,estadoMenu::(Int)
  } deriving Show

initialState :: MyGame
initialState = Game
  { quadLoc = (0,-200) --Localização do quadrado
  , quadVel = (5) --Velocidade Inicial do quadrado
  ,	paredeTopoLoc=(-300	,285) --Posição inicial da parede esquerda
  ,paredeTopo2Loc=(300,285) --Posição inicial da parede direita
  ,espacoLoc=(0,285) -- Posicão do quadrado de saída
  ,pos = [100,-20,180,-160,-60,200,140,-140,-120,160,180,-180,100,140,-20,-60] --posição da saída de cada fase
  ,pontos = (0)		--pontos da jogada
  ,menu = True --saber se esta no menu
  ,estadoMenu = (0) --opcao do menu
  }

window :: Display
window = InWindow "Reverse Tetris" (width, height) (offset, offset)

background :: Color
background = black

render :: MyGame -> Picture 
render game = if menu game -- se o estado inicial eh o menu
	then renderMenu game   -- renderiza o menu
	else renderGame game   -- renderiza o game

renderMenu :: MyGame -> Picture 
renderMenu game = 
	pictures [titulo,opcao1]
	where
    --opcao2 =  Translate (-170) (-20) $ Scale 0.5 0.5 $ color corTextoMenu $ Text "Meu joguin"
		titulo =  Translate (-170) (-20) $ Scale 0.5 0.5 $ color corTextoMenu $ Text "Meu joguin"
		opcao1 =  Translate (-100) (-90) $ Scale 0.15 0.2 $ color corTextoMenu $ Text "Pressione 'p' para jogar"
		corTextoMenu = dark red
		
renderGame :: MyGame -> Picture   
renderGame game  =	
  pictures [quadrado, walls,paredeTopo,paredeTopo2,espaco, pontuacao]
  where

    quadrado = uncurry translate (quadLoc game) $ color quadradocolor $ rectangleSolid 30 30 
    quadradocolor = dark red

    espaco = uncurry translate (espacoLoc game) $ color espacocolor $  rectangleSolid 30 30
    espacocolor = white

    paredeTopo = uncurry translate (paredeTopoLoc game) $ color paredeTopocolor $ rectangleSolid 560 30 
    paredeTopocolor = green

    paredeTopo2 = uncurry translate (paredeTopo2Loc game) $ color paredeTopo2color $ rectangleSolid 560	 30 
    paredeTopo2color = green

    pontuacao = Translate (-290) (-290) $ Scale 0.5 0.5 $ color quadradocolor $ Text $ show $ pontos game

    wall3 :: Float -> Picture
    wall3 offset =
      translate (-300) offset $
        color wallColor $
          rectangleSolid 10 600
    wall2 :: Float -> Picture
    wall2 offset =
      translate 300 offset $
        color wallColor $
          rectangleSolid 10 600

    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 600 10

    wallColor = greyN 0.5
    walls = pictures [wall (-300),wall2 (0),wall3 (0)]

moveQuad :: Float    
         -> MyGame 
         -> MyGame 

moveQuad seconds game = game { quadLoc = (x', y'),quadVel = vy' }
  where
    
    (x, y) = quadLoc game
    (vy) = quadVel game
    x' = x 
    y' = y + vy
    vy' = vy 


fps :: Int
fps = 60


type Num1 = Float 
type Position = (Float, Float)

getPos:: [Float]->Float
getPos [] = 0
getPos (x:xs) = x

getTail::[Float]->[Float]
getTail [] = [0,100,-20,180,-160,-60,0,200,140,-140,0,-120,180]
getTail (x:xs)
	| xs == [] = [0,100,-20,180,-160,-60,0,200,140,-140,0,-120,180]
	|otherwise = xs

wallCollision :: Position->Position -> Bool 
wallCollision (x, y)(x',_) = topCollision 
  where
    topCollision    = y >=255 && (x/=x')

espacoCollision :: Position ->Position -> Bool 
espacoCollision (x, y)(x',_)  = tCollision 
  where
    tCollision    = y >=290 && (x==x') 	


wallBounce :: MyGame -> MyGame
wallBounce game = game
	{ pontos = pontos'
	, quadVel=vy'
	,pos = pos'
	, quadLoc = (x',y')
	,espacoLoc = (espacoX',espacoY)
	,paredeTopoLoc = (paredeT1x',paredeT1y)
	, paredeTopo2Loc = (paredeT2x',paredeT2y) }
  where
    (x,y) = quadLoc game
    (paredeT1x,paredeT1y) = paredeTopoLoc game
    (paredeT2x,paredeT2y) = paredeTopo2Loc game
    (espacoX,espacoY)  = espacoLoc game
    posL = pos game
    num = getPos (pos  game)
    vy = (quadVel game)
    antigaPontuacao = pontos game
    novaPontuacao = antigaPontuacao 

    (pontos', vy',pos',paredeT1x',paredeT2x',espacoX',x',y') = if wallCollision (quadLoc game) (espacoLoc game) -- If colidiu com parede verde
          then
            (upPont(antigaPontuacao-5), vy ,posL,paredeT1x,paredeT2x,espacoX,x,-280)	
          else
            
  				if espacoCollision (quadLoc game) (espacoLoc game) --se quadrado colidiu com a saída
    			then
    				( antigaPontuacao+10, vy, getTail posL, -300+num, 300+num, num, 0, -280) 
    			  --valor da fase, pontos, velocidade, num fases passadas, proxima fase, nova parede Esq, nova perede dir, posicao da saída, x do quadrado, y do quadrado)
    			else
    				( antigaPontuacao, vy,posL,paredeT1x,paredeT2x,espacoX,x,y)


upPont :: Int -> Int
upPont x
	|x < 0 = 0
	|otherwise = x 
handleKeys :: Event -> MyGame -> MyGame


handleKeys (EventKey (Char 'p') _ _ _) game =
	game {estadoMenu = estadoMenu', menu = val'}
  where
    menuV = menu game
    estadoAtual = estadoMenu game
    (estadoMenu',val') =if menuV==False
        then 
          if estadoAtual==0
          then (1,False)

          else(0,True)
        else 
          if estadoAtual ==0
            then(1,True)
            else(0,False)



handleKeys (EventKey (Char 'd') _ _ _) game =
  game { quadLoc = (x', y') }
  where
  	(x,y)= quadLoc game
  	x' = x+10
  	y'=y


handleKeys (EventKey (Char 'a') _ _ _) game =
  game { quadLoc = (x', y') }
  where
  	(x,y)= quadLoc game
  	x' = x-10
  	y'=y

handleKeys (EventKey (Char '1') _ _ _) game =
  game { quadVel = vy' }
  where
    vy'= 3
handleKeys (EventKey (Char '2') _ _ _) game =
  game { quadVel = vy' }
  where
    vy'= 5
handleKeys (EventKey (Char '3') _ _ _) game =
  game { quadVel = vy' }
  where
    vy'= 7


handleKeys _ game = game

update :: Float -> MyGame -> MyGame 
update seconds = wallBounce . moveQuad seconds

estadoJogo :: Float -> MyGame -> MyGame 
estadoJogo seconds game = if menu game --Se eu estou no menu
	then game--roda menu
	else update seconds game -- Senao roda game


main :: IO ()
main = play window background fps initialState render handleKeys estadoJogo







