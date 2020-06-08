module Lib where
import Text.Show.Functions

laVerdad = True


data Festival = Festival {lugarDondeSeRealiza :: String,
                          cantidadDeGente :: Float,
                          animoInicialDelPublico :: String,
                          bandas :: [Banda]
                          } deriving (Show)




data Banda = Banda {nombre :: String,
                    genero :: String,
                    descripciones :: [String],
                    decibeles :: Float
                   } deriving (Show)


--Punto 1

tocar :: Banda -> Festival -> Festival
tocar banda festival = festival {cantidadDeGente = modifGenteSegunGenero banda festival, 
                                 animoInicialDelPublico = modifAnimoSegunGenero banda festival}

modifGenteSegunGenero :: Banda -> Festival -> Float
modifGenteSegunGenero banda festival 
 | genero banda == "rock nacional" = cantidadDeGente festival + 100
 | genero banda == "pop" = popSegunAnimo festival 
 | otherwise = cantidadDeGente festival + (cantidadDeGente festival * 0.01)

popSegunAnimo :: Festival -> Float
popSegunAnimo festival
 | animoInicialDelPublico festival == "indiferente" = (cantidadDeGente festival) *2
 | otherwise = cantidadDeGente festival
 
modifAnimoSegunGenero :: Banda -> Festival -> String
modifAnimoSegunGenero banda festival 
 | genero banda == "pop" = (animoInicialDelPublico festival) ++ " euforico"
 | (losUltimos 5 (genero banda)) == "metal" = metalSegunSubGenero banda festival
 | otherwise =  animoInicialDelPublico festival


losUltimos :: Int -> [a] -> [a]
losUltimos c xs = drop (length xs - c) xs

metalSegunSubGenero :: Banda -> Festival -> String
metalSegunSubGenero banda festival 
 | genero banda == "Heavy metal" = animoInicialDelPublico festival ++ " pesado"
 | genero banda == "Trash metal" = animoInicialDelPublico festival ++ " basura"
 | otherwise = animoInicialDelPublico festival ++ " " ++ (take 5 (genero banda))


--Punto 2


hullabalooza :: Festival
hullabalooza = Festival {lugarDondeSeRealiza = "Argentina",
                          cantidadDeGente = 20000,
                          animoInicialDelPublico = "indiferente",
                          bandas = [miranda,losRedondos,metallica,soda]
                          } 

miranda :: Banda
miranda = Banda {nombre ="Miranda",
                    genero = "pop",
                    descripciones = ["Insipida","Incolora","Inodora"],
                    decibeles = 60
                    } 


losRedondos :: Banda
losRedondos = Banda {nombre ="Los Redondos",
                    genero = "rock nacional",
                    descripciones = ["Legendaria","Pogosa"],
                    decibeles = 45
                    } 


metallica :: Banda
metallica = Banda {nombre ="Metallica",
                    genero = "Heavy metal",
                    descripciones = ["Legendaria","Vendida"],
                    decibeles = 60
                    } 


soda :: Banda
soda = Banda {nombre ="Soda",
                    genero = "rock nacional",
                    descripciones = ["Irrepetible"],
                    decibeles = 60
                    }



megadeth :: Banda
megadeth = Banda {nombre ="Megadeth",
                    genero = "trash metal",
                    descripciones = ["Legendaria"],
                    decibeles = 60
                    }


--Punto 3


theStrokes :: Banda
theStrokes = Banda {nombre ="The Strokes",
                    genero = "Heavy metal/pop",
                    descripciones = ["Suicidio Asistido","Emoconal"],
                    decibeles = 45
                    }


--Punto 4 (CREO QUE ESTA ES LA QUE VA PARA HACER FOLD CON UNA LISTA DE FUNCIONES!!)

suceder :: Festival -> Festival
suceder festival = (queToquenTodas (bandas festival)) festival

queToquenTodas :: [Banda] -> Festival -> Festival
queToquenTodas bandas festival = foldl (flip tocar) festival bandas


--Punto 5

type Clasificacion = Banda -> Bool

vendida :: Clasificacion
vendida banda = ((>= 3). length)  (descripciones banda)  || (length . filter (=="Vendida")) (descripciones banda) == 1

acustica :: Clasificacion
acustica banda = decibeles banda > 55

legendaria :: Clasificacion
legendaria banda =  (length . filter (=="Legendaria")) (descripciones banda) == 1 && decibeles banda > 40  


-- Punto 6

popularidad :: Banda -> [Clasificacion]-> Int
popularidad _ [] = 0
popularidad banda (clasif:clasifs)
 | clasif banda == True = 100 + (popularidad banda clasifs)
 | otherwise = popularidad  banda  clasifs


--Punto 7


esBuenFest :: Festival -> [Clasificacion] -> Bool
esBuenFest festival clasifs =  sum  (map  ((flip popularidad) clasifs) (bandas festival)) > 1000 && estaOrdenadaDos (map ((flip popularidad) clasifs) (bandas festival)) 

popularidadAnt :: Int
popularidadAnt = 0


estaOrdenadaDos :: [Int] -> Bool
estaOrdenadaDos [valor] = True
estaOrdenadaDos (valor1:valor2:valores) 
 | valor1 < valor2 = estaOrdenadaDos (valor2:valores) 
 | otherwise = False 

{--
estaOrdenada :: Festival -> [Clasificacion] [Banda] -> Bool
estaOrdenada _ _ [] = True
estaOrdenada festival clasifs (banda1 : banda2 : bandas)
 | popularidad banda1 clasifs >= popularidadAnt  && estaOrdenado festival clasifs (banda2 : bandas) 
   where popularidadAnt = popularidad banda1 clasifs
--}