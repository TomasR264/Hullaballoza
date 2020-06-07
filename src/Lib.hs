module Lib where
import Text.Show.Functions

laVerdad = True


data Festival = Festival {lugarDondeSeRealiza :: String,
                          cantidadDeGente :: Float,
                          animoInicialDelPublico :: String,
                          bandas :: [Banda]
                          } deriving (Show)




data Banda = Banda {nombre :: String,
                    genero :: String
                   } deriving (Show)




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
 | genero banda == "pop" = "euforico"
 | otherwise = metalSegunSubGenero banda festival


metalSegunSubGenero :: Banda -> Festival -> String
metalSegunSubGenero banda festival 
 | genero banda == "Heavy metal" = animoInicialDelPublico festival ++ "pesado"
 | genero banda == "Trash metal" = animoInicialDelPublico festival ++ "basura"
 | otherwise = animoInicialDelPublico festival ++ (take 5 (genero banda))






