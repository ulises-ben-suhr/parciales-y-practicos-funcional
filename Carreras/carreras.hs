data Auto = Auto {
    color :: String,
    velocidad ::Int,
    distancia :: Int
} deriving (Show,Eq)

type Carrera = [Auto]

rojo :: Auto
rojo = Auto "rojo" 120 0

blanco :: Auto
blanco = Auto "blanco" 120 0

azul :: Auto
azul = Auto "azul" 120 0

negro :: Auto
negro = Auto "negro" 120 0



competidores :: Carrera
competidores = [rojo, blanco, azul, negro]

sonElMismo :: Auto -> Auto -> Bool
sonElMismo coche1 coche2 = color coche1 == color coche2

sonDistintos :: Auto -> Auto -> Bool
sonDistintos coche1 coche2 = color coche1 /= color coche2

deltaDistancia :: Auto -> Auto -> Int
deltaDistancia coche1 coche2 = abs (distancia coche2 - distancia coche1)

vaGanandole :: Auto -> Auto -> Bool
vaGanandole coche1 coche2 = distancia coche2 > distancia coche1

-- PUNTO 1

estaCerca :: Auto -> Auto -> Bool
estaCerca coche1 coche2 = sonDistintos coche1 coche2 && ((< 10) . deltaDistancia coche1) coche2 

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo coche rivales = (not . any (estaCerca coche)) rivales && ((== distancia coche) . maximum . map distancia) rivales

puesto :: Auto -> Carrera -> Int
puesto coche = (+1) . length . filter (vaGanandole coche)

-- PUNTO 2

correr :: Int -> Auto -> Auto 
correr tiempo coche = coche {distancia = distancia coche + velocidad coche * tiempo}

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad perro coche = coche {velocidad = max 0 (((perro) . velocidad) coche)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad v = alterarVelocidad (subtract v)


-- PUNTO 3

type PowerUp = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> Carrera -> Carrera
terremoto coche = afectarALosQueCumplen (estaCerca coche) (bajarVelocidad 50)

miguelitos :: Int -> Auto -> Carrera -> Carrera
miguelitos valor coche = afectarALosQueCumplen (vaGanandole coche) (bajarVelocidad valor)

jetPack :: Int -> Auto -> Carrera -> Carrera
jetPack tiempo coche = afectarALosQueCumplen (sonElMismo coche) (alterarVelocidad (`div` 2) . correr tiempo . alterarVelocidad (*2))

-- PUNTO 4

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int,String)]
simularCarrera corredores = map (\c -> (distancia c, color c)) . foldl (\c ev -> ev c ) corredores

correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo = map (correr tiempo)

usaPowerUp :: PowerUp -> String -> Carrera -> Carrera
usaPowerUp powerup colorCoche corredores = powerup ((head . filter ((== colorCoche) . color)) corredores) corredores

ejSimulacion :: [(Int,String)]
ejSimulacion = simularCarrera competidores [correnTodos 30, usaPowerUp (jetPack 3) "azul",
    usaPowerUp terremoto "blanco", correnTodos 40, usaPowerUp (miguelitos 20) "blanco",
    usaPowerUp (jetPack 3) "negro", correnTodos 10]

-- PUNTO 5

-- A) El misil teledirigido puede tener el tipo String -> Auto -> Carrera -> Carrera, y no habría que refactoriazar toda la solución, por lo que lo veo factible. De hecho cualquier power up que cumpla el tipo Auto -> Carrera -> Carrera puede ser agregado

-- B) Ambas funciones tienen por dominio a un elemento de un conjunto y el conjunto, y ambas deben comparar a todos los elementos del conjunto para determinar la imagen, si la lista es infinita, lo que se puede hacer es tomar los primeros x elementos y aplicar cualquiera de las dos funciones, lo que pasa es que haciendo eso, el resultado obtenido sería relativo a la porción del conjunto tomado, no al la infinidad de elementos que lo componen.
