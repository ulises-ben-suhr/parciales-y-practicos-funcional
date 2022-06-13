data Planeta = Planeta {
    nombrePlaneta :: String,
    posicion :: Posicion,
    relacionTemporal :: (Int -> Int)
} 

data Posicion = Posicion {
    coordX :: Float,
    coordY :: Float,
    coordZ :: Float
} deriving (Show, Eq)

data Astronauta = Astronauta {
    nombrePersona :: String,
    edadTerrestre :: Int,
    planeta :: Planeta
} 

coordenadasPlaneta :: Planeta -> [Float]
coordenadasPlaneta p = [(coordX . posicion) p, (coordY . posicion) p, (coordZ . posicion) p]

deltaCoordenadas :: Planeta -> Planeta -> [Float]
deltaCoordenadas p1 p2 = zipWith subtract (coordenadasPlaneta p1) (coordenadasPlaneta p2)

cuadrado :: Float -> Float
cuadrado base = base * base

coeficienteTemporal :: Astronauta -> (Int -> Int)
coeficienteTemporal = relacionTemporal . planeta

cambiarUbicacion :: Planeta -> Astronauta -> Astronauta
cambiarUbicacion destino viajero = viajero {planeta = destino}


distanciaEntrePlanetas :: Planeta -> Planeta -> Float
distanciaEntrePlanetas p1 = sqrt . sum . map cuadrado . deltaCoordenadas p1

tiempoViaje :: Float -> Planeta -> Planeta -> Float
tiempoViaje velocidad p1 = (/ velocidad) . distanciaEntrePlanetas p1

pasarTiempo :: Int -> Astronauta -> Astronauta
pasarTiempo anios viajero
    = viajero {edadTerrestre = ((+ (edadTerrestre viajero)) . ($ anios) . coeficienteTemporal) viajero}

type Nave = Planeta -> Planeta -> Float

naveVieja :: Int -> Planeta -> Planeta -> Float
naveVieja tanques origen destino
    | tanques >= 6 = tiempoViaje 7 origen destino
    | otherwise = tiempoViaje 10 origen destino

naveFuturista :: Planeta -> Planeta -> Float
naveFuturista _ _ = 0



