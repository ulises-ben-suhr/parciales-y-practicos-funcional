------------------------ MODELO INICIAL ------------------------

data Jugador = Jugador {
    nombre :: String,
    padre :: String,
    habilidad :: Habilidad
} deriving (Show,Eq)

data Habilidad = Habilidad {
    fuerzaJugador :: Int,
    precisionJugador :: Int
} deriving (Show,Eq)

data Tiro = UnTiro {
    velocidad :: Int,
    precision :: Int,
    altura :: Int
} deriving (Show,Eq)

type Puntos = Int

----------------------- FUNCIONES UTILES -----------------------

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord b => (a -> b) -> a -> a -> a
mayorSegun f a b
    | f a > f b = a
    | otherwise = b

--------------------- JUGADORES DE EJEMPLO ---------------------

bart :: Jugador
bart = Jugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = Jugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

----------------------------------------------------------------
-------------------------- RESOLUCION --------------------------
----------------------------------------------------------------

---------------------------- PUNTO 1 ---------------------------

type Palo = Habilidad -> Tiro

putter :: Habilidad -> Tiro
putter skill = UnTiro {velocidad = 10, precision = 2 * precisionJugador skill, altura = 0}

madera :: Habilidad -> Tiro
madera skill = UnTiro {velocidad = 100, altura = 5, precision = precisionJugador skill `div` 2}

hierros :: Int -> Habilidad -> Tiro
hierros n skill = UnTiro {velocidad = n * fuerzaJugador skill, precision = precisionJugador skill `div` n, altura = max 0 (n - 3)}

palosDisponibles :: [Palo]
palosDisponibles = [putter, madera, hierros 1, hierros 2, hierros 3, hierros 4,
    hierros 5, hierros 6, hierros 7, hierros 8, hierros 9, hierros 10]

---------------------------- PUNTO 2 ---------------------------

golpe :: Jugador -> Palo -> Tiro
golpe tirador palo = (palo . habilidad) tirador

---------------------------- PUNTO 3 ---------------------------

type Obstaculo = Tiro -> Tiro

tiroMuerto :: Tiro
tiroMuerto = UnTiro {velocidad = 0, precision = 0, altura = 0}

tiroGanador :: Tiro
tiroGanador = UnTiro {velocidad = 0, precision = 0, altura = 0}

tunelConRampita :: Tiro -> Tiro
tunelConRampita tiro
    | precision tiro > 90 && altura tiro == 0 = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}
    | otherwise = tiroMuerto

laguna :: Int -> Tiro -> Tiro
laguna longitud tiro
    | velocidad tiro > 80 && between 1 5 (altura tiro) = tiro {altura = altura tiro `div` longitud}
    | otherwise = tiroMuerto

hoyo :: Tiro -> Tiro
hoyo tiro
    | between 5 20 (velocidad tiro) && altura tiro == 0 && precision tiro > 95 = tiroGanador
    | otherwise = tiroMuerto

---------------------------- PUNTO 4 ---------------------------

palosUtiles :: Jugador -> Obstaculo -> [Palo] -> [Palo]
palosUtiles tirador obstaculo = filter ((/= tiroMuerto) . obstaculo . golpe tirador )

obstaculosSuperados :: Tiro -> [Obstaculo] -> Int
obstaculosSuperados tiro = length . takeWhile (\obs ->((/= tiroMuerto) . obs) tiro)

paloMasUtil :: Jugador -> [Obstaculo] -> [Palo] -> Palo
paloMasUtil tirador obstaculos = maximoSegun (flip obstaculosSuperados obstaculos . golpe tirador)

---------------------------- PUNTO 5 ---------------------------

padresDeNiniosQueNoGanaron :: [(Jugador, Int)] -> [String]
padresDeNiniosQueNoGanaron tabla = (map (padre . fst) . filter ( (< (snd (maximoSegun snd tabla))) . snd )) tabla

tablaPuntos :: [(Jugador, Int)]
tablaPuntos = [(bart, 12), (todd, 12), (rafa, 15)]







