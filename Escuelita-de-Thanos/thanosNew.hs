-- PRIMERA PARTE

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} 

type Gema = Personaje -> Personaje

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    energia :: Int,
    planetaResidencia :: String,
    habilidades :: [String]
} deriving(Show, Eq)

data Universo = Universo {
    habitantes :: [Personaje]
} deriving(Show, Eq)

-- PUNTO 1

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo 
    | esDelInfinito guantelete =  universo {habitantes = (take (((`div` 2) . length . habitantes) universo) . habitantes) universo }
    | otherwise = universo

esDelInfinito :: Guantelete -> Bool
esDelInfinito guantelete = ((== "uru") . material) guantelete && ((== 6) . length . gemas) guantelete

-- PUNTO 2

aptoPendex :: Universo -> Bool
aptoPendex = any ((<45) . edad) . habitantes

energiaUniversal :: Universo -> Int
energiaUniversal = sum . map energia . filter ((>1) . length . habilidades) . habitantes

-- SEGUNDA PARTE

-- PUNTO 3

quitarEnergia :: Int -> Personaje -> Personaje
quitarEnergia valor personaje = personaje {energia = energia personaje - valor}

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad habilidad personaje = personaje {habilidades = (filter (/= habilidad). habilidades) personaje}

teletransportar :: String -> Personaje -> Personaje
teletransportar planeta personaje = personaje {planetaResidencia = planeta}

rejuvenecer :: Int -> Personaje -> Personaje
rejuvenecer anios personaje = personaje {edad = max 18 (edad personaje - anios)}




laMente :: Int -> Personaje -> Personaje
laMente energiaPerdida = quitarEnergia energiaPerdida

elAlma :: String -> Personaje -> Personaje
elAlma habilidadPerdida enemigo 
    | ( elem habilidadPerdida . habilidades ) enemigo = (quitarEnergia 10 . quitarHabilidad habilidadPerdida) enemigo
    | otherwise = quitarEnergia 10 enemigo

elEspacio :: String -> Personaje -> Personaje
elEspacio planeta = quitarEnergia 20 . teletransportar planeta

elPoder :: Personaje -> Personaje
elPoder enemigo 
    | ((<=2) . length . habilidades) enemigo = enemigo {energia = 0, habilidades = []}
    | otherwise = enemigo {energia = 0} 

elTiempo :: Personaje -> Personaje
elTiempo enemigo | ((>18) . edad) enemigo = (quitarEnergia 50 . rejuvenecer (((`div` 2) . edad) enemigo)) enemigo
                 | otherwise = quitarEnergia 50 enemigo

laLocura :: Gema -> Personaje -> Personaje
laLocura gema = gema . gema

-- PUNTO 4 y 5

manopla :: Guantelete
manopla = Guantelete "goma" [elTiempo, elAlma "usar mjolnir", laLocura (elAlma "programacion en Haskell")]

ulises :: Personaje
ulises = Personaje "ulises" 24 60 "tierra" ["programacion en Haskell", "usar mjolnir", "cocinar"]

atacar :: Guantelete -> Personaje -> Personaje
atacar guantelete personajeInicial = foldl (\pers efecto -> efecto pers ) personajeInicial (gemas guantelete)

