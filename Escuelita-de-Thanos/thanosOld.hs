--------------------------------
--    ESTRUCTURAS DE DATOS    --
--------------------------------

data Gema = Gema {
    tipo :: String,
    efecto :: Personaje -> Personaje
}

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    planetaResidencia :: String,
    energia :: Int,
    habilidades :: [String]
} deriving (Show, Eq)

data Universo = Universo {
    habitantes :: [Personaje]
}

--------------------------------
--     DATOS PREDEFINIDOS     --
--------------------------------
delAlma :: Gema
delAlma = Gema "alma" controlarAlma
delTiempo :: Gema
delTiempo = Gema "tiempo" quitarTiempoDeVida
deLaLocura :: Gema
deLaLocura = Gema "locura" otraGema
delEspacio :: Gema
delEspacio = Gema "espacio" transportar
deLaMente :: Gema
deLaMente = Gema "mente" debilitarEnergia
delPoder :: Gema
delPoder = Gema "poder" quitarEnergia

delInfinito :: Guantelete
delInfinito = Guantelete "uru" [delAlma, delTiempo, deLaLocura, delEspacio, deLaMente, delPoder]
delChanta :: Guantelete
delChanta = Guantelete "madera" []
delHeroe :: Guantelete
delHeroe = Guantelete "vibranio" [delAlma, delEspacio]
delVillano :: Guantelete
delVillano = Guantelete "adamantio" [deLaMente, deLaLocura, delTiempo]
delPayaso :: Guantelete
delPayaso = Guantelete "goma" [delTiempo, delAlma]

drStrange :: Personaje
drStrange = Personaje "Dr Strange" 43 "La Tierra" 100 ["Volar", "Abrir portales", "Crear objetos magicos", "Manipular el Tiempo", "Entrar en la dimension espejo", "Manipular la dimension espejo"]
ironMan :: Personaje
ironMan = Personaje "Iron Man" 47 "La Tierra" 85 ["Super inteligencia", "Crear tecnologia avanzada", "Pilotar armaduras Mark"]
groot :: Personaje
groot = Personaje "Groot" 602 "Planeta X" 50 ["Estirar cuerpo", "Super fuerza", "Inmortal"]
deadpool :: Personaje
deadpool = Personaje "Deadpool" 31 "La Tierra" 45 ["Inmortal", "Super precision"]
viudaNegra :: Personaje
viudaNegra = Personaje "Viuda Negra" 26 "La Tierra" 45 ["Artista marcial"]

universo101 :: Universo
universo101 = Universo [drStrange, ironMan, groot, deadpool, viudaNegra]


--------------------------------
--           PARTE 1          --
--------------------------------


chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo | esDelInfinito guantelete = universo { habitantes = (take (((`div` 2) . length . habitantes) universo) . habitantes) universo }
                              | otherwise = universo

esDelInfinito :: Guantelete -> Bool
esDelInfinito guantelete = ((== "uru").material) guantelete && ((== 6) . length . gemas) guantelete

aptoPendex :: Universo -> Bool
aptoPendex = any (<45) . map edad . habitantes

energiaUniversal :: Universo -> Int
energiaUniversal = sum . map energia . filter (\h -> ( (>1) . length . habilidades) h) . habitantes


--------------------------------
--           PARTE 2          --
--------------------------------



controlarAlma :: Personaje -> Personaje
controlarAlma rival = rival

quitarTiempoDeVida :: Personaje -> Personaje
quitarTiempoDeVida rival = rival

otraGema :: Personaje -> Personaje
otraGema rival = rival

transportar :: Personaje -> Personaje
transportar rival = rival

debilitarEnergia :: Personaje -> Personaje
debilitarEnergia rival = rival

quitarEnergia :: Personaje -> Personaje
quitarEnergia rival = rival

















-- utilizar :: [Gema] -> Personaje