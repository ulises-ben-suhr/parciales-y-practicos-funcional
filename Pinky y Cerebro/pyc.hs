data Animal = Animal {
    intelecto :: Int,
    especie :: String,
    capacidades :: [String]
} deriving(Show,Eq)

cerebro :: Animal
cerebro = Animal 150 "raton" ["estratega"]
pinky :: Animal
pinky = Animal 115 "raton" ["espia","hacer narf","hacer asda","hacer guau"]


inteligenciaSuperior :: Int -> Animal -> Animal
inteligenciaSuperior n bicho = bicho { intelecto = intelecto bicho + n }

pinkificar :: Animal -> Animal
pinkificar bicho = bicho { capacidades = [] }

superpoderes :: Animal -> Animal
superpoderes bicho | especie bicho == "elefante" = bicho { capacidades = capacidades bicho ++ ["no tenerle miedo a los ratones"] }
                   | especie bicho == "raton" && intelecto bicho > 100 = bicho { capacidades = capacidades bicho ++ ["hablar"] }
                   | otherwise = bicho

------------------------------------------------------------------------------------------------------------------------------------------------

antropomorficos :: Animal -> Bool
antropomorficos = (\ b -> ((> 60) . intelecto) b && (elem "hablar" .capacidades) b)

noTanCuerdo :: Animal -> Bool
noTanCuerdo = (>2) . length . filter pinkiesco . capacidades

pinkiesco :: String -> Bool
pinkiesco habilidad = ((== "hacer") . take 5) habilidad && ((\p -> ((<=4) . length) p && any esVocal p) . drop 6) habilidad

esVocal :: Char -> Bool
esVocal letra = letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'

------------------------------------------------------------------------------------------------------------------------------------------------

type Transformacion = Animal -> Animal

data Experimento = Experimento {
    pruebas :: [Transformacion],
    criterioExito ::  Animal -> Bool
}

torturas1 :: Experimento
torturas1 = Experimento [pinkificar,inteligenciaSuperior 10, superpoderes] antropomorficos

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experimento bicho =  criterioExito experimento ((flip aplicarPruebas bicho . pruebas) experimento)

aplicarPruebas ::  [Transformacion] -> Animal -> Animal
aplicarPruebas torturas bicho = foldl (\animalito prueba -> prueba $ animalito) bicho torturas

------------------------------------------------------------------------------------------------------------------------------------------------

reporte :: [String] -> [Transformacion] -> ([String] -> [String] -> Bool) -> [Animal] -> [Animal]
reporte habilidades experimento cuantificador = filter (cuantificador habilidades . capacidades) . map (aplicarPruebas experimento)

reporteIntelectual :: [Animal] -> [String] -> [Transformacion] -> [Int]
reporteIntelectual victimas habilidades experimento = (map intelecto . reporte habilidades experimento hayRepetidos) victimas

reporteTaxonomico :: [Animal] -> [String] -> [Transformacion] -> [String]
reporteTaxonomico victimas habilidades experimento = (map especie . reporte habilidades experimento todosRepetidos) victimas

reporteCapacitivo :: [Animal] -> [String] -> [Transformacion] -> [[String]]
reporteCapacitivo victimas habilidades experimento = (map capacidades . reporte habilidades experimento noHayRepetidos) victimas

noHayRepetidos :: Eq a => [a] -> [a] -> Bool
noHayRepetidos lista = not . hayRepetidos lista

hayRepetidos :: Eq a => [a] -> [a] -> Bool
hayRepetidos lista = any (\v -> elem v lista)

todosRepetidos :: Eq a => [a] -> [a] -> Bool
todosRepetidos lista = all (\v -> elem v lista)

