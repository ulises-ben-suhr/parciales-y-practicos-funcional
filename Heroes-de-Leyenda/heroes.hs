------------------------------ MODELO ------------------------------

data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareasRealizadas :: [String]
} deriving (Eq)

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
} deriving (Eq, Show)

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Heroe -> Bool
}

------------------------ TIPOS DE DATOS ------------------------

type Tarea = Heroe -> Heroe

type Labor = [Tarea]

------------------------ OBJETOS DE EJEMPLO ------------------------

lanzaOlimpo :: Artefacto
lanzaOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

relampagoZeus :: Artefacto
relampagoZeus = Artefacto "Relampago de Zeus" 500

pistola :: Artefacto
pistola = Artefacto "pistola" 1000

------------------------ HEROES DE EJEMPLO ------------------------

heracles :: Heroe
heracles = Heroe "Guardian del Olimpo" 700 [pistola, relampagoZeus] []

------------------------ BESTIAS DE EJEMPLO ------------------------

leonNemea :: Bestia
leonNemea = Bestia "Leon de Nemea" (\heroe -> (length . epiteto) heroe >= 20)

------------------------ FUNCIONES AUXILIARES ------------------------

removerComunes :: [Artefacto] -> [Artefacto]
removerComunes = filter (\a -> rareza a < 1000)

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza art = art {rareza = rareza art * 3}

vence :: Heroe -> Bestia -> Bool
vence heroe =  ($ heroe) . debilidad

rarezaArtefactos :: Heroe -> Int
rarezaArtefactos = (sum . map rareza . artefactos)

vencedorReconocimiento :: Heroe -> Heroe -> Heroe
vencedorReconocimiento h1 h2
    | reconocimiento h1 > reconocimiento h2 = h1
    | reconocimiento h2 > reconocimiento h1 = h2
    | otherwise = vencedorArtefactos h1 h2

vencedorArtefactos :: Heroe -> Heroe -> Heroe
vencedorArtefactos h1 h2
    | rarezaArtefactos h1 > rarezaArtefactos h2 = h1
    | rarezaArtefactos h2 > rarezaArtefactos h1 = h2
    | otherwise = Heroe "anonimo" 0 [] []

------------------------ FUNCIONES REQUERIMIENTO ------------------------

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria h
    | reconocimiento h > 1000 = h {epiteto = "El Mitico"}
    | reconocimiento h >= 500 = h {epiteto = "El Magnifico", artefactos = artefactos h ++ [lanzaOlimpo]}
    | reconocimiento h > 100 = h {epiteto = "Hoplita", artefactos = artefactos h ++ [xiphos]}
    | otherwise = h

encontrarArtefacto :: Artefacto -> Tarea -- Heroe -> Heroe
encontrarArtefacto reliquia h
    = h {reconocimiento = reconocimiento h + rareza reliquia,
    artefactos = artefactos h ++ [reliquia],
    tareasRealizadas = tareasRealizadas h ++ ["Encontrar Artefacto" ++ nombre reliquia]}

escalarOlimpo :: Tarea -- Heroe -> Heroe
escalarOlimpo h
    = h {reconocimiento = reconocimiento h + 500,
    artefactos = ((++ [relampagoZeus]) . removerComunes . map triplicarRareza . artefactos) h,
    tareasRealizadas = tareasRealizadas h ++ ["Escalar el Olimpo"]}

ayudarCruzarCalle :: Int -> Tarea -- Heroe -> Heroe
ayudarCruzarCalle calles h
    = h {epiteto = "Gros" ++ take calles (repeat 'o'),
    tareasRealizadas = tareasRealizadas h ++ ["Ayude a cruzar " ++ (show calles) ++ " calles"]}

matarBestia :: Bestia -> Tarea
matarBestia bicho heroe
    | vence heroe bicho = heroe {epiteto = "El asesino de" ++ nombreBestia bicho} 
    | otherwise = heroe {epiteto = "El cobarde", tareasRealizadas = (tail . tareasRealizadas) heroe } 

matarLeonNemea :: Tarea
matarLeonNemea = matarBestia leonNemea

realizarTarea :: Tarea -> Heroe -> Heroe
realizarTarea tarea = tarea

presumirLogros :: Heroe -> Heroe -> (Heroe, Heroe)
presumirLogros h1 h2 
    | (vencedorReconocimiento h1 h2) == h1 = (h1,h2)
    | otherwise = (h2,h1)

realizarLabor :: Heroe -> Labor -> Heroe
realizarLabor heroe = foldl (\h tarea -> tarea h) heroe

--Se puede utilizar una lista de baroes infinita siempre y cuando utilicemos una función que corte la toma de elementos, como take o takewhile