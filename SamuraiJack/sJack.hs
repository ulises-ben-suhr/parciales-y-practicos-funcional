data Elemento = UnElemento {
    tipo :: String,
    ataque :: Personaje -> Personaje,
    defensa :: Personaje -> Personaje
}

data Personaje = UnPersonaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int
} --deriving (Show, Eq)



-- Punto 1
-- 1 A

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anioDestino persona = persona {anioPresente = anioDestino}

-- 1 BC
modificarSalud :: Float -> Personaje -> Personaje
modificarSalud saludNueva persona = persona {salud = max 0 (salud persona + saludNueva)}

-- 1 B
meditar :: Personaje -> Personaje
meditar persona = modificarSalud (salud persona / 2) persona

-- 1 C
causarDanio :: Float -> Personaje -> Personaje
causarDanio danio = modificarSalud (negate danio) 


-- Punto 2
-- 2 A

esMalvado :: Personaje -> Bool
esMalvado = any ((== "Maldad") . tipo) . elementos

-- 2 B
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce persona elemento = salud persona - (salud . (($ persona) . ataque)) elemento

-- 2 C
danioMortal :: Personaje -> Personaje -> Bool
danioMortal persona = any ((== salud persona) . danioQueProduce persona) . elementos

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales persona = filter (danioMortal persona)

ulises = UnPersonaje "ulises" 100 [] 2022

laVida = UnPersonaje "La Vida" 1000 [mondongo, trabajo, madrugar] 2022

mondongo = UnElemento "mondongo" (causarDanio 40) meditar

trabajo = UnElemento "trabajo" (causarDanio 60) meditar

madrugar = UnElemento "madrugar" (causarDanio 80) meditar



-- PUNTO 3
-- 3 A

-- concentracion :: Int -> Elemento
-- concentracion nivel = UnElemento "Magia" 

