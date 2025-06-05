module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Samurai

type Habilidad = Number
type Maestro = Guerrero
type Nombre = String

data Guerrero = UnGuerrero { 
    nombre :: String, 
    habilidad :: Habilidad,
    caracteristicas :: [String] 
}

habilidoso :: Habilidad -> Guerrero -> Bool
habilidoso minimo guerrero = habilidad guerrero >= minimo

honorable:: Guerrero -> Bool
honorable guerrero = not (elem "deshonra" (caracteristicas guerrero))

guerrero1 :: Guerrero
guerrero1 = UnGuerrero {
    nombre = "Carlos",
    habilidad = 8,
    caracteristicas = ["es crack", "es habilidoso"]
}

guerrero2 :: Guerrero
guerrero2 = UnGuerrero {
    nombre = "Camilo",
    habilidad = 3,
    caracteristicas = ["honorable", "no es habilidoso"]
}

guerrero3 :: Guerrero
guerrero3 = UnGuerrero {
    nombre = "Paco",
    habilidad = 1,
    caracteristicas = ["deshonra"]
}

maestroHabilidoso :: Maestro
maestroHabilidoso = UnGuerrero{
    nombre = "Jose",
    habilidad = 8,
    caracteristicas = ["gran maestro"]
}

maestroHonorable :: Maestro
maestroHonorable = UnGuerrero{
    nombre = "Romualdo",
    habilidad = 5,
    caracteristicas = ["muy honorable"]
}

reclutas :: Maestro -> [Guerrero] -> [Nombre]
reclutas maestro guerreros
    | habilidoso 7 maestro = map nombre (filter (habilidoso 7) guerreros)
    | otherwise = map nombre (filter honorable guerreros)

honorable' :: Guerrero -> Bool
honorable' = not . (elem "deshonra") . caracteristicas

-- Videojuegos

type Preferencia = Videojuego -> Bool

data Videojuego = UnVideojuego {
    titulo :: String,
    desarrolladora :: String,
    generos :: [String],
    lanzamiento :: Number
}

data Jugador = UnJugador{
    nombre' :: String,
    prefiere :: [Preferencia]
}

juan :: Jugador
juan = UnJugador{
    nombre' = "Juan",
    prefiere = [(esDesarrolladoPor "Nintendo"), (perteneceAGenero "plataformas")]  
}

maria :: Jugador
maria = UnJugador{
    nombre' = "Maria",
    prefiere = [esReciente]
}

pedro :: Jugador
pedro = UnJugador{
    nombre' = "Pedro",
    prefiere = [(esDesarrolladoPor "Rockstar Games"), (perteneceAGenero "mundo abierto")]
}

marioBros :: Videojuego
marioBros = UnVideojuego{
    titulo = "Mario Bros",
    desarrolladora = "Nintendo",
    generos = ["plataformas", "mundo abierto"],
    lanzamiento = 2018
}

esDesarrolladoPor :: String -> Videojuego -> Bool
esDesarrolladoPor dev = (==dev).desarrolladora

perteneceAGenero :: String -> Videojuego -> Bool 
perteneceAGenero genero videojuego = elem genero (generos videojuego)

esReciente :: Videojuego -> Bool
esReciente videojuego = (lanzamiento videojuego) > 2015

preferidosDelComite