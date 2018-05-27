import Data.List

data Jugador = Jugador { nombre :: String, edad :: Int, promedioGol :: Float, habilidad :: Int, cansancio :: Float } deriving(Show)

martin = Jugador "Martin" 26 0.0 50 35.0
juan = Jugador "Juancho" 30 0.2 50 40.0
maxi = Jugador "Maxi Lopez" 27 0.4 68 30.0

jonathan = Jugador "Chueco" 20 1.5 80 99.0
lean = Jugador "Hacha" 23 0.01 50 35.0
brian = Jugador "Panadero" 21 5 80 15.0

garcia = Jugador "Sargento" 30 1 80 13.0
messi = Jugador "Pulga" 26 10 99 43.0
aguero = Jugador "Aguero" 24 5 90 5.0

type Equipo = (NombreEquipo,Grupo,Jugadores)
type NombreEquipo = String
type Grupo = Char
type Jugadores = [Jugador]

nombreEquipo (nombreEquipo,_,_) = nombreEquipo
grupo (_,grupo,_) = grupo
jugadores (_,_,jugadores) = jugadores

equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])

-- EJERCICIO 1
figurasDeEquipo :: Equipo -> Jugadores
figurasDeEquipo = filter esFigura.jugadores

esFigura :: Jugador -> Bool
esFigura unJugador = ((>75).habilidad) unJugador && ((>0).promedioGol) unJugador

-- EJERCICIO 2
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero :: Equipo -> Bool
tieneFarandulero = any esJugadorFarandulero.jugadores

esJugadorFarandulero :: Jugador -> Bool
esJugadorFarandulero unJugador = elem (nombre unJugador) jugadoresFaranduleros

-- EJERCICIO 3
figuritasDificiles :: Char -> [Equipo] -> Jugadores
figuritasDificiles grupoEspecifico = obtenerJovenesNoFaranduleros.obtenerFiguras.filtradoSegunGrupo grupoEspecifico

filtradoSegunGrupo :: Char -> [Equipo] -> [Equipo]
filtradoSegunGrupo grupoEspecifico = filter (\equipo-> ((== grupoEspecifico).grupo) equipo)
 
obtenerFiguras :: [Equipo] -> Jugadores
obtenerFiguras = concat.map figurasDeEquipo

obtenerJovenesNoFaranduleros :: Jugadores -> Jugadores
obtenerJovenesNoFaranduleros = filter (\unJugador -> esJoven unJugador && (not.esJugadorFarandulero) unJugador) 

esJoven :: Jugador -> Bool
esJoven = (<27).edad

-- EJERCICIO 4
jugarPartido :: Equipo -> Equipo
jugarPartido unEquipo = ( nombreEquipo unEquipo , grupo unEquipo , map modificarCansancio (jugadores unEquipo) )

modificarCansancio :: Jugador -> Jugador
modificarCansancio unJugador 
 | (not.esJugadorFarandulero) unJugador && esJoven unJugador && esFigura unJugador = cambiarCansancio ((+50).(*0)) unJugador
 | esJoven unJugador = cambiarCansancio (+ (0.1 * cansancio unJugador)) unJugador
 | (not.esJoven) unJugador && esJugadorFarandulero unJugador = cambiarCansancio (+20) unJugador
 | otherwise = cambiarCansancio (*2) unJugador

cambiarCansancio :: (Float->Float) -> Jugador -> Jugador
cambiarCansancio funcion unJugador = unJugador { cansancio = (funcion.cansancio) unJugador }

-- EJERCICIO 5 
ganadorDelPartido :: Equipo -> Equipo -> Equipo
ganadorDelPartido primerEquipo segundoEquipo 
 | total primerEquipo > total segundoEquipo = jugarPartido primerEquipo
 | otherwise = jugarPartido segundoEquipo

total :: Equipo -> Float
total = sumarPromedioGol.elegir11Jugadores 

quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs 

elegir11Jugadores :: Equipo -> Jugadores
elegir11Jugadores = take 11.jugadoresMenosCansados

jugadoresMenosCansados :: Equipo -> Jugadores
jugadoresMenosCansados = quickSort (\jugador1 jugador2 -> cansancio jugador1 < cansancio jugador2).jugadores

sumarPromedioGol :: Jugadores -> Float
sumarPromedioGol = sum.map promedioGol

-- EJERCICIO 6
campeonDelTorneo :: [Equipo] -> Equipo
campeonDelTorneo listaEquipos = foldl1 ganadorDelPartido listaEquipos

ganadorDelTorneo :: [Equipo] -> Equipo
ganadorDelTorneo [x] = x
ganadorDelTorneo (x:xs)
 | (nombreEquipo.ganadorDelPartido x.head) xs == nombreEquipo x = ganadorDelTorneo (x: (tail xs))
 | otherwise = ganadorDelTorneo xs

-- EJERCICIO 7	
elGroso :: [Equipo] -> String
elGroso = primerJugador.ganadorDelTorneo

primerJugador :: Equipo -> String
primerJugador = nombre.head.filter esFigura.jugadores

--primerJugador :: Equipo -> String
--primerJugador = find esFigura.jugadores