module Lib () where
pancho :: Persona
pancho = ( 40, 120, 1)

andres :: Persona
andres = ( 22, 80, 6)

--Punto 1:
estaSaludable :: Persona -> Bool
estaSaludable unaPersona = (not . esObesa) unaPersona && ((>5). tonificacion) unaPersona

esObesa :: Persona -> Bool
esObesa = ((> 100) . peso)

peso :: Persona -> Float
peso (_, peso, _) = peso

tonificacion :: Persona -> Int
tonificacion (_,_, tonificacion) = tonificacion

edad :: Persona -> Float
edad (edad, _ ,_) = edad

type Persona = (Float, Float, Int)
--También se podría hacer directamente por pattern matching

--Punto 2:
type Calorias = Float

bajarDePeso :: Calorias -> Persona -> Persona
bajarDePeso unasCalorias unaPersona
  | esObesa unaPersona                             = reducirPeso (unasCalorias / 150) unaPersona
  | tieneMasDe 30 unaPersona && unasCalorias > 200 = reducirPeso 1 unaPersona
  | otherwise                                      = reducirPeso (unasCalorias / ((peso unaPersona) * (edad unaPersona)) ) unaPersona

tieneMasDe :: Float -> Persona -> Bool
tieneMasDe unaEdad = ((> unaEdad) . edad)

reducirPeso :: Float -> Persona -> Persona
reducirPeso unaCantidad = modificarPeso (subtract unaCantidad)

modificarPeso :: (Float -> Float) -> Persona -> Persona
modificarPeso modificacion (unaEdad, unPeso, unaTonificacion) = (unaEdad, modificacion unPeso, unaTonificacion)

--También se podría hacer así (Me parece menos clara, pero se repite menos código)

bajarDePesoV2 :: Calorias -> Persona -> Persona
bajarDePesoV2 unasCalorias unaPersona = (flip reducirPeso unaPersona . calcularPesoSegunCalorias unasCalorias) unaPersona

calcularPesoSegunCalorias :: Calorias -> Persona -> Float
calcularPesoSegunCalorias unasCalorias unaPersona
  | esObesa unaPersona                             = (unasCalorias / 150)
  | tieneMasDe 30 unaPersona && unasCalorias > 200 =  1
  | otherwise                                      = (unasCalorias / (peso unaPersona) * (edad unaPersona))

--Punto 3:
--a
type Ejercicio = Float -> Persona -> Persona
type VelocidadPromedio = Float

cinta :: VelocidadPromedio -> Ejercicio
cinta velocidadPromedio unosMinutos = bajarDePeso (unosMinutos * velocidadPromedio)

caminata :: Ejercicio
caminata = cinta 5

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta unosMinutos = cinta (velocidadPromedio (velocidadMaxima 6 unosMinutos) 6) unosMinutos

velocidadMaxima :: Float -> Float -> Float
velocidadMaxima velocidadInicial unTiempo = velocidadInicial + (unTiempo / 5)

velocidadPromedio :: (Fractional a) => a -> a -> a
velocidadPromedio unValor otroValor = (unValor + otroValor) / 2 --Da casi igual a [6 .. velocidadMaxima] / length de la lista

--b
pesas :: Float -> Ejercicio
pesas unosKilos unosMinutos unaPersona
  | unosMinutos > 10 = bajarDePeso (unosKilos * 0.1) unaPersona
  | otherwise     = unaPersona

--c
colina :: Float -> Ejercicio
colina unaInclinacion unosMinutos = bajarDePeso (2* unosMinutos * unaInclinacion)

--d
montaña :: Float -> Ejercicio
montaña unaInclinacion unTiempo = (aumentarTonificacion 1 . sucesionDeColinas unaInclinacion (unTiempo/2))

sucesionDeColinas :: Float -> Float -> Persona -> Persona
sucesionDeColinas unaInclinacion mitadDeTiempo = (colina (unaInclinacion + 3) mitadDeTiempo . colina unaInclinacion mitadDeTiempo)

aumentarTonificacion :: Int -> Persona -> Persona
aumentarTonificacion unaCantidad = modificarTonificacion (+ unaCantidad)

modificarTonificacion :: (Int -> Int) -> Persona -> Persona
modificarTonificacion modificacion (unaEdad, unPeso, unaTonificacion) = (unaEdad, unPeso, modificacion unaTonificacion)

--Punto 4:
--a
type Rutina = (String, Float, [Ejercicio])

hacerRutina :: Rutina -> Persona -> Persona
hacerRutina (_, unTiempo, unosEjercicios) unaPersona = foldr (flip ($) (tiempoPorEjercicio unTiempo unosEjercicios)) unaPersona unosEjercicios

tiempoPorEjercicio :: Float -> [Ejercicio] -> Float
tiempoPorEjercicio unTiempo = ((unTiempo/) . fromIntegral .length)

--Con recursividad:
hacerRutinaRecursiva :: Rutina -> Persona -> Persona
hacerRutinaRecursiva (unNombre , unTiempo, []) unaPersona = unaPersona
hacerRutinaRecursiva (unNombre , unTiempo, (unEjercicio : restoDeEjercicios)) unaPersona = hacerRutinaRecursiva (unNombre, unTiempo, restoDeEjercicios) (unEjercicio (tiempoPorEjercicio unTiempo restoDeEjercicios) unaPersona)

ejemploDeUso :: Persona
ejemploDeUso = hacerRutina ("ejemplo", 500, [caminata, entrenamientoEnCinta, pesas 5, colina 10, montaña 10]) pancho

--b
type Resumen = (String, Float, Int)

resumenDeRutina :: Rutina -> Persona -> Resumen
resumenDeRutina unaRutina unaPersona = (construirResumen (nombre unaRutina) unaPersona . hacerRutina unaRutina) unaPersona

construirResumen :: String -> Persona -> Persona -> Resumen
construirResumen unNombre unaPersona otraPersona = (unNombre, deltaSegun peso unaPersona otraPersona, deltaSegun tonificacion unaPersona otraPersona)

deltaSegun :: (Num a)=>(Persona -> a) -> Persona -> Persona -> a
deltaSegun transformador estadoFinalUnaPersona estadoInicialUnaPersona = transformador estadoFinalUnaPersona - transformador estadoInicialUnaPersona

nombre :: Rutina -> String
nombre (unNombre, _ ,_) = unNombre
