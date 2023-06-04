module Lib () where

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
