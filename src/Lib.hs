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

edad :: Persona -> Int
edad (edad, _ ,_) = edad

type Persona = (Int, Float, Int)
--También se podría hacer directamente por pattern matching

--Punto 2:
type Calorias = Int
bajarDePeso :: Calorias
