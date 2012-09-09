module Grafo where

import List

--Definición de Grafo--
data Grafo a = G {nodos :: [a], adyacencias :: a->[a]}

--Funciones adicionales (pueden agregar otras)--

conjuntosIguales::Eq a =>[a]->[a]->Bool
conjuntosIguales xs ys = (all (flip elem xs) ys) && (all (flip elem ys) xs)

instance Eq a =>Eq (Grafo a) where
  (==) g1 g2 = (conjuntosIguales (nodos g1) (nodos g2)) && (all (\x->conjuntosIguales (adyacencias g1 x) (adyacencias g2 x)) (nodos g1))

instance Show a =>Show (Grafo a) where
	show (G nodos adyacencias) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (adyacencias x) ++ "\n") nodos) ++ "]"

sacarDeLista::Eq a => a -> [a] -> [a]
sacarDeLista x ys = filter (`notElem` [x]) ys

estanLosNodos::Eq a => [a] -> Grafo a -> Bool
estanLosNodos ns grafo = all (`elem` (nodos grafo)) ns

predecesores::Eq a => a -> Grafo a -> [a]
predecesores nodo grafo = (filter (\nodo_origen -> elem nodo (adyacencias grafo nodo_origen)) (nodos grafo))

sucesores:: a -> Grafo a -> [a] --Es un renombre de adyacencias
sucesores = flip adyacencias

adyacenciasNoDirigidas::Eq a =>Grafo a -> a  -> [a]
adyacenciasNoDirigidas grafo nodo = sucesores nodo grafo ++ predecesores nodo grafo

--Funciones pedidas--

-- Ej1
agregarNodo::Eq a => a -> Grafo a -> Grafo a
agregarNodo nodo grafo = if (not (estanLosNodos [nodo] grafo))
						 then G (nodo : nodos grafo) (\x -> if x == nodo then [] else adyacencias grafo x)
						 else grafo

-- Ej2						 
agregarEje::Eq a => a -> a -> Grafo a -> Grafo a
agregarEje nodo1 nodo2 grafo = 	if not(nodo1 == nodo2) && (estanLosNodos [nodo1,nodo2] grafo && notElem nodo2 (adyacencias grafo nodo1))
								then G (nodos grafo) (\x -> if x == nodo1 then nodo2 : (adyacencias grafo nodo1) else adyacencias grafo x)
								else grafo

-- Ej3
sacarEje::Eq a => a -> a -> Grafo a -> Grafo a
sacarEje nodo1 nodo2 grafo = 	if (estanLosNodos [nodo1,nodo2] grafo && elem nodo2 (adyacencias grafo nodo1))
								then G (nodos grafo) (\x -> if x == nodo1 then sacarDeLista nodo2 (adyacencias grafo nodo1) else adyacencias grafo x)
								else grafo

-- Ej4
sacarNodo::Eq a => a -> Grafo a -> Grafo a
sacarNodo nodo grafo = 	if (estanLosNodos [nodo] grafo) 
						then G (sacarDeLista nodo (nodos grafo)) (\x -> if elem nodo (adyacencias grafo x) then adyacencias (sacarEje x nodo grafo) x else adyacencias grafo x)
						else grafo

-- Ej5 a)
grado::Eq a => a -> Grafo a -> Int
grado nodo grafo = 	if (estanLosNodos [nodo] grafo)
					then gradoOut nodo grafo + gradoIn nodo grafo
					else error "El nodo no pertenece al grafo"

gradoOut:: a -> Grafo a -> Int
gradoOut nodo grafo = length (adyacencias grafo nodo)

gradoIn::Eq a => a -> Grafo a -> Int
gradoIn nodo grafo = length (predecesores nodo grafo)

-- Ej5 b)
maximoGrado::Eq a => Grafo a -> Int
maximoGrado grafo = if length (nodos grafo) > 0 
					then maximum (map (flip grado grafo) (nodos grafo)) -- maximum no esta definido para listas vacias, asi que fallaba si el grafo no tenia nodos. Por eso el chequeo 
					else 0

-- Ej6 a)
diferencia::Eq a => [a] -> [a] -> [a]
diferencia lista1 lista2 = filter (`notElem` lista2) lista1 --A la lista1 le saco los que aparecen en la lista2. Si la lista1 tiene repetidos, estos quedan. ¿está bien o está mal?

-- Ej6 b)
esSubgrafo::Eq a => Grafo a -> Grafo a -> Bool
esSubgrafo grafo1 grafo2 = 	(estanLosNodos (nodos grafo1) grafo2)
							&&
							(all (== True) (map (\n -> conjuntoIncluido  (adyacencias grafo1 n) (adyacencias grafo2 n)) (nodos grafo1)))


conjuntoIncluido::Eq a => [a] -> [a] -> Bool
conjuntoIncluido xs ys = null (diferencia xs ys)

-- Ej7
subgrafoInducido::Eq a => Grafo a -> [a] -> Grafo a
subgrafoInducido grafo nodos = if (estanLosNodos nodos grafo)
									then G nodos (\x -> if elem x nodos then intersect nodos (adyacencias grafo x) else error "El nodo no pertenece")
									else error "el conjunto de nodos no es subconjunto de los nodos del grafo"
-- No usé la función diferencia, como recomienda en el ejercicio. Tal vez me estoy perdiendo de algo

-- Ej8
enCiclo::Eq a => Grafo a -> Bool
enCiclo grafo = chequearTerminacion (iterate sacarNodosDeGradoMenorADos grafo)

chequearTerminacion::Eq a => [Grafo a] -> Bool
chequearTerminacion (x:xs) = if (x == head xs) then length (nodos x) > 1 else chequearTerminacion xs

sacarNodosDeGradoMenorADos::Eq a => Grafo a -> Grafo a
sacarNodosDeGradoMenorADos grafo = subgrafoInducido grafo (filter (\nodo -> grado nodo grafo >= 2) (nodos grafo))

-- Ej9
conexo::Eq a => Grafo a -> Bool
conexo grafo = if null (nodos grafo) 
				then True
				else conexoDesde grafo [head (nodos grafo)]

conexoDesde::Eq a => Grafo a -> [a] -> Bool
conexoDesde grafo marcados = if conjuntosIguales marcados (nodos grafo)
									then True
									else if conjuntosIguales marcados (agrandarMarcados grafo marcados) 
											then False
											else conexoDesde grafo (agrandarMarcados grafo marcados)

agrandarMarcados::Eq a => Grafo a -> [a] -> [a]
agrandarMarcados grafo marcados = union marcados (concat (map (\x -> adyacenciasNoDirigidas grafo x) marcados))
-- Tengo un conjunto de nodos marcados. Itero sobre cada uno y marco los vecinos, agrandando (o manteniendo igual) al conjunto.

-- Ej10
esUnArbol::Eq a => Grafo a -> Bool
esUnArbol grafo = not (enCiclo grafo) && (conexo grafo)

--Grafos de prueba--
grafo1::Grafo Char
grafo1 = G ['a','b','c'] ady1
	  where
		ady1 'a' = "bc"
		ady1 'b' = "c"
		ady1 'c' = ""
		ady1 x = error ("El nodo "++(x:" no pertenece al grafo"))

grafo2::Grafo Char
grafo2 = G ['a','c'] ady2
	  where
		ady2 'a' = ['c']
		ady2 'c' = []
		ady2 x = error ("El nodo "++(x:" no pertenece al grafo"))
		
grafo3::Grafo Int
grafo3 = G [1,2,3,4,5] (\x->filter (<= 5) [2*x,2*x+1])

grafo4::Grafo Int
grafo4 = agregarEje 5 1 grafo3

grafo5::Grafo Int
grafo5 = agregarEje 1 5 grafo3

grafo6::Grafo Int
grafo6 = G (nodos grafo5) (adyacenciasNoDirigidas grafo5)

grafo7::Grafo Int
grafo7 = agregarNodo 0 grafo3

grafo8::Grafo Int
grafo8 = agregarNodo 0 grafo5

grafoVacio::Grafo Int
grafoVacio = G [] (\x -> [])

grafoConCiclo::Grafo Int
grafoConCiclo = agregarEje 5 4 (agregarEje 4 5 (agregarEje 3 4 (agregarEje 2 3 (agregarEje 2 1 (agregarEje 1 2 (agregarNodo 5 (agregarNodo 4 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio))))))))))