import List

--Definición de Grafo--
data Grafo a = G {nodos :: [a], adyacencias :: a->[a]}

--Funciones adicionales (pueden agregar otras)--

conjuntosIguales::Eq a =>[a]->[a]->Bool
conjuntosIguales xs ys = (all (flip elem xs) ys) && (all (flip elem ys) xs)

sacarDeLista::Eq a => a -> [a] -> [a]
sacarDeLista x ys = filter (`notElem` [x]) ys

estanLosNodos::Eq a => Grafo a -> [a] -> Bool
estanLosNodos grafo ns = all (`elem` (nodos grafo)) ns

instance Eq a =>Eq (Grafo a) where
  (==) g1 g2 = (conjuntosIguales (nodos g1) (nodos g2)) && (all (\x->conjuntosIguales (adyacencias g1 x) (adyacencias g2 x)) (nodos g1))

instance Show a =>Show (Grafo a) where
	show (G nodos adyacencias) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (adyacencias x) ++ "\n") nodos) ++ "]"

predecesores::Eq a => Grafo a -> a -> [a]
predecesores grafo nodo = [ nodo_origen | nodo_origen <- nodos grafo, elem nodo (adyacencias grafo nodo_origen)]

sucesores::Grafo a -> a -> [a] --Es un renombre de adyacencias
sucesores grafo nodo = adyacencias grafo nodo 

adyacenciasNoDirigidas::Eq a => Grafo a -> a -> [a]
adyacenciasNoDirigidas grafo nodo = sucesores grafo nodo ++ predecesores grafo nodo

--Funciones pedidas--

agregarNodo::Eq a => Grafo a -> a ->Grafo a
agregarNodo grafo nodo = if (not (estanLosNodos grafo [nodo]))
						 then G (nodo : nodos grafo) (\x -> if x == nodo then [] else adyacencias grafo x)
						 else grafo
							
agregarEje::Eq a => Grafo a -> a -> a -> Grafo a
agregarEje grafo nodo1 nodo2 = 	if (estanLosNodos grafo [nodo1,nodo2] && notElem nodo2 (adyacencias grafo nodo1))
								then G (nodos grafo) (\x -> if x == nodo1 then nodo2 : (adyacencias grafo nodo1) else adyacencias grafo x)
								else grafo

sacarEje::Eq a => Grafo a -> a -> a -> Grafo a
sacarEje grafo nodo1 nodo2 = 	if (estanLosNodos grafo [nodo1,nodo2] && elem nodo2 (adyacencias grafo nodo1))
								then G (nodos grafo) (\x -> if x == nodo1 then sacarDeLista nodo2 (adyacencias grafo nodo1) else adyacencias grafo x)
								else grafo
								
sacarNodo::Eq a => Grafo a -> a -> Grafo a
sacarNodo grafo nodo = 	if (estanLosNodos grafo [nodo]) 
						then G (sacarDeLista nodo (nodos grafo)) (\x -> if elem nodo (adyacencias grafo x) then adyacencias (sacarEje grafo x nodo) x else adyacencias grafo x)
						else grafo

grado::Eq a =>Grafo a -> a -> Int
grado grafo nodo = gradoOut grafo nodo + gradoIn grafo nodo

gradoOut::Grafo a -> a -> Int
gradoOut grafo nodo = length (adyacencias grafo nodo)

gradoIn::Eq a => Grafo a -> a -> Int
gradoIn grafo nodo = length (predecesores grafo nodo)

maximoGrado::Eq a => Grafo a -> Int
maximoGrado grafo = maximum (map (grado grafo) (nodos grafo))

diferencia::Eq a => [a] -> [a] -> [a]
diferencia lista1 lista2 = [ x| x <- lista1, notElem x lista2] --A la lista1 le saco los que aparecen en la lista2. Si la lista1 tiene repetidos, estos quedan. ¿está bien o está mal?

esSubgrafo::Eq a => Grafo a -> Grafo a -> Bool
esSubgrafo grafo1 grafo2 = 	((estanLosNodos grafo2 (nodos grafo1))
							&&
							(all (== True) [conjuntoIncluido  (adyacencias grafo1 n) (adyacencias grafo2 n) | n <- nodos grafo1]))


conjuntoIncluido::Eq a => [a] -> [a] -> Bool
conjuntoIncluido xs ys = null (diferencia xs ys)

subgrafoInducido::Eq a => Grafo a -> [a] -> Grafo a
subgrafoInducido grafo nodos = if (estanLosNodos grafo nodos)
									then G nodos (\x -> if elem x nodos then intersect nodos (adyacencias grafo x) else error "El nodo no pertenece")
									else error "el conjunto de nodos no es subconjunto de los nodos del grafo"
-- No usé la función diferencia, como recomienda en el ejercicio. Tal vez me estoy perdiendo de algo

enCiclo::Eq a => Grafo a -> a -> Bool
enCiclo grafo nodo = if estanLosNodos grafo [nodo] 
							then chequearTerminacion nodo (iterate sacarNodosDeGradoMenorADos grafo)
							else error "el nodo no pertenece al grafo"

chequearTerminacion:: Eq a => a -> [Grafo a] -> Bool
chequearTerminacion nodo (x:y:xs) = if (x == y) then (estanLosNodos x [nodo]) else chequearTerminacion nodo (y:xs)
--No me gusta el nombre de esta función, pero no se bien como llamarla

sacarNodosDeGradoMenorADos::Eq a => Grafo a -> Grafo a
sacarNodosDeGradoMenorADos grafo = subgrafoInducido grafo (filter (\nodo -> grado grafo nodo >= 2) (nodos grafo))

conexo::Eq a => Grafo a -> Bool
conexo grafo = if null (nodos grafo) 
				then True
				else conexoDesde grafo [head (nodos grafo)]

conexoDesde::Eq a => Grafo a -> [a] -> Bool
conexoDesde grafo marcados = if (length marcados == length (nodos grafo)) 
									then True
									else if conjuntosIguales marcados (agrandarMarcados grafo marcados) 
											then False
											else conexoDesde grafo (agrandarMarcados grafo marcados)

agrandarMarcados::Eq a => Grafo a -> [a] -> [a]
agrandarMarcados grafo marcados = union marcados [vecino | x <- marcados, vecino <- adyacenciasNoDirigidas grafo x ]
-- Tengo un conjunto de nodos marcados. Itero sobre cada uno y marco los vecinos, agrandando (o manteniendo igual) al conjunto.

--Grafos de prueba--
grafo1::Grafo Char
grafo1 = G ['a','b','c'] ady1
	  where
		ady1 'a' = "bc"
		ady1 'b' = "c"
		ady1 'c' = ""
		ady1 x = error "El nodo "++(x:" no pertenece al grafo")

grafo2::Grafo Char
grafo2 = G ['a','c'] ady2
	  where
		ady2 'a' = ['c']
		ady2 'c' = []
		ady2 x = error "El nodo "++(x:" no pertenece al grafo")
		
grafo3::Grafo Int
grafo3 = G [1,2,3,4,5] (\x->filter (<= 5) [2*x,2*x+1])

grafo4::Grafo Int
grafo4 = agregarEje grafo3 5 1

grafo5::Grafo Int
grafo5 = agregarEje grafo3 1 5

grafo6::Grafo Int
grafo6 = G (nodos grafo5) (adyacenciasNoDirigidas grafo5)

grafo7::Grafo Int
grafo7 = agregarNodo grafo3 0

grafo8::Grafo Int
grafo8 = agregarNodo grafo5 0