import List

--Definición de Grafo--
data Grafo a = G {nodos :: [a], adyacencias :: a->[a]}

--Funciones adicionales (pueden agregar otras)--

conjuntosIguales::Eq a =>[a]->[a]->Bool
conjuntosIguales xs ys = (all (flip elem xs) ys) && (all (flip elem ys) xs)

instance Eq a =>Eq (Grafo a) where
  (==) g1 g2 = (conjuntosIguales (nodos g1) (nodos g2)) && (all (\x->conjuntosIguales (adyacencias g1 x) (adyacencias g2 x)) (nodos g1))

--Funciones pedidas--

agregarNodo::
		     
agregarEje::
					  
sacarEje::

sacarNodo::

grado::

máximoGrado::

esSubgrafo::

diferencia::

subgrafoInducido::

enCiclo::
	
conexo::

esÁrbol::

------------------------
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
grafo4 = agregarEje grafo3 (5,1)

grafo5::Grafo Int
grafo5 = agregarEje grafo3 (1,5)

grafo6::Grafo Int
grafo6 = G (nodos grafo5) (adyacenciasNoDirigidas grafo5)

grafo7::Grafo Int
grafo7 = agregarNodo grafo3 0

grafo8::Grafo Int
grafo8 = agregarNodo grafo5 0

--Agreguen sus propios grafos y pruebas.