import Grafo
import HUnit

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [ 
	"grafo" ~: testsGrafo
	]

-- INVARIANTE DE LOS GRAFOS
-- no pueden tener nodos repetidos, 
-- no debe tener lazos - ejes de un nodo a sí mismo - 
-- y todos los ejes deben apuntar a nodos que estén en en grafo

testsGrafo = test [
	-- Tests agregarNodo
	--un nodo
	[1] ~~? (nodos (agregarNodo 1 grafoVacio)),
	--dos nodos
	[1,2] ~~? (nodos (agregarNodo 2 (agregarNodo 1 grafoVacio))),
	--nodo repetido
	[1] ~~? (nodos (agregarNodo 1 (agregarNodo 1 grafoVacio))),
	
	-- Tests agregarEje
	--lazo
	[] ~~? (adyacencias (agregarEje 1 1(agregarNodo 1 grafoVacio)) 1),
	--una conexion
	[2] ~~? (adyacencias (agregarEje 1 2 (agregarNodo 2 (agregarNodo 1 grafoVacio))) 1),
	--dos conexiones
	[2,3] ~~? (adyacencias (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio))))) 1),
	--eje repetido
	[2] ~~? (adyacencias (agregarEje 1 2 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio))))) 1),
	--nodo inexistente
	[] ~~? (adyacencias (agregarEje 1 2 (agregarNodo 1 grafoVacio)) 1),
	
	-- Tests sacarEjes
	--un eje
	[] ~~? (adyacencias (sacarEje 1 2 (agregarEje 1 2 (agregarNodo 2 (agregarNodo 1 grafoVacio)))) 1),
	--un eje inexistente (un extremo no existe en el grafo)
	[2] ~~? (adyacencias (sacarEje 1 3 (agregarEje 1 2 (agregarNodo 2 (agregarNodo 1 grafoVacio)))) 1),
	--otro eje inexistente (ambos extremos no existen en el grafo)
	[2] ~~? (adyacencias (sacarEje 3 4 (agregarEje 1 2 (agregarNodo 2 (agregarNodo 1 grafoVacio)))) 1),
	--otro eje inexistente (ambos nodos pertenecen al grafo)
	[2] ~~? (adyacencias (sacarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio))))) 1),
	--un eje simulando un lazo (nunca se podría crear desde un principio)
	[2] ~~? (adyacencias (sacarEje 1 1 (agregarEje 1 2 (agregarNodo 2 (agregarNodo 1 grafoVacio)))) 1),
	
	-- Tests sacarNodo
	--nodo existente
	[2] ~~? (nodos (sacarNodo 1 (agregarNodo 2 (agregarNodo 1 grafoVacio)))),
	--nodo inexistente
	[1,2] ~~? (nodos (sacarNodo 4 (agregarNodo 2 (agregarNodo 1 grafoVacio)))),
	--dos veces el mismo nodo
	[2] ~~? (nodos (sacarNodo 1 (sacarNodo 1 (agregarNodo 2 (agregarNodo 1 grafoVacio))))),
	--verificando que se eliminen los ejes asociados al nodo eliminado
	[] ~~? (adyacencias (sacarNodo 1 (agregarEje 1 2 (agregarNodo 2 (agregarNodo 1 grafoVacio)))) 2),
	[3] ~~? (adyacencias (sacarNodo 2 (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio)))))) 1),
	[] ~~? (adyacencias (sacarNodo 2 (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio)))))) 3),
	
	-- Tests grado
	--de un nodo existente
	2 ~=? (grado 1 (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio)))))),
	--de un nodo inexistente
	--(grado 1 (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio)))))) --> Lanza excepcion
	
	-- Tests maximoGrado
	--de un grafo vacio
	0 ~=? (maximoGrado grafoVacio),
	--de un grafo con nodos pero sin conexiones
	0 ~=? (maximoGrado (agregarNodo 1 grafoVacio)),
	--de un grafo con nodos y conexiones
	2 ~=? (maximoGrado (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio)))))),
	
	-- Tests esSubgrafo
	--el grafo vacio es subgrafo de todos
	True ~=? (esSubgrafo grafoVacio grafoVacio),
	True ~=? (esSubgrafo grafoVacio (agregarNodo 1 grafoVacio)),
	True ~=? (esSubgrafo grafoVacio (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio)))))),
	--nadie es subgrafo del grafo vacio excepto el mismo grafo vacio
	False ~=? (esSubgrafo (agregarNodo 1 grafoVacio) grafoVacio),
	False ~=? (esSubgrafo (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio))))) grafoVacio),
	--invirtiendo uno de los ejes
	False ~=? (esSubgrafo (agregarEje 3 1 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio))))) (agregarEje 1 3 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio)))))),
	--sacando un nodo
	True ~=? (esSubgrafo (agregarEje 1 2 (agregarNodo 2 (agregarNodo 1 grafoVacio))) (agregarEje 3 1 (agregarEje 1 2 (agregarNodo 3 (agregarNodo 2 (agregarNodo 1 grafoVacio))))))
	]
	
---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
