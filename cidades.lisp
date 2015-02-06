(defconstant goal 'faro)

(defstruct city
	state 
	father
	gn
	hn
	fn
	dn
)

(defun make-cidade (nome pai distancia heuristica)
	(let ((result nil))
		(if (eql pai nil)
			(setf result (make-city 
				:state nome
				:father pai
				:gn nil
				:hn nil
				:fn nil
				:dn 0
			))	
		)
		(if (not (eql pai nil))
			(setf result (make-city 
				:state nome
				:father pai
				:gn distancia
				:hn heuristica
				:fn (+ distancia heuristica)
				:dn (+ (city-dn pai) 1)
			))
		)
		result
	)
)

(defun successors (city)
	(let ((result nil))
		(dotimes (i (length grafo))
			(if (eql (city-state city) (city-state (first (nth i grafo))))
				(dolist (j (rest (nth i grafo)))
					;actualizar pai
					(setf (city-father j) city)
					;actualizar profundidade
					(setf (city-dn j) (1+ (city-dn city)))
					;actualizamos heuristica
					(setf (city-hn j) (get-hn j))
					;actualizamos funcao avaliacao
					(setf (city-fn j) (+ (city-gn j) (city-hn j)))
					;adicionar lista
					(setf result (cons j result))
				)
			)
		)
		result
	)
)

(defun successors-limit-prof (city limit)
	(if (< (city-dn city) limit)
		(let ((result nil))
			(dotimes (i (length grafo))
				(if (eql (city-state city) (city-state (first (nth i grafo))))
					(dolist (j (rest (nth i grafo)))
						(setf (city-father j) city)
						(setf (city-dn j) (1+ (city-dn city)))
						(setf result (cons j result))
					)
				)
			)
			result
		)		
	)
)

(defun successors-limit-fn (city limit)
	(if (or (eql (city-fn city) nil) (< (city-fn city) limit))
		(let ((result nil))
			(dotimes (i (length grafo))
				(if (eql (city-state city) (city-state (first (nth i grafo))))
					(dolist (j (rest (nth i grafo)))
						;actualizar pai
						(setf (city-father j) city)
						;actualizar profundidade
						(setf (city-dn j) (1+ (city-dn city)))
						;actualizamos heuristica
						(setf (city-hn j) (get-hn j))
						;actualizamos funcao avaliacao
						(setf (city-fn j) (+ (city-gn j) (city-hn j)))
						;adicionar lista
						(setf result (cons j result))
					)
				)
			)
			result
		)
	)
)

(defun get-hn (city)
	(dolist (z heuristica-faro)
		(if (eql (city-state (first z)) (city-state city))
			(return (first (rest z)))
		)
	)
)

(defun goal (city)
	(if (eql (city-state city) goal)
		t
		nil
	)
)

(defun print-path (city)
	(defun aux (city cost)
		(if (null (city-father city))
			(cons (city-state city) cost)
			(cons (city-state city) (aux (city-father city) (+ cost (city-gn city))))
		)	
	)
	(aux city 0)
) 

(defun main ()
	(let ((result nil))
		;(setf result (BFS v-castelo))
		;(setf result (UCS v-castelo))
		;(setf result (DFS v-castelo))
		;(setf result (IDDFS coimbra))
		;(setf result (greedy v-castelo))
		;(setf result (A* v-castelo))
		(setf result (IDA* coimbra))
		;(setf result (Hillclimbing v-castelo))
		;(get-nodes-prof coimbra 2)
		(if (not (equal result nil))
			(print-path result)
		)
	)
)

