(defun BFS (start)
	(let ((frontier (successors start)) (node nil) (expanded nil))
		(loop while (> (length frontier) 0)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;retirar fila e actualizar fronteira
			do(progn 
				(setf node (remove-node frontier))
				(setf frontier (update-frontier frontier))
				(setf expanded (cons start expanded))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
				;se for objectivo
				(if (goal node)
					(return node)
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;expandir no filho
					(progn 
						;(format t "Ordem Expansao: ~a ~%" (city-state node))
						(setf expanded (cons node expanded))
						(setf frontier (append frontier (insert-new-nodes (append frontier expanded) node)))
						;(print-frontier frontier)
					)
				)
			)
		)
	)
)

(defun UCS (start)
	(let ((frontier (successors start)) (node nil) (expanded nil))
		(loop while (> (length frontier) 0)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;retirar fila e actualizar fronteira
			do(progn 
				(sort frontier (function max-cities-gn))
				(setf node (remove-node frontier))
				(setf frontier (update-frontier frontier))
				(setf expanded (cons start expanded))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
				;se for objectivo
				(if (goal node)
					(return node)
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;expandir no filho
					(progn 
						;(format t "Ordem Expansao: ~a dist: ~a ~%" (city-state node) (city-fn node))
						(setf expanded (cons node expanded))
						(setf frontier (append frontier (insert-new-nodes (append frontier expanded) node)))
						;(print-frontier frontier)
					)
				)
			)
		)
	)
)

(defun DFS (start)
	(let ((frontier (successors start)) (node nil) (expanded nil))
		(loop while (> (length frontier) 0)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;retirar fila e actualizar fronteira
			do(progn 
				(setf node (remove-node frontier))
				(setf frontier (update-frontier frontier))
				(setf expanded (cons start expanded))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
				;se for objectivo
				(if (goal node)
					(return node)
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;expandir no filho
					(progn 
						;(format t "Ordem Expansao: ~a ~%" (city-state node))
						(setf expanded (cons node expanded))
						(setf frontier (append (insert-new-nodes (append frontier expanded) node) frontier))
						;(print-frontier frontier)
					)
				)
			)
		)
	)
)

(defun DFLS (start limit)
	(let ((frontier (successors-limit-prof start limit)) (node nil) (expanded nil))
		(loop while (> (length frontier) 0)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;retirar fila e actualizar fronteira
			do(progn 
				(setf node (remove-node frontier))
				(setf frontier (update-frontier frontier))
				(setf expanded (cons start expanded))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
				;se for objectivo
				(if (goal node)
					(return node)
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;expandir no filho
					(progn 
						;(format t "Ordem Expansao: ~a ~%" (city-state node))
						(setf expanded (cons node expanded))
						(setf frontier (append (insert-new-nodes-limited (append frontier expanded) node limit (function successors-limit-prof)) frontier))
						;(print-frontier frontier)
					)
				)
			)
		)
	)
)

(defun IDDFS (start)
	(let ((i 0) (result nil))
		(loop while (eql result nil)
			do(setf result (DFLS start i))
			(format t "iteracao ~a ~%" i)
			(incf i)
		)
		result
	)
)

(defun greedy (start)
	(let ((frontier (successors start)) (node nil) (expanded nil))
		(loop while (> (length frontier) 0)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;retirar fila e actualizar fronteira
			do(progn 
				(sort frontier (function max-cities-hn))
				(setf node (remove-node frontier))
				(setf frontier (update-frontier frontier))
				(setf expanded (cons start expanded))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
				;se for objectivo
				(if (goal node)
					(return node)
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;expandir no filho
					(progn 
						;(format t "Ordem Expansao: ~a dist: ~a ~%" (city-state node) (city-hn node))
						(setf expanded (cons node expanded))
						(setf frontier (append frontier (insert-new-nodes (append frontier expanded) node)))
						;(print-frontier frontier)
					)
				)
			)
		)
	)
)

(defun A* (start)
	(let ((frontier (successors start)) (node nil) (expanded nil))
		(loop while (> (length frontier) 0)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;retirar fila e actualizar fronteira
			do(progn 
				(sort frontier (function max-cities-fn))
				(setf node (remove-node frontier))
				(setf frontier (update-frontier frontier))
				(setf expanded (cons start expanded))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
				;se for objectivo
				(if (goal node)
					(return node)
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;expandir no filho
					(progn 
						;(format t "Expansion: ~a gn: ~a hn: ~a fn: ~a ~%" (city-state node) (city-gn node) (city-hn node) (city-fn node))
						(setf expanded (cons node expanded))
						(setf frontier (append frontier (insert-new-nodes (append frontier expanded) node)))
						(print-frontier frontier)
					)
				)
			)
		)
	)
)

(defun aux-IDA* (start fn)
	(let ((frontier (successors-limit-fn start fn)) (node nil) (expanded nil) (excluded nil))
		(loop while (> (length frontier) 0)
			;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			;retirar fila e actualizar fronteira
			do(progn 
				(setf node (remove-node frontier))
				(setf frontier (update-frontier frontier))
				(setf expanded (cons start expanded))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
				;se for objectivo
				(if (goal node)
					(return node)
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;expandir no filho
					(progn 
						;verificar se no pode ser excluido por limite fn
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						(if (> (city-fn node) fn)
							(setf excluded (cons node excluded))
						)
						;expandir de certeza
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						(format t "Expansion: ~a ~%" (city-state node))
						(setf expanded (cons node expanded))
						(setf frontier (append (insert-new-nodes-limited (append frontier expanded) node fn (function successors-limit-fn)) frontier))
						;(print-frontier frontier)
					)
				)
			)
		)
		(if (eql (length frontier) 0)
			excluded
			node
		)
	)
)

(defun IDA* (start)
	(let ((result nil) (fn (get-hn start)))
		(setf result (aux-IDA* start fn))

		(loop while (not (eql (type-of result) 'city))
			do(setf fn (get-min-fn result fn))
			(format t "--------> New FN Limit ~a ~%" fn)
			do(setf result (aux-IDA* start fn))
		)
		result
	)
)

(defun Hillclimbing (start)
	(let ((actual nil) (next-node nil) (stop nil))
		;para nao lixar
		(setf (city-hn start) (get-hn start))
		(setf actual start)
		(loop while (null stop)
			do(setf next-node (get-best-frontier-hn (successors actual)))
			do(if (> (city-hn next-node) (city-hn actual))
				(setf stop t)
				(setf actual next-node)
			)
		)
		actual
	)
)
