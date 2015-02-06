;retorna no e nova fronteira
(defun remove-node (frontier)
	(first frontier)
)

(defun update-frontier (frontier)
	(rest frontier)
)

(defun max-cities-fn (city1 city2)
	(if (< (city-fn city1) (city-fn city2))
		t
		nil
	)
)

(defun max-cities-gn (city1 city2)
	(if (< (city-gn city1) (city-gn city2))
		t
		nil
	)
)

(defun max-cities-hn (city1 city2)
	(if (< (city-hn city1) (city-hn city2))
		t
		nil
	)
)

(defun is-leaf (node)
	(if (eql (length (successors node)) 0)
		t
		nil
	)
)

(defun insert-new-nodes (frontier node)
	(let ((candidates (successors node)) (result nil) (count 0))
		;(format t "candidados ~a ~%" candidates)
		(dolist (i candidates)
			(dolist (j frontier)
				(if (eql (city-state i) (city-state j))
					(incf count)
				)
			)
			(if (eql count 0)
				(setf result (cons i result))
			)
			(setf count 0)
		)
		result
	)
)

(defun insert-new-nodes-limited (frontier node limit func)
	(let ((candidates (funcall func node limit)) (result nil) (count 0))
		;(format t "candidados ~a ~%" candidates)
		(dolist (i candidates)
			(dolist (j frontier)
				(if (eql (city-state i) (city-state j))
					(incf count)
				)
			)
			(if (eql count 0)
				(setf result (cons i result))
			)
			(setf count 0)
		)
		result
	)
)

(defun print-frontier (frontier)
	(format t "( ")
	(dolist (i frontier)
		(format t " ~a ~a" (city-state i) (city-gn i))
	)
	(format t ") ~%~%")
)

(defun get-min-fn (excluded previous)
	(let ((min most-positive-fixnum))
		(dolist (i excluded)
			(if (and (< (city-fn i) min) (> (city-fn i) previous))
				(setf min (city-fn i))
			)
		)
		min
	)
)

(defun get-best-frontier-hn (frontier)
	(let ((best (first frontier)))
		(dolist (i frontier)
			(if (< (city-hn i) (city-hn best))
				(setf best i)
			)
		)
		best
	)
)

(defun random-successors (node)
	(let ((successors (successors node)))
		(if (equal successors nil)
			nil
			(nth (random (length successors)) successors)
		)
 	)
)

(defun get-nodes-prof (start prof)
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
				(if (eql (city-dn node) prof)
					(return (append frontier (list node)))
					
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