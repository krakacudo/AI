;;;; procura.lisp
;;;; Disciplina de IA - 2020 / 2021
;;;; Problema do Quatro -  1� parte do projeto de Intelig�ncia Artificial.
;;;; Autores: Ricardo Lopes 180221044, Rui Silva 180221045 


;;Efetua a procura em largura-primeiro e recebe como argumento o n� inicial, a fun��o objetivo a ser testada, a fun��o sucessores e a lista de operadores.
;;Retorna o n� solu��o encontrado.
(defun bfs(no-inicial f-objetivo f-sucessores &optional letra abertos fechados (tempo (get-universal-time)))
(let ((sucessores (funcall f-sucessores no-inicial)))
 (cond
      ((null no-inicial) nil)

     ((funcall f-objetivo no-inicial) no-inicial)

      ((lista-solucaop sucessores f-objetivo) (mostrar-solucao (lista-solucaop sucessores f-objetivo) (1+ (length fechados)) (+ (length abertos) (length sucessores) (1+ (length fechados))) (- (get-universal-time) tempo) letra))

      ((null abertos) (bfs (car (abertos-bfs abertos sucessores)) f-objetivo f-sucessores letra (cdr (abertos-bfs abertos sucessores))  (cons no-inicial fechados) tempo))

       (t (bfs (car abertos) f-objetivo f-sucessores letra (cdr (abertos-bfs abertos sucessores)) (cons no-inicial fechados) tempo))
 )
)
)


;;Efetua a procura em largura-primeiro e recebe como argumento o n� inicial, a fun��o objetivo a ser testada, a fun��o sucessores e a lista de operadores.
;;Retorna o n� solu��o encontrado.
(defun dfs(no-inicial f-objetivo f-sucessores &optional (profundidade 9999) letra  abertos fechados (tempo (get-universal-time)))
  (let ((sucessores (funcall f-sucessores no-inicial profundidade)))
    (cond
     ((null no-inicial) nil)

     ((funcall f-objetivo no-inicial) no-inicial)

     ((lista-solucaop sucessores f-objetivo) (mostrar-solucao (lista-solucaop sucessores f-objetivo) (1+(length fechados)) (+ (length abertos) (length sucessores) (1+ (length fechados))) (- (get-universal-time) tempo) letra))

     ((null abertos) (dfs (car (abertos-dfs abertos (reverse sucessores))) f-objetivo f-sucessores profundidade letra (cdr (abertos-dfs abertos (reverse sucessores)))  (cons no-inicial fechados) tempo))

     (t (dfs (car abertos) f-objetivo f-sucessores profundidade letra (cdr (abertos-dfs abertos (reverse sucessores))) (cons no-inicial fechados) tempo))
     )
    )
)

;;Efetua a procura ordenada a* e recebe como argumento o n� inicial, a fun��o objetivo a ser testada, a fun��o sucessores e a lista de operadores.
;;Retorna o n� solu��o encontrado.
(defun astar(no-inicial f-objetivo f-sucessores &optional letra abertos fechados (tempo (get-universal-time)))
  (let ((sucessores (funcall f-sucessores no-inicial)))
    (cond
     ((null no-inicial) nil)

     ((funcall f-objetivo no-inicial) no-inicial)

     ((lista-solucaop sucessores f-objetivo) (mostrar-solucao (lista-solucaop sucessores f-objetivo) (1+ (length fechados)) (+ (length abertos) (length sucessores) (1+ (length fechados))) (- (get-universal-time) tempo) letra))

     ((null abertos) (astar (car (abertos-a* abertos sucessores fechados)) f-objetivo f-sucessores letra (cdr (abertos-a* abertos sucessores fechados))  (cons no-inicial fechados) tempo))

     (t (astar (car abertos) f-objetivo f-sucessores letra (cdr (abertos-a* abertos sucessores fechados)) (cons no-inicial fechados) tempo))
     )
    )
)

;;Efetua a procura ida* e recebe como argumento o n� inicial, a fun��o objetivo a ser testada, a fun��o sucessores e a lista de operadores.
;;Retorna o n� solu��o encontrado.
(defun idastar(no-inicial f-objetivo f-sucessores &optional (limiar (custo no-inicial)) letra abertos fechados (tempo (get-universal-time)))
  (let ((sucessores (funcall f-sucessores no-inicial)))
    (cond
     ((null no-inicial) nil)

     ((funcall f-objetivo no-inicial) no-inicial)

     ((lista-solucaop sucessores f-objetivo) (mostrar-solucao (lista-solucaop sucessores f-objetivo) (1+ (length fechados)) (+ (length abertos) (length sucessores) (1+ (length fechados))) (- (get-universal-time) tempo) letra))

     ((null abertos) (idastar no-inicial f-objetivo f-sucessores (custo (car (sort sucessores 'comparar-no))) letra (abertos-ida* abertos sucessores fechados (custo no-inicial)) nil tempo))

     (t (idastar (car abertos) f-objetivo f-sucessores limiar letra (cdr (abertos-ida* abertos sucessores fechados limiar)) (cons no-inicial fechados) tempo))
     )
    )
)


(defun teste() 
'((
 ((branca quadrada alta oca) (preta redonda baixa oca) (preta quadrada alta oca) (branca quadrada alta cheia))
 ((branca redonda alta oca) 0 (branca redonda alta cheia) 0)
 ((preta quadrada baixa cheia) (preta redonda alta cheia) (branca quadrada baixa oca) 0)
 ((preta quadrada baixa oca) 0 (branca quadrada baixa cheia) 0)
)
(
 (branca redonda baixa oca)
 (preta redonda baixa cheia)
 (preta redonda alta oca)
 (preta quadrada alta cheia)
 (branca redonda baixa cheia)  ))

)
