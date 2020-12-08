; Proyecto: N-Reinas
; EIF-400 Paradigmas de Programación
; Autor:
; Luis Venegas Ulloa

; Dominio: Una lista y una posición (posición menor que el tamaño de la lista). 
; Codominio: Elemento en la n-ésima posición (empezando en 1).
(define element-at
  (lambda (n m)
    (cond  ((null? n) null )
           ((zero? (- m 1)) (car n))
           (else(element-at (cdr n) (- m 1))))))

; Dominio: Una lista de listas, una fila y una columna (fila y columna menor que el tamaño de la lista de listas).
; Codominio: Elemento en la n-ésima posición (empezando en la fila y columna en 1).
(define matrix-at
  (lambda (m f c)
    (element-at(element-at m f) c)))

; Dominio: Una lista, una columna y el elemento a insertar (columna menor que el tamaño de la lista de listas).
; Codominio: Una lista.
(define insert-pos-aux
  (lambda (l c n)
    (cond((equal? 1 c) (cons n (cdr l)))
         (else( cons (car l) (insert-pos-aux (cdr l) (- c 1) n))))))

; Dominio: Una lista, una columna y el elemento a insertar (columna menor que el tamaño de la lista de listas).
; Codominio: Una lista.
(define insert-list
  (lambda (l c n)
    (insert-pos-aux l c n)))

; Dominio: Una lista de listas, una fila, una columna y el elemento a insertar
; (fila y columna menor que el tamaño de la lista de listas).
; Codominio: Una lista de listas con el nuevo elemento en la n-ésima fila y columna
; (empezando en la fila y columna en 1). 
(define insert-matrix
  (lambda (m f c n)
     (cond((equal? 1 f) (cons (insert-list (car m) c n )(cdr m)))
         (else( cons (car m) (insert-matrix (cdr m) (- f 1) c n))))))

; Dominio: Tamño de la lista y el elemento del cual va a estar conformada la lista.
; Codominio: Una lista.
(define create-list
  (lambda (c n)
    (cond((equal? 1 c) (cons n '()))
         (else( cons n (create-list  (- c 1) n))))))

; Dominio: Cantidad de filas, cantidad de columnas y el elemento del cual va a estar conformada la matriz.
; Codominio: Una lista de listas.
(define create-matrix
  (lambda (f c n)
    (cond((equal? 1 f) (cons (create-list c n) '()))
         (else( cons (create-list c n) (create-matrix  (- f 1) c n))))))

; Dominio: Una lista, columna e iterador.
; Codominio: Un booleano si se cumplen la validacion de la fila y false si no cumple.
(define validate-row-aux-a
  (lambda (l c i)
    (cond((= c i)(zero? (car l)))
         ((equal? 1 (car l)) false)
         (else(validate-row-aux-a (cdr l ) c (+ i 1))))))

; Dominio: Una lista y una columna (columna menor que el tamaño de la lista de listas).
; Codominio: Un booleano si se cumplen la validacion de la fila y false si no cumple.
(define validate-row-aux
  (lambda (l c)
    (validate-row-aux-a l c 1)))

; Dominio: Una lista de listas, fila, columna e iterador.
; Codominio: Un booleano si se cumplen la validacion de la fila y false si no cumple.
(define validate-row
  (lambda (m f c i)
    (cond ((= f i) (validate-row-aux (car m) c))
          (else( validate-row (cdr m) f c (+ 1 i))))))

; Dominio: Una lista de listas, una fila y una columna (fila y columna menor que el tamaño de la lista de listas).
; Codominio: Un booleano si se cumplen la validacion de la diagonal inferior y false si no cumple.
(define validate-lower-diagonal
  (lambda (m f c)
    (cond((or (= 1 f) (= 1 c))(zero? (matrix-at m f c)))                      
          (( not (zero? (matrix-at m f c))) false)
          (else(validate-lower-diagonal m (- f 1)  (- c 1))))))

; Dominio: Una lista de listas, una fila y una columna (fila y columna menor que el tamaño de la lista de listas).
; Codominio: Un booleano si se cumplen la validacion de la diagonal superior y false si no cumple.
(define validate-upper-diagonal
  (lambda (m f c)
    (cond(  (or (= (length m)  f) (= 1 c))  (zero? (matrix-at m f c)))                      
          (( not (zero? (matrix-at m f c))) false)
          (else(validate-upper-diagonal m (+ f 1)  (- c 1))))))

; Dominio: Una lista de listas, una fila y una columna (fila y columna menor que el tamaño de la lista de listas). 
; Codominio: Un booleano si se cumplen todas las validaciones anteriores y false si alguna no cumple.
(define validate
  (lambda (m f c)
    (cond ((and  (validate-upper-diagonal m f c ) (and (validate-lower-diagonal  m f c) (validate-row m f c 1) ) )   true)                                                                                    
          (else false))))

; Dominio: Una listas de listas, fila, columna, tamaño, lista, cantidad de soluciones
; (fila y columna menor que el tamaño de la lista de listas).  
; Codominio: Una lista de listas.
(define place-queen-aux
  (lambda (m f c tam l n)
    (cond((= (length l) n) l)
         ((> f tam )  l)
         ((>  c  tam)(cons m l )) 
         ((validate m f c)(place-queen-aux m (+ f 1) c tam (place-queen-aux (insert-matrix m f c 1) 1 (+ c 1) tam l n) n) )
         (else (place-queen-aux m (+ f 1) c tam l n)))))

; Dominio: El número de reinas y el número de soluciones que se desea.
; Codominio: Una lista de listas de la solución de la cantidad de reinas
; que se enviaron igual al número de soluciones dado.
(define backtracking-n-queens-aux
  (lambda (tam n)
    (place-queen-aux (create-matrix tam tam 0) 1 1 tam '() n)))

; Dominio: Una lista de listas.
; Codominio: Una lista de listas (imprime en pantalla una lista de listas como matriz).
(define print-matrix
  (lambda(m)
    (map (lambda(m) (print m) (newline)) m))) 

; Dominio: El número de reinas y el número de soluciones que se desea.
; Codominio: Una lista de listas de la solución de la cantidad de reinas
; que se enviaron igual al número de soluciones dado (con un formato de impresión).
(define backtrackingNReinas
  (lambda (tam n)
    (map (lambda(m) (print-matrix m) (newline)) (backtracking-n-queens-aux tam n))))
