; Problemas de programação funcional do site Hacker Rank, usando common lisp
; Alguns facilitadores como mapcar, reduce e filter são evitados para maximizar o aproveitamento dos exercícios

; (PROBLEMA 1)
; Resolver A+B, onde:
;    a,b: 1 <= a,b <= 1000

(defun soma (a b)
  (unless (or (< a 1) (> a 1000)
              (< b 1) (> b 1000)) 
    (+ a b)))

(format t "~d~%" (soma (parse-integer (read-line)) (parse-integer (read-line))))

; (PROBLEMA 2)
; Imprimir "Hello World"

(defun helloworld ()
    (format t "Hello World~%"))

(helloworld)

; (PROBLEMA 3)
; Imprimir "Hello World" N vezes, onde:
;    n: 0 <= n <= 50

(defun helloworld-n-vezes (n)
  (if (or (<= n 0) (> n 50)) nil
      (progn (format t "Hello World~%")
             (helloworld-n-vezes (- n 1)))))

(helloworld-n-vezes (parse-integer (read-line)))

; (PROBLEMA 4)
; Considerando uma lista de inteiros com X elementos, retornar uma nova lista
; onde cada elemento da lista original é repetido S vezes a partir de sua posição relativa.
; Imprimir a lista resultante, na qual:
;    x: 0 <= x <= 10
;    s: 1 <= s <= 100

(defun ler-lista ()
  (labels
      ((entrada (x)
         (let ((n (read *standard-input* nil)))
           (if (or (null n) (> x 10)) nil
               (cons n (entrada (+ x 1)))))))
    (entrada 1)))

(defun x-em-s-vezes (s lista)
  (labels
      ((repete (s lista novalista)
         (if (<= s 0) novalista
             (repete (- s 1) lista (cons (car lista) novalista))))
       (percorre (s lista novalista)
         (if (or (null lista) (< s 1) (> s 100))
             (reverse novalista)
             (percorre s (cdr lista) (repete s lista novalista)))))
    (percorre s lista '())))
    
(format t "~{~a~^~%~}" (x-em-s-vezes (parse-integer (read-line)) (ler-lista)))

; (PROBLEMA 5)
; Considerando uma lista de inteiros com B elementos, filtrar cada elemento Y de acordo
; com a restrição (Y < X). Imprimir lista resultante, na qual: 
;    b: 0 <= b <= 100
;    y: -100 <= y <= 100
;    x: -100 <= x <= 100

(defun ler-x ()
  (let ((x (parse-integer (read-line))))
    (unless (or (< x -100) (> x 100)) x)))

(defun ler-lista (b)
  (let ((y (read *standard-input* nil)))
    (cond
      ((or (null y) (> b 100)) nil)
      ((or (< y -100) (> y 100)) (ler-lista (+ b 1)))
      (t (cons y (ler-lista (+ b 1)))))))

(defun filtro (x lista)
  (if (or (null x) (null lista)) nil
      (if (< (car lista) x)
          (progn
            (format t "~d~%" (car lista))
            (filtro x (cdr lista)))
          (filtro x (cdr lista)))))

(filtro (ler-x) (ler-lista 1))

; (PROBLEMA 6)
; Considerando uma lista de inteiros com N elementos, filtrar as posições ímpares.
; Imprimir a lista resultante.

(defun ler-lista ()
  (let ((n (read *standard-input* nil)))
    (if (null n) nil
        (cons n (ler-lista)))))

(defun filtro (lista)
  (labels
      ((aux (lista i novalista)
         (cond ((null lista) (reverse novalista))
               ((/= (mod i 2) 0) (aux (cdr lista) (+ i 1) (cons (car lista) novalista)))
               (t (aux (cdr lista) (+ i 1) novalista)))))
    (aux lista 0 '())))

(format t "~{~a~^~%~}" (filtro (ler-lista)))

; (PROBLEMA 7)
; Considerando um inteiro N, retornar uma lista de inteiros com N elementos.
; Imprimir lista resultante, na qual:
;    n: 1 <= n <= 100

(defun lista-n-inteiros (n)
  (labels
      ((aux (i n lista)
         (if (or (> i n) (< n 1) (> n 100))
             (reverse lista)
             (aux (+ i 1) n (cons i lista))))) 
    (aux 1 n '())))

(format t "[~{~a~^, ~}]" (lista-n-inteiros (parse-integer (read-line))))

; (PROBLEMA 8)
; Considerando uma lista de inteiros com N elementos, retornar uma nova lista 
; onde a posição relativa de cada elemento X é o inverso da lista original.
; Imprimir lista resultante, na qual:
;    n: 1 <= n <= 100
;    x: 0 <= x <= 100

(defun ler-lista ()
  (let ((x (read *standard-input* nil)))
    (cond
      ((null x) nil)
      ((or (< x 0) (> x 100)) (ler-lista))
      (t (cons x (ler-lista))))))

(defun reverter (lista)
  (labels
      ((aux (lista novalista)
         (if (null lista) novalista
             (aux (cdr lista) (cons (car lista) novalista)))))
    (let ((n (length lista)))
      (unless (or (< n 1) (> n 100))
        (aux lista '())))))

(format t "~{~a~^~%~}" (reverter (ler-lista)))

;
