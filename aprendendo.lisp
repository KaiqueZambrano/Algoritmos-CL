; hello world
(defun hello ()
  (format t "Hello World~%"))

; eleva a ao quadrado
(defun quadrado (a)
  (* a a))

; imprime um inteiro a
(defun imprimir (a)
  (format t "~d~%" a))

; conceitos eval/apply
(defvar lista '(1 2 3 4))
(defvar maximo (apply #'max lista))
(defvar produto (mapcar #'quadrado lista))
(defvar soma (reduce #'+ lista))

; funções anônimas, closure e imutabilidade
(defun contador ()
  (let ((cont 0))
    (list
     (lambda ()
       (incf cont))
     (lambda ()
       (decf cont)))))

; funções são valores de primeira ordem
(defvar meuContador (contador))
(defvar opInc (first meuContador))
(defvar opDec (second meuContador))

; função que incrementa/decrementa cont acessando funções internas
(defun incremento ()
  (imprimir (funcall opInc)))

(defun decremento ()
  (imprimir (funcall opDec)))
