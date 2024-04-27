; (PROBLEMA 1)
; resolva a+b, onde :
;    a,b: 1 <= a,b <= 1000

(defun soma (a b)
  (unless (or (< a 1) (< b 1) (> a 1000) (> b 1000)) 
    (+ a b)))

(format t "~d~%" (soma (parse-integer (read-line)) (parse-integer (read-line))))

; (PROBLEMA 2)
; imprima hello world

(defun helloworld ()
    (format t "Hello World"))

(helloworld)

; (PROBLEMA 3)
; imprima hello world n vezes, onde:
;    n: 0 <= n <= 50

(defun n-helloworld (n)
  (if (or (< n 0) (> n 50))
      nil
      (progn
        (unless (= n 0)
         (format t "Hello World~%"))
        (n-helloworld (- n 1)))))

(n-helloworld (parse-integer (read-line)))

; (PROBLEMA 4)
; considerando uma lista de inteiros com X elementos,
; retorne uma nova lista repetindo cada elemento S vezes em posição relativa
; e imprima a lista resultante, onde:
;    x: 0 <= x <= 10
;    s: 1 <= s <= 100

(defun imprimir-lista (lista)
  (if (null lista)
      nil
      (progn
        (format t "~d~%" (car lista))
        (imprimir-lista (cdr lista)))))

(defun ler-lista (&optional lista)
  (let ((input (lambda ()
                 (let ((n (read *standard-input* nil)))
                   (if (null n)
                       nil
                       (cons n (ler-lista)))))))
    (setf lista (funcall input))
    (unless (or (< (length lista) 0) (> (length lista) 10)) lista)))

(defun x-em-s-vezes (s lista &optional sx)
  (if (or (null lista) (< s 1) (> s 100))
      (reverse sx)
      (progn
        (dotimes (i s)
          (setf sx (cons (car lista) sx)))
        (x-em-s-vezes s (cdr lista) sx))))

(imprimir-lista (x-em-s-vezes (parse-integer (read-line)) (ler-lista)))

; (PROBLEMA 5)
; considerando uma lista de inteiros com B elementos,
; faça um filtro que compara cada elemento Y com uma restrição X,
; e imprima a lista resultante, onde:
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
  (if (or (null x) (null lista))
      nil
      (cond
        ((< (car lista) x) (progn (format t "~d~%" (car lista)) (filtro x (cdr lista))))
        (t (filtro x (cdr lista))))))

(let ((b 1)) (filtro (ler-x) (ler-lista b)))

