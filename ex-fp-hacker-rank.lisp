; 1: escreva hello world
(defun helloworld ()
  (format t "Hello World~%"))

(helloworld)

; 2: a+b, onde 1 <= a,b <= 1000
(defun soma (a b)
  (unless (or (or (< a 1) (< b 1))
              (or (> a 1000) (> b 1000))) 
    (+ a b)))

(let ((a (parse-integer (read-line)))
      (b (parse-integer (read-line))))
  (format t "~d" (soma a b)))

; 3: escreva hello world em n linhas, onde 0 <= n <= 50 
(defun n-helloworld (n)
  (unless (or (< n 0) (> n 50))
    (loop for i from 1 to n do (helloworld))))

(let ((n (parse-integer (read-line))))
  (n-helloworld n))

; 4: 
