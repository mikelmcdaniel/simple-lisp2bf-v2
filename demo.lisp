(defun is_prime (x)
  (let ((j 0))
    (setq j 2)
    (loop while (!= x j) do
      (cond
        ((!= (% x j) 0) (setq j (+ j 1))
        1)
        (1 (setq j x)
        0)))))

(defun square (x)
  (* x x))

(defun cube (x)
  (* x (square x)))

(defun factorial (x)
  (cond
    ((= x 0) 1)
    (1 (* x (factorial (- x 1))))))

(defun print_primes (first last)
  (loop while (<= first last) do
    (cond
      ((is_prime first) (print_num first)
      (write "\n")))
    (setq first (+ first 1))))

(defun print_num (x)
  (cond
    ((>= x 100) (write (+ (/ x 100) '0'))))
  (cond
    ((>= x 10) (write (+ (% (/ x 10) 10) '0'))))
  (write (+ (% x 10) '0')))

(defun is_digit (c)
  (* (bool (<= '0' c)) (bool (<= c '9'))))

(defun read_num ()
  (let ((c 0) (result 0))
    (setq c (read))
    (setq result 0)
    (loop while (is_digit c) do
      (setq result (+ (* result 10) (- c '0')))
      (setq c (read)))
    result))

(defun start ()
  (let ((last 0) (start 0))
    (write "Enter start: ")
    (setq start (read_num))
    (write "Enter last: ")
    (setq last (read_num))
    (write "\n")
    (print_num start)
    (write " ** ")
    (print_num last)
    (write " % 256 == ")
    (print_num (** start last))
    (write "\n")
    (print_num start)
    (write " ** 3 % 256 == ")
    (print_num (cube start))
    (write "\n")
    (print_num start)
    (write "! % 256 == ")
    (print_num (factorial start))
    (write "\n")
    (write "log2(")
    (print_num last)
    (write ") == ")
    (print_num (log2 last))
    (write "\n")
    (write "Here are the primes from ")
    (print_num start)
    (write " to ")
    (print_num last)
    (write ":\n")
    (print_primes start last)
    (exit)))
