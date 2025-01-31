(defun is_prime (x)
  (let ((j 2))
    (loop while (!= x j) do
      (cond
        ((!= (% x j) 0) (setq j (+ j 1)) 1)
        (1 (setq j x) 0)))))

(defun square (x) (* x x))

(defun cube (x) (* x (square x)))

(defun factorial (x)
  (cond
    ((= x 0) 1)
    (1 (* x (factorial (- x 1))))))

(defun print_primes (first last)
  (loop while (<= first last) do
    (cond ((is_prime first) (print_num first) (write '\n')))
    (setq first (+ first 1))))

(defun print_num (x)
  (cond ((>= x 100) (write (+ (/ x 100) '0'))))
  (cond ((>= x 10) (write (+ (% (/ x 10) 10) '0'))))
  (write (+ (% x 10) '0')))

(defun is_digit (c)
  (* (>= c '0') (<= c '9')))

(defun read_num ()
  (let ((c 0) (result 0))
    (loop while (is_digit (setq c (read))) do
      (setq result (+ (* result 10) (- c '0'))))))

(defun start ()
  (let ((start 0) (last 0))
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
    (write "^3 % 256 == ")
    (print_num (cube start))
    (write "\n")

    (print_num start)
    (write "! % 256 == ")
    (print_num (factorial start))
    (write "\n")


    (write "Here are the primes from ")
    (print_num start)
    (write " to ")
    (print_num last)
    (write ":\n")
    (print_primes start last))
  (exit))
