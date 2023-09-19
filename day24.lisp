(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/24

(defparameter *day24-input* (local-file #P"day24.txt" :error T))

(defparameter *day24-input-re*
  (cl-ppcre:create-scanner "^(inp|add|mul|div|mod|eql) (\\w+)( (-?\\w+))?$"))

(defun d24-data ()
  "Validates the data and finds the three unique parameters for each cycle of the input code."
  (declare (optimize (speed 3)))
  (with-open-file (stream *day24-input* :if-does-not-exist :error)
    (let ((parameters (make-array '(3 14) :element-type '(signed-byte 8) :initial-element 0))
          (expected-ops #("inp" "mul" "add" "mod" "div" "add" "eql" "eql" "mul"
                          "add" "mul" "add" "mul" "mul" "add" "add" "mul" "add"))
          (expected-vars #("w" "x" "x" "x" "z" "x" "x" "x" "y"
                           "y" "y" "y" "z" "y" "y" "y" "y" "z"))
          (expected-args #("" "0" "z" "26" "div" "chk" "w" "0" "0"
                           "25" "x" "1" "y" "0" "w" "add" "x" "y")))
      (declare (type (simple-vector 18) expected-ops expected-vars expected-args))
      (dotimes (i 14 (prog1 parameters
                       (let ((line (read-line stream NIL NIL)))
                         (when line (error "Unexpected data in input: ~a" line)))))
        (dotimes (j 18)
          (let ((line (read-line stream)))
            (declare (type (simple-array character (*)) line))
            (multiple-value-bind (match groups)
                (cl-ppcre:scan-to-strings *day24-input-re* line)
              (declare (type (or null (simple-array character (*))) match))
              (declare (type (or null (simple-array (or null (simple-array character (*))) (*)))
                             groups))
              (unless match (error "Invalid line: ~a" line))
              (let ((expected-op (svref expected-ops j))
                    (expected-var (svref expected-vars j))
                    (expected-arg (svref expected-args j))
                    (op (aref groups 0))
                    (var (aref groups 1))
                    (arg (aref groups 3)))
                (declare (type (simple-array character (*))
                               expected-op expected-var expected-arg op var))
                (declare (type (or null (simple-array character (*))) arg))
                (unless (string= op expected-op)
                  (error "Invalid operation '~a' (~a). Expected '~a'." op j expected-op))
                (unless (string= var expected-var)
                  (error "Invalid variable '~a' (~a). Expected '~a'." var j expected-var))
                (cond
                  ((string= "div" expected-arg)
                   (setf (aref parameters 0 i) (parse-integer arg)))
                  ((string= "chk" expected-arg)
                   (setf (aref parameters 1 i) (parse-integer arg)))
                  ((string= "add" expected-arg)
                   (setf (aref parameters 2 i) (parse-integer arg)))
                  ((and (or (not (null arg)) (< 0 (length expected-arg)))
                        (or (null arg)       (not (string= arg expected-arg))))
                   (error "Invalid argument '~a'. Expected '~a'." arg expected-arg)))))))))))

(defun d24-solve (parameters high-p)
  "Find the correct input.

There are 14 largely similar sub-routines in the puzzle input:

inp w
mul x 0
add x z
mod x 26
div z DIV
add x CHK
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y ADD
mul y x
add z y

The sub-routine in lisp:

(if (/= w (+ CHK (mod z 26)))
    (setf z (+ (* (floor z DIV) 26) w ADD))
    (setf z (floor z DIV))))

The w-value is the current digit of the input value and DIV, CHK, and ADD are parameters unique for
each cycle of the routine. The z-value acts as a \"stack of bits in base-26.\"

The parameter DIV is only ever 1 or 26. This leads to the fact that z is a base 26 value.

From here on we can search the 14-digit number closest to the wanted input.

See https://www.keiruaprod.fr/blog/2021/12/29/a-comprehensive-guide-to-aoc-2021-day-24.html"
  (declare (type (simple-array (signed-byte 8) (3 14)) parameters))
  (declare (type boolean high-p))
  (declare (optimize (speed 3)))
  (let ((array (make-array 14 :element-type '(signed-byte 8) :initial-element (if high-p 9 1)))
        (stack (make-array 8 :element-type '(signed-byte 8) :initial-element 0
                             :adjustable T :fill-pointer 0))
        (result (make-string 14 :element-type 'base-char :initial-element (if high-p #\9 #\1))))
    (dotimes (i 14 (prog1 (nth-value 0 (parse-integer result))
                     (when (< 0 (length stack))
                       (error "Finished with a non-empty stack: ~a" stack))))
      (let ((div (aref parameters 0 i)) ;; Divider of the z-value.
            (chk (aref parameters 1 i)) ;; Checker of the x-value.
            (add (aref parameters 2 i))) ;; Addition of the y-value.
        (cond
          ((= div 1) ;; Push into the stack.
           (vector-push-extend add stack)
           (vector-push-extend i stack))
          ((= div 26) ;; Consider the last item on the stack.
           (let ((j (vector-pop stack))
                 (add (vector-pop stack)))
             (setf (aref array i) (+ (aref array j) add chk))
             (cond ;; Correct so that the digit is in 1-9 range.
               ((< 9 (aref array i))
                (setf (aref array j) (- (aref array j) (- (aref array i) 9)))
                (setf (aref array i) 9))
               ((< (aref array i) 1)
                (setf (aref array j) (+ (aref array j) (- 1 (aref array i))))
                (setf (aref array i) 1)))
             (setf (char result i) (digit-char (aref array i)))
             (setf (char result j) (digit-char (aref array j)))))
          (T (error "Unexpected DIV: ~a" div)))))))

(defun d24p1 ()
  (d24-solve (d24-data) T))

;; Answer: 93997999296912

(defun d24p2 ()
  (d24-solve (d24-data) NIL))

;; Answer: 81111379141811
