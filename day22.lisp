(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/21

(defparameter *day22-input* (local-file #P"day22.txt" :error T))

(defparameter *day22-input-re*
  (cl-ppcre:create-scanner "^(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)"))

(defstruct (d22-core (:constructor %make-d22-core ()))
  (min-x 0 :type (signed-byte 32))
  (max-x 0 :type (signed-byte 32))
  (min-y 0 :type (signed-byte 32))
  (max-y 0 :type (signed-byte 32))
  (min-z 0 :type (signed-byte 32))
  (max-z 0 :type (signed-byte 32))
  (on-count 0 :type (unsigned-byte 32))
  (cubes NIL :type (or null (simple-array bit (* * *)))))

(defstruct (d22-step (:constructor %make-d22-step (on-p x-from x-to y-from y-to z-from z-to)))
  (on-p NIL :type boolean)
  (x-from 0 :type (signed-byte 32))
  (x-to 0 :type (signed-byte 32))
  (y-from 0 :type (signed-byte 32))
  (y-to 0 :type (signed-byte 32))
  (z-from 0 :type (signed-byte 32))
  (z-to 0 :type (signed-byte 32)))

(defstruct (d22-process (:constructor %make-d22-process (core steps)))
  (current 0 :type (unsigned-byte 16))
  (core (%make-d22-core) :type d22-core)
  (steps (make-array 0 :element-type 'd22-step :initial-element (%make-d22-step NIL 0 0 0 0 0 0))
   :type (simple-array d22-step (*))))

(defmethod d22-normalize ((process d22-process))
  (declare (optimize (speed 3)))
  (with-slots (core steps) process
    (declare (type (simple-array d22-step (*)) steps))
    (with-slots (min-x max-x min-y max-y min-z max-z) core
      (declare (type (signed-byte 32) min-x max-x min-y max-y min-z max-z))
      (loop for step across steps
            do (with-slots (x-from x-to y-from y-to z-from z-to) step
                 (declare (type (signed-byte 32) x-from x-to y-from y-to z-from z-to))
                 (setf x-from (- x-from min-x))
                 (setf x-to (- x-to min-x))
                 (setf x-to (1+ x-to))
                 (setf y-from (- y-from min-y))
                 (setf y-to (- y-to min-y))
                 (setf y-to (1+ y-to))
                 (setf z-from (- z-from min-z))
                 (setf z-to (- z-to min-z))
                 (setf z-to (1+ z-to))))
      (setf max-x (- max-x min-x))
      (setf min-x 0)
      (setf max-x (1+ max-x))
      (setf max-y (- max-y min-y))
      (setf min-y 0)
      (setf max-y (1+ max-y))
      (setf max-z (- max-z min-z))
      (setf min-z 0)
      (setf max-z (1+ max-z))))
  process)

(defmethod d22-do-step ((process d22-process))
  (declare (optimize (speed 3)))
  (with-slots (current core steps) process
    (declare (type (unsigned-byte 16) current))
    (declare (type (simple-array d22-step (*)) steps))
    (with-slots (min-x max-x min-y max-y min-z max-z on-count cubes) core
      (declare (type (signed-byte 32) min-x max-x min-y max-y min-z max-z))
      (declare (type (unsigned-byte 32) on-count))
      (declare (type (simple-array bit (* * *)) cubes))
      (let ((step (aref steps current)))
        (with-slots (on-p x-from x-to y-from y-to z-from z-to) step
          (declare (type (signed-byte 32) x-from x-to y-from y-to z-from z-to))
          (loop with switch-to of-type bit = (if on-p 1 0)
                for z of-type (signed-byte 32) from z-from below z-to
                while (< z max-z)
                when (<= min-z z)
                do (loop for y from y-from below y-to
                         while (< y max-y)
                         when (<= min-y y)
                         do (loop for x from x-from below x-to
                                  while (< x max-x)
                                  when (<= min-x x)
                                  do (let ((prev-state (aref cubes z y x)))
                                       (unless (= switch-to prev-state)
                                         (setf (aref cubes z y x) switch-to)
                                         (setf on-count (logand #xffffffff (if (zerop switch-to)
                                                                               (1- on-count)
                                                                               (1+ on-count))))))))
                finally (return
                          (progn
                            (setf current (1+ current))
                            (< current (length steps))))))))))

(defun d22-data ()
  (declare (optimize (speed 3)))
  (with-open-file (stream *day22-input* :if-does-not-exist :error)
    (let ((core (%make-d22-core)))
      (with-slots (min-x max-x min-y max-y min-z max-z) core
        (loop for line = (read-line stream NIL :eof)
              until (eql line :eof)
              count line into count
              collect (multiple-value-bind (match groups)
                          (cl-ppcre:scan-to-strings *day22-input-re* line)
                        (declare (type (or null string) match))
                        (declare (type (or null (simple-array (simple-array character (*)) (7)))
                                       groups))
                        (unless match (error "Invalid input: ~a" line))
                        (let ((x0 (parse-integer (aref groups 1)))
                              (x1 (parse-integer (aref groups 2)))
                              (y0 (parse-integer (aref groups 3)))
                              (y1 (parse-integer (aref groups 4)))
                              (z0 (parse-integer (aref groups 5)))
                              (z1 (parse-integer (aref groups 6))))
                          (when (< x0 min-x) (setf min-x x0))
                          (when (< max-x x1) (setf max-x x1))
                          (when (< y0 min-y) (setf min-y y0))
                          (when (< max-y y1) (setf max-y y1))
                          (when (< z0 min-z) (setf min-z z0))
                          (when (< max-z z1) (setf max-z z1))
                          (%make-d22-step (string= "on" (aref groups 0))
                                          x0 x1 y0 y1 z0 z1)))
              into steps
              finally (return (%make-d22-process core (make-array count
                                                                  :element-type 'd22-step
                                                                  :initial-contents steps))))))))

(defun d22p1 ()
  (let ((process (d22-data)))
    (with-slots (core steps) process
      (with-slots (min-x max-x min-y max-y min-z max-z on-count cubes) core
        (setf min-x -50 max-x 50
              min-y -50 max-y 50
              min-z -50 max-z 50)
        (d22-normalize process)
        (setf cubes (make-array '(101 101 101) :element-type 'bit))
        (loop while (d22-do-step process))
        on-count))))

;; Answer: 611176
