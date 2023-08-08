#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/19

(defparameter *day19-input* (local-file #P"day19.txt" :error T))

(defstruct (d19-vec (:constructor %make-d19-vec (&optional (x 0) (y 0) (z 0))))
  (x 0 :type (signed-byte 32))
  (y 0 :type (signed-byte 32))
  (z 0 :type (signed-byte 32)))

(defmacro with-d19-vec ((x y z) vec &body body)
  (let ((vec-var (gensym "VEC")))
    `(let* ((,vec-var ,vec)
            (,x (d19-vec-x ,vec-var))
            (,y (d19-vec-y ,vec-var))
            (,z (d19-vec-z ,vec-var)))
       (declare (type (signed-byte 32) ,x ,y ,z))
       ,@body)))

(defmacro with-d19-vecs ((x0 y0 z0 x1 y1 z1) vec-a vec-b &body body)
  (let ((a-var (gensym "VEC"))
        (b-var (gensym "VEC")))
    `(let* ((,a-var ,vec-a)
            (,b-var ,vec-b)
            (,x0 (d19-vec-x ,a-var))
            (,y0 (d19-vec-y ,a-var))
            (,z0 (d19-vec-z ,a-var))
            (,x1 (d19-vec-x ,b-var))
            (,y1 (d19-vec-y ,b-var))
            (,z1 (d19-vec-z ,b-var)))
       (declare (type (signed-byte 32) ,x0 ,y0 ,z0 ,x1 ,y1 ,z1))
       ,@body)))

(declaim (inline d19-vec-clone))
(defun d19-vec-clone (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (%make-d19-vec x y z)))

(declaim (inline d19-vec-set))
(defun d19-vec-set (vec x y z)
  (declare (type d19-vec vec))
  (declare (type (signed-byte 32) x y z))
  (declare (optimize (speed 3)))
  (prog1 vec
    (setf (d19-vec-x vec) x)
    (setf (d19-vec-y vec) y)
    (setf (d19-vec-z vec) z)))

(declaim (inline d19-vec-copy))
(defun d19-vec-copy (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) b
    (d19-vec-set a x y z)))

(declaim (inline d19-vec=))
(defun d19-vec= (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (and (= (d19-vec-x a) (d19-vec-x b))
       (= (d19-vec-y a) (d19-vec-y b))
       (= (d19-vec-z a) (d19-vec-z b))))

(declaim (inline d19-vec-equal))
(defun d19-vec-equal (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vecs (x0 y0 z0 x1 y1 z1) a b
    (let ((x0 (abs x0)) (y0 (abs y0)) (z0 (abs z0))
          (x1 (abs x1)) (y1 (abs y1)) (z1 (abs z1)))
      (or (and (= x0 x1) (or (and (= y0 y1) (= z0 z1))
                             (and (= y0 z1) (= z0 y1))))
          (and (= x0 y1) (or (and (= y0 z1) (= z0 x1))
                             (and (= y0 x1) (= z0 z1))))
          (and (= x0 z1) (or (and (= y0 x1) (= z0 y1))
                             (and (= y0 y1) (= z0 x1))))))))

;; Arithmetic.

(declaim (inline d19-vec+))
(defun d19-vec+ (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vecs (x0 y0 z0 x1 y1 z1) a b
    (%make-d19-vec (+ x0 x1) (+ y0 y1) (+ z0 z1))))

(declaim (inline d19-nvec+))
(defun d19-nvec+ (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vecs (x0 y0 z0 x1 y1 z1) a b
    (d19-vec-set a (+ x0 x1) (+ y0 y1) (+ z0 z1))))

(declaim (inline d19-vec-))
(defun d19-vec- (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vecs (x0 y0 z0 x1 y1 z1) a b
    (%make-d19-vec (- x0 x1) (- y0 y1) (- z0 z1))))

(declaim (inline d19-nvec-))
(defun d19-nvec- (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vecs (x0 y0 z0 x1 y1 z1) a b
    (d19-vec-set a (- x0 x1) (- y0 y1) (- z0 z1))))

(declaim (inline d19-vec-negate))
(defun d19-vec-negate (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (%make-d19-vec (- x) (- y) (- z))))

(declaim (inline d19-nvec-negate))
(defun d19-nvec-negate (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec (- x) (- y) (- z))))

(declaim (inline d19-vec-length))
(defun d19-vec-length (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (let ((x2 (* x x)) (y2 (* y y)) (z2 (* z z)))
      (declare (type (signed-byte 32) x2 y2 z2))
      (sqrt (the (signed-byte 32) (+ x2 y2 z2))))))

(declaim (inline d19-vec-distance))
(defun d19-vec-distance (a b) ;; Manhattan
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vecs (x0 y0 z0 x1 y1 z1) a b
    (the (signed-byte 32) (+ (abs (- x1 x0)) (abs (- y1 y0)) (abs (- z1 z0))))))

(declaim (inline d19-vec-crossp))
(defun d19-vec-crossp (a b)
  (declare (type d19-vec a b))
  (declare (optimize (speed 3)))
  (with-d19-vecs (x0 y0 z0 x1 y1 z1) a b
    (%make-d19-vec (the (signed-byte 32) (- (* y0 z1) (* z0 y1)))
                   (the (signed-byte 32) (- (* z0 x1) (* x0 z1)))
                   (the (signed-byte 32)
                        (- (the (signed-byte 32) (* x0 y1))
                           (the (signed-byte 32) (* y0 x1)))))))

;; Rotations.

(declaim (inline d19-vec-rotate-x-cw))
(defun d19-vec-rotate-x-cw (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec x z (- y))))

(declaim (inline d19-vec-rotate-x-ccw))
(defun d19-vec-rotate-x-ccw (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec x (- z) y)))

(declaim (inline d19-vec-rotate-x-180))
(defun d19-vec-rotate-x-180 (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec x (- y) (- z))))

(declaim (inline d19-vec-mirror-x))
(defun d19-vec-mirror-x (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (setf (d19-vec-x vec) (- (d19-vec-x vec)))
  vec)

(declaim (inline d19-vec-rotate-y-cw))
(defun d19-vec-rotate-y-cw (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec (- z) y x)))

(declaim (inline d19-vec-rotate-y-ccw))
(defun d19-vec-rotate-y-ccw (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec z y (- x))))

(declaim (inline d19-vec-rotate-y-180))
(defun d19-vec-rotate-y-180 (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec (- x) y (- z))))

(declaim (inline d19-vec-mirror-y))
(defun d19-vec-mirror-y (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (setf (d19-vec-y vec) (- (d19-vec-y vec)))
  vec)

(declaim (inline d19-vec-rotate-z-cw))
(defun d19-vec-rotate-z-cw (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec y (- x) z)))

(declaim (inline d19-vec-rotate-z-ccw))
(defun d19-vec-rotate-z-ccw (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec (- y) x z)))

(declaim (inline d19-vec-rotate-z-180))
(defun d19-vec-rotate-z-180 (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (with-d19-vec (x y z) vec
    (d19-vec-set vec (- x) (- y) z)))

(declaim (inline d19-vec-mirror-z))
(defun d19-vec-mirror-z (vec)
  (declare (type d19-vec vec))
  (declare (optimize (speed 3)))
  (setf (d19-vec-z vec) (- (d19-vec-z vec)))
  vec)

(declaim (inline d19-vec-rotations))
(defun d19-vec-rotations (from to)
  (declare (type d19-vec from to))
  (declare (optimize (speed 3)))
  (with-d19-vecs (fx fy fz tx ty tz) from to
    (let ((rots (queue-make)))
      (cond
        ((= tx (- fx))
         (queue-push :z rots)
         (queue-push :180 rots))
        ((= tx fy)
         (queue-push :z rots)
         (queue-push :cw rots))
        ((= tx (- fy))
         (queue-push :z rots)
         (queue-push :ccw rots))
        ((= tx fz)
         (queue-push :y rots)
         (queue-push :ccw rots))
        ((= tx (- fz))
         (queue-push :y rots)
         (queue-push :cw rots)))
      (cond
        ((= ty (- fy))
         (queue-push :x rots)
         (queue-push :180 rots))
        ((= ty fz)
         (queue-push :x rots)
         (queue-push :cw rots))
        ((= ty (- fz))
         (queue-push :x rots)
         (queue-push :ccw rots)))
      (cond
        ((= tz (- fz))
         (queue-push :z rots)
         (queue-push :mirror rots)))
      (queue-as-list rots))))

(declaim (inline d19-vec-rotate))
(defun d19-vec-rotate (vec axis direction)
  (declare (type d19-vec vec))
  (declare (type (or (eql :x) (eql :y) (eql :z) (eql :all)) axis))
  (declare (type (or (eql :cw) (eql :ccw) (eql :180) (eql :mirror)) direction))
  (declare (optimize (speed 3)))
  (case axis
    (:x (case direction
          (:cw (d19-vec-rotate-x-cw vec))
          (:ccw (d19-vec-rotate-x-ccw vec))
          (:180 (d19-vec-rotate-x-180 vec))
          (:mirror (d19-vec-mirror-x vec))))
    (:y (case direction
          (:cw (d19-vec-rotate-y-cw vec))
          (:ccw (d19-vec-rotate-y-ccw vec))
          (:180 (d19-vec-rotate-y-180 vec))
          (:mirror (d19-vec-mirror-y vec))))
    (:z (case direction
          (:cw (d19-vec-rotate-z-cw vec))
          (:ccw (d19-vec-rotate-z-ccw vec))
          (:180 (d19-vec-rotate-z-180 vec))
          (:mirror (d19-vec-mirror-z vec))))
    (:all (ecase direction (:mirror (d19-nvec-negate vec)))))
  vec)

(defstruct (d19-scanner (:constructor %make-d19-scanner (id)))
  (id 0 :type (integer 0 26))
  (location (%make-d19-vec) :type d19-vec)
  (count 0 :type (unsigned-byte 16))
  (reports (make-array #x200 :element-type 'd19-vec :initial-element (%make-d19-vec))
   :type (simple-array d19-vec (#x200)))
  (vectors (make-array '(#x200 #x200) :element-type 'd19-vec :initial-element (%make-d19-vec))
   :type (simple-array d19-vec (#x200 #x200))))

(defmacro with-d19-scanner ((count reports &optional vectors) scanner &body body)
  (let ((scanner-var (gensym "SCANNER")))
    `(let* ((,scanner-var (the d19-scanner ,scanner))
            (,count (d19-scanner-count ,scanner-var))
            (,reports (d19-scanner-reports ,scanner-var))
            ,@(when vectors `((,vectors (d19-scanner-vectors ,scanner-var)))))
       (declare (type (unsigned-byte 16) ,count))
       (declare (type (simple-array d19-vec (#x200)) ,reports))
       ,@(when vectors `((declare (type (simple-array d19-vec (#x200 #x200)) ,vectors))))
       ,@body)))

(defun d19-scanner-calculate-vectors (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-slots (count reports vectors) scanner
    (dotimes (i count scanner)
      (loop for j of-type (unsigned-byte 16) from (1+ i) below count
            for vector = (d19-vec- (aref reports i) (aref reports j))
            do (setf (aref vectors i j) (d19-vec-negate vector))
            do (setf (aref vectors j i) vector)))))

(defun make-d19-scanner (id &rest scans)
  (declare (type (integer 0 26) id))
  (declare (optimize (speed 3)))
  (let ((scanner (%make-d19-scanner id)))
    (loop for scan of-type d19-vec in scans
          for i of-type (unsigned-byte 16) = (d19-scanner-count scanner)
          do (setf (d19-scanner-count scanner) (1+ i))
          do (setf (aref (d19-scanner-reports scanner) i) scan))
    (d19-scanner-calculate-vectors scanner)))

(defun d19-scanner-clone (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (let ((other (%make-d19-scanner (d19-scanner-id scanner))))
    (with-d19-scanner (count reports vectors) scanner
      (setf (d19-scanner-count other) count)
      (dotimes (i count other)
        (setf (aref (d19-scanner-reports other) i) (d19-vec-clone (aref reports i)))
        (dotimes (j count)
          (unless (= i j)
            (setf (aref (d19-scanner-vectors other) i j) (d19-vec-clone (aref vectors i j)))))))))

(defmethod print-object ((scanner d19-scanner) stream)
  (print-unreadable-object (scanner stream :type 'd19-scanner)
    (with-d19-scanner (count reports) scanner
      (format stream ":ID ~d :COUNT ~d :LOCATION ~d~%~t:REPORTS ~a"
              (d19-scanner-id scanner) count (d19-scanner-location scanner)
              (make-array count :element-type 'd19-vec
                                :initial-contents (loop for i from 0 below count
                                                        collect (aref reports i)))))))

(defun d19-scanner-add (scanner report)
  (declare (type d19-scanner scanner))
  (declare (type d19-vec report))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count)
      (when (d19-vec= report (aref reports i))
        (return-from d19-scanner-add)))
    (incf (d19-scanner-count scanner))
    (setf (aref reports count) report)
    (dotimes (i count scanner)
      (let* ((vector (d19-vec- report (aref reports i))))
        (setf (aref vectors i count) vector)
        (setf (aref vectors count i) (d19-vec-negate vector))))))

(defun d19-scanner-move (scanner vector)
  (declare (type d19-scanner scanner))
  (declare (type d19-vec vector))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports) scanner
    (dotimes (i count scanner)
      (d19-nvec+ (aref reports i) vector))))

;; Rotations.

(defun d19-scanner-rotate-x-cw (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-x-cw (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-x-cw (aref vectors i j))))))

(defun d19-scanner-rotate-x-ccw (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-x-ccw (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-x-ccw (aref vectors i j))))))

(defun d19-scanner-rotate-x-180 (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-x-180 (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-x-180 (aref vectors i j))))))

(defun d19-scanner-mirror-x (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-mirror-x (aref reports i))
      (dotimes (j count)
        (d19-vec-mirror-x (aref vectors i j))))))

(defun d19-scanner-rotate-y-cw (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-y-cw (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-y-cw (aref vectors i j))))))

(defun d19-scanner-rotate-y-ccw (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-y-ccw (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-y-ccw (aref vectors i j))))))

(defun d19-scanner-rotate-y-180 (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-y-180 (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-y-180 (aref vectors i j))))))

(defun d19-scanner-mirror-y (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-mirror-y (aref reports i))
      (dotimes (j count)
        (d19-vec-mirror-y (aref vectors i j))))))

(defun d19-scanner-rotate-z-cw (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-z-cw (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-z-cw (aref vectors i j))))))

(defun d19-scanner-rotate-z-ccw (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-z-ccw (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-z-ccw (aref vectors i j))))))

(defun d19-scanner-rotate-z-180 (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-rotate-z-180 (aref reports i))
      (dotimes (j count)
        (d19-vec-rotate-z-180 (aref vectors i j))))))

(defun d19-scanner-mirror-z (scanner)
  (declare (type d19-scanner scanner))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (dotimes (i count scanner)
      (d19-vec-mirror-z (aref reports i))
      (dotimes (j count)
        (d19-vec-mirror-z (aref vectors i j))))))

(defun d19-scanner-rotate (scanner rotations)
  (declare (type d19-scanner scanner))
  (declare (type list rotations))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (loop for axis of-type (or (eql :x) (eql :y) (eql :z) (eql :all)) in rotations by #'cddr
          for direction of-type (or (eql :cw) (eql :ccw) (eql :180) (eql :mirror))
          in (rest rotations) by #'cddr
          do (dotimes (i count scanner)
               (d19-vec-rotate (aref reports i) axis direction)
               (dotimes (j count)
                 (d19-vec-rotate (aref vectors i j) axis direction))))))

(defstruct (d19-data (:constructor %make-d19-data ()))
  (count 0 :type (integer 0 26))
  (scanners (make-array 26 :element-type 'd19-scanner :initial-element (%make-d19-scanner 0))
   :type (simple-array d19-scanner (26))))

(defun make-d19-data (&rest scanners)
  (declare (optimize (speed 3)))
  (loop with data = (%make-d19-data)
        for scanner of-type d19-scanner in scanners
        for i of-type (integer 0 26) = (d19-data-count data)
        do (setf (d19-data-count data) (1+ i))
        do (setf (aref (d19-data-scanners data) i) scanner)
        finally (return data)))

(defmethod print-object ((data d19-data) stream)
  (print-unreadable-object (data stream :type 'd19-data)
    (with-slots (count scanners) data
      (format stream ":COUNT ~d~%:SCANNERS ~a"
              count (make-array count :element-type 'd19-scanner
                                      :initial-contents (loop for i from 0 below count
                                                              collect (aref scanners i)))))))

(defun d19-data ()
  (declare (optimize (speed 3)))
  (let ((id 0))
    (declare (type (integer 0 26) id))
    (flet ((make-scanner (reports)
             (declare (optimize (speed 3)))
             (prog1 (apply #'make-d19-scanner id (queue-as-list reports))
               (incf id))))
      (with-open-file (stream *day19-input* :if-does-not-exist :error)
        (loop with scanners = (queue-make)
              with reports = (queue-make)
              for line of-type (or string (eql :eof)) = (read-line stream NIL :eof)
              do (cond
                   ((eql line :eof)
                    (when (< 0 (queue-length reports))
                      (queue-push (make-scanner reports) scanners)))
                   ((position #\, (the string line) :test #'char=)
                    (loop for i = 0 then (1+ j)
                          for j = (position #\, line :start i :test #'char=)
                          collect (parse-integer (subseq line i j)) into values
                          while j
                          finally (queue-push (apply #'%make-d19-vec values) reports)))
                   ((< 0 (queue-length reports))
                    (queue-push (make-scanner reports) scanners)
                    (setf reports (queue-make))))
              until (eql :eof line)
              finally (return (apply #'make-d19-data (queue-as-list scanners))))))))

(defun d19-match (scanner other)
  (declare (type d19-scanner scanner other))
  (declare (optimize (speed 3)))
  (with-d19-scanner (count reports vectors) scanner
    (declare (ignore reports))
    (with-d19-scanner (other-count other-reports other-vectors) other
      (declare (ignore other-reports))
      (flet ((position-in (item i earlier-matches)
               ;; (declare (optimize (speed 3)))
               (dotimes (index count)
                 (when (and (<= count (aref earlier-matches index))
                            (d19-vec-equal item (aref vectors i index)))
                   (return index)))))
        (dotimes (i count)
          (dotimes (j other-count)
            (let ((match (make-array count :element-type '(unsigned-byte 16) :initial-element #x1ff))
                  (reverse
                    (make-array other-count :element-type '(unsigned-byte 16) :initial-element #x1ff))
                  (match-count 0))
              (declare (type (unsigned-byte 32) match-count))
              (loop for k from 0 below other-count
                    for vector = (aref other-vectors j k)
                    for index = (position-in vector i match)
                    do (when index
                         (setf (aref match index) k)
                         (setf (aref reverse k) index)
                         (incf match-count))
                    finally (when (<= 12 match-count)
                              (return-from d19-match (values i j match reverse)))))))))))

(defun d19-orientated-p (scanner other scanner-index other-index)
  (declare (type d19-scanner scanner other))
  (declare (type (unsigned-byte 16) scanner-index other-index))
  (declare (optimize (speed 3)))
  (with-d19-scanner (scan-count scan-reports scan-vectors) scanner
    (declare (ignore scan-reports))
    (with-d19-scanner (oth-count oth-reports oth-vectors) other
      (declare (ignore oth-reports))
      (loop for i from 0 below scan-count
            for vec = (aref scan-vectors scanner-index i)
            when (dotimes (j oth-count)
                   (when (d19-vec= vec (aref oth-vectors other-index j))
                     (return T)))
            count vec into valid
            finally (return (<= 12 valid))))))

(defun d19-orientate (scanner other)
  (declare (type d19-scanner scanner other))
  (declare (optimize (speed 3)))
  (multiple-value-bind (scanner-index other-index matches reverse)
      (d19-match scanner other)
    (declare (type (or null (simple-array (unsigned-byte 16) (*))) matches reverse))
    (unless scanner-index (return-from d19-orientate))
    (with-d19-scanner (oth-count oth-reports oth-vectors) other
      (with-d19-scanner (scan-count scan-reports scan-vectors) scanner
        (dotimes (i scan-count)
          (let ((match (aref matches i)))
            (when (< match oth-count)
              (let ((vec-to (aref scan-vectors scanner-index i))
                    (vec-from (aref oth-vectors other-index match)))
                (let ((rotations (d19-vec-rotations vec-from vec-to)))
                  (when rotations (d19-scanner-rotate other rotations))))
              (when (d19-orientated-p scanner other scanner-index other-index)
                (let ((to (aref scan-reports scanner-index))
                      (from (aref oth-reports other-index))
                      (new-reports (make-array oth-count :element-type 'd19-vec
                                                         :initial-element (%make-d19-vec)))
                      (new-count 0))
                  (let ((delta (d19-vec- to from)))
                    (setf (d19-scanner-location other) delta)
                    (d19-scanner-move other delta))
                  (dotimes (i oth-count)
                    (unless (< (aref reverse i) oth-count)
                      (setf (aref new-reports new-count) (aref oth-reports i))
                      (incf new-count)))
                  (return-from d19-orientate (values new-reports new-count)))))))))))

(defun d19-unique-reports (count scanners)
  (declare (type (unsigned-byte 32) count))
  (declare (type (simple-array d19-scanner (26)) scanners))
  (declare (optimize (speed 3)))
  (let ((merged (make-array count :element-type 'boolean :initial-element NIL))
        (result (d19-scanner-clone (aref scanners 0)))
        (valid-count 1))
    (declare (type (unsigned-byte 32) valid-count))
    (setf (aref merged 0) T)
    (loop repeat (* count count) ;; Just in case.
          while (< valid-count count)
          do (loop for i from 0 below count
                   for scanner-a = (aref scanners i)
                   for a-merged-p of-type boolean = (aref merged i)
                   ;; do (format T "~&i: ~a~%" i)
                   do (loop for j from (1+ i) below count
                            for scanner-b = (aref scanners j)
                            for b-merged-p of-type boolean = (aref merged j)
                            ;; do (format T "~&~tj: ~a~%" j)
                            do (when (and (/= i j) (not (eql a-merged-p b-merged-p)))
                                 (let ((base (if a-merged-p scanner-a scanner-b))
                                       (other (if b-merged-p scanner-a scanner-b)))
                                   ;; (format T "~&Validating Scanner ~d against Scanner ~d~%"
                                   ;;         (d19-scanner-id other) (d19-scanner-id base))
                                   (multiple-value-bind (new-reports new-count)
                                       (d19-orientate base other)
                                     (declare (type (or null (simple-array d19-vec (*))) new-reports))
                                     (declare (type (or null (unsigned-byte 32)) new-count))
                                     (when (and new-reports (< 0 new-count))
                                       ;; (format T "~&Merged ~d from Scanner ~d~%"
                                       ;;         new-count (d19-scanner-id other))
                                       (setf (aref merged (d19-scanner-id other)) T)
                                       (incf valid-count)
                                       (dotimes (k new-count)
                                         (d19-scanner-add result (aref new-reports k)))
                                       (return)))))))
          finally (return result))))

(defun d19p1 ()
  (declare (optimize (speed 3)))
  (with-slots (count scanners) (d19-data)
    (d19-scanner-count (d19-unique-reports count scanners))))

;; Answer: 313

(defun d19p2 ()
  (declare (optimize (speed 3)))
  (with-slots (count scanners) (d19-data)
    (declare (type (integer 0 26) count))
    (declare (type (simple-array d19-scanner (26)) scanners))
    (d19-unique-reports count scanners)
    (let ((max 0))
      (dotimes (i 26 max)
        (let ((from (d19-scanner-location (aref scanners i))))
          (loop for j from (1+ i) below count
                for to = (d19-scanner-location (aref scanners j))
                for dist = (d19-vec-distance from to)
                when (< max dist) do (setf max dist)))))))

;; Answer: 10656
