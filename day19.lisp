#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day19-input* (local-file #P"day19.txt" :error T))

(defparameter *day19-scanner-re* (cl-ppcre:create-scanner "--- scanner (\\d+) ---"))

(defparameter *day19-report-re* (cl-ppcre:create-scanner "-?\\d+\\b"))

(defstruct d19-point
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer))

(defun d19-point-eql (point other)
  (declare (type d19-point point other))
  (and (= (d19-point-x point) (d19-point-x other))
       (= (d19-point-y point) (d19-point-y other))
       (= (d19-point-z point) (d19-point-z other))))

(defun d19-point-add (point other &optional (destructive-p T))
  (declare (type d19-point point other))
  (let ((x (+ (d19-point-x point) (d19-point-x other)))
        (y (+ (d19-point-y point) (d19-point-y other)))
        (z (+ (d19-point-z point) (d19-point-z other))))
    (cond
      (destructive-p
       (setf (d19-point-x point) x)
       (setf (d19-point-y point) y)
       (setf (d19-point-z point) z)
       point)
      (T (make-d19-point :x x :y y :z z)))))

(defun d19-point-negate (point &optional (destructive-p T))
  (declare (type d19-point point))
  (let ((x (- (d19-point-x point)))
        (y (- (d19-point-y point)))
        (z (- (d19-point-z point))))
    (cond
      (destructive-p
       (setf (d19-point-x point) x)
       (setf (d19-point-y point) y)
       (setf (d19-point-z point) z)
       point)
      (T (make-d19-point :x x :y y :z z)))))

(defun d19-point-sub (point other &optional (destructive-p T))
  (d19-point-add point (d19-point-negate other NIL) destructive-p))

(defun d19-rotate (point axis direction &optional (destructive-p T))
  (declare (type d19-point point))
  (declare (type keyword axis direction))
  (declare (type boolean destructive-p))
  (flet ((set-point (x y z)
           (cond
             (destructive-p
              (setf (d19-point-x point) x)
              (setf (d19-point-y point) y)
              (setf (d19-point-z point) z)
              point)
             (T (make-d19-point :x x :y y :z z)))))
    (ecase direction
      (:cw
       (ecase axis
         (:x (set-point (d19-point-x point)
                        (d19-point-z point)
                        (- (d19-point-y point))))
         (:y (set-point (d19-point-z point)
                        (d19-point-y point)
                        (- (d19-point-x point))))
         (:z (set-point (d19-point-y point)
                        (- (d19-point-x point))
                        (d19-point-z point)))))
      (:ccw
       (ecase axis
         (:x (set-point (d19-point-x point)
                        (- (d19-point-z point))
                        (d19-point-y point)))
         (:y (set-point (- (d19-point-z point))
                        (d19-point-y point)
                        (d19-point-x point)))
         (:z (set-point (- (d19-point-y point))
                        (d19-point-x point)
                        (d19-point-z point))))))))

(defun d19-rotation-test ()
  (let ((point (make-d19-point :x 1)))
    ;; Z axis.
    (assert (=  1 (d19-point-y (d19-rotate point :z :ccw))))
    (assert (= -1 (d19-point-x (d19-rotate point :z :ccw))))
    (assert (= -1 (d19-point-y (d19-rotate point :z :ccw))))
    ;; X axis.
    (assert (= -1 (d19-point-z (d19-rotate point :x :ccw))))
    (assert (=  1 (d19-point-y (d19-rotate point :x :ccw))))
    (assert (=  1 (d19-point-z (d19-rotate point :x :ccw))))
    ;; Y axis.
    (assert (= -1 (d19-point-x (d19-rotate point :y :ccw))))
    (assert (= -1 (d19-point-z (d19-rotate point :y :ccw))))
    (assert (=  1 (d19-point-x (d19-rotate point :y :ccw))))))

(defclass d19-scanner ()
  ((id :initarg :id :accessor id)
   (report-count :initform 0 :accessor report-count)
   (reports :initform NIL :accessor reports)
   (vectors :initform NIL :accessor vectors))
  (:default-initargs :id (error "ID required")))

(defmethod initialize-instance :after ((scanner d19-scanner) &key reports)
  (let* ((count (length reports))
         (vectors (make-array (list count count) :element-type '(or null d19-point)
                                                 :initial-element NIL)))
    (setf (report-count scanner) count)
    (setf (reports scanner) (make-array count :element-type 'd19-point
                                              :initial-contents reports))
    (setf (vectors scanner) vectors)
    (let ((reports (reports scanner)))
      (loop for report across reports
            for i from 0
            do (loop for j from (1+ i)
                     while (< j count)
                     for other = (aref reports j)
                     for vector = (d19-point-sub other report NIL)
                     do (setf (aref vectors i j) vector)
                     do (setf (aref vectors j i) (d19-point-negate vector NIL)))))))

(defmethod d19-orientation ((scanner d19-scanner))
  (make-generator (:final-value NIL)
    (let ((points (reports scanner))
          (vectors (vectors scanner)))
      (labels ((rotate-all (axis)
                 (dotimes (i (report-count scanner))
                   (d19-rotate (aref points i) axis :ccw))
                 (dotimes (i (report-count scanner))
                   (dotimes (j (report-count scanner))
                     (when (/= i j)
                       (setf (aref vectors i j) (d19-point-sub
                                                 (aref points j)
                                                 (aref points i)
                                                 NIL))))))
               (yield-four ()
                 (dotimes (i 4)
                   (yield scanner)
                   (rotate-all :x))))
        (dotimes (i 4)
          (yield-four)
          (rotate-all :y))
        (rotate-all :z)
        (yield-four)
        (rotate-all :z)
        (rotate-all :z)
        (yield-four)
        (rotate-all :z) ;; Return to normal.
        (yield NIL)))))

(defmethod d19-move ((scanner d19-scanner) vector)
  (declare (type d19-point vector))
  (let ((reports (reports scanner)))
    (dotimes (i (report-count scanner))
      (d19-point-add (aref reports i) vector))))

(defmethod d19-matches ((scanner d19-scanner) (other d19-scanner))
  (flet ((find-vector (vector i others)
           (declare (type d19-point vector))
           (declare (type array others))
           (let ((depth (array-dimension others 1)))
             (dotimes (j depth)
               (when (and (/= i j) (d19-point-eql vector (aref others i j)))
                 (return-from find-vector T))))))
    (dotimes (i (- (report-count scanner) 10))
      (dotimes (j (report-count other))
        (loop for k from 0 below (report-count other)
              for vector = (aref (vectors other) j k)
              when (and (/= j k) (find-vector vector i (vectors scanner)))
              count vector into matches
              unless (< matches 11)
              do (return-from d19-matches T))))))

(defmethod d19-validate ((scanner d19-scanner) valid-scanners)
  (let ((orientation (d19-orientation scanner)))
    (loop for scanner = (next orientation)
          while scanner
          do (loop for valid in valid-scanners
                   when (d19-matches scanner valid)
                   do (loop for point across (reports scanner)
                            do (loop for target across (reports valid)
                                     do (d19-move scanner (d19-point-sub target point NIL))
                                     do (loop for report across (reports scanner)
                                              when (find report (reports valid)
                                                         :test #'d19-point-eql)
                                              count report into matches
                                              unless (< matches 12)
                                              do (return-from d19-validate scanner)))
                            finally (error "Couldn't validate position despite the match."))))))

(defun d19-validate-all (sensors)
  (flet ((validate-all (sensors valid)
           (let ((valid (copy-list valid)))
             (loop with unknown = sensors
                   for prev = NIL then current
                   for current = unknown then (rest current)
                   while current
                   do (when (d19-validate (car current) valid)
                        (push (car current) valid)
                        (if prev
                            (setf (cdr prev) (cdr current))
                            (setf unknown (cdr unknown))))
                   finally (return (values valid unknown))))))
    (loop with prev-unknown = (1- (length sensors))
          with validated = (list (first sensors))
          for current = (rest sensors) then unknown
          for (valid unknown) = (multiple-value-list (validate-all current validated))
          unless (< (length unknown) prev-unknown) ;; TODO: Error instead.
          do (error "Could not combine all of the sensors.")
          ;; do (return-from d19-validate-all (values validated unknown))
          do (setf validated valid)
          do (setf prev-unknown (length unknown))
          while unknown
          finally (return validated))))

(defun d19-data ()
  (with-open-file (stream *day19-input* :if-does-not-exist :error)
    (flet ((parse (point)
             (make-d19-point :x (parse-integer (first point))
                             :y (parse-integer (second point))
                             :z (parse-integer (third point)))))
      (loop with scanners = NIL
            with current = NIL
            with reports = NIL
            with count = 0
            for line = (read-line stream NIL :eof)
            until (eql :eof line)
            for scanner-p = (cl-ppcre:scan-to-strings *day19-scanner-re* line)
            for numbers = (cl-ppcre:all-matches-as-strings *day19-report-re* line)
            do (cond
                 (scanner-p
                  (when current
                    (unless reports (error "No reports for scanner: ~a" current))
                    (push (make-instance 'd19-scanner :id current :reports (nreverse reports))
                          scanners))
                  (setf current (parse-integer (first numbers)))
                  (incf count))
                 (numbers (push (parse numbers) reports)))
            finally (return
                      (progn
                        (unless current (error "No scanners"))
                        (unless reports (error "No reports for scanner: ~a" current))
                        (push (make-instance 'd19-scanner :id current :reports (nreverse reports))
                              scanners)
                        (values (nreverse scanners) count)))))))

(defun d19p1 ()
  (loop for to-validate = (d19-data) then unknown
        for (valid unknown) = (multiple-value-list (d19-validate-all to-validate))
        for unique = (loop for report across (reports (first valid)) collecting report)
        do (loop for other in (rest valid)
                 do (loop for report across (reports other)
                          unless (find report unique :test #'d19-point-eql)
                          do (push report unique)))
        summing (length unique)
        while unknown))
