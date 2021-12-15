#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defun local-file (filename &key error)
  (let ((file (asdf:system-relative-pathname :aoc-2021 filename)))
    (when (and error (not (probe-file file)))
      (error "Missing file: ~a" filename))
    file))

(defun queue-make ()
  (cons NIL NIL)) ;; (HEAD . TAIL)

(defun queue-length (queue)
  (length (car queue)))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun queue-push (obj queue)
  (if (cdr queue)
      (setf (cddr queue) (cons obj NIL)
            (cdr queue) (cddr queue))
      (setf (cdr queue) (cons obj NIL)
            (car queue) (cdr queue)))
  queue)

(defun queue-pop (queue)
  (when (car queue)
    (let ((obj (caar queue)))
      (setf (car queue) (cdar queue))
      (unless (car queue)
        (setf (cdr queue) NIL))
      obj)))

(defun pqueue-pop (queue &optional (key #'identity) (test #'<))
  (when (car queue)
    (loop with min-prev = NIL
          with min = (car queue)
          for cur = (car queue) then next
          for next = (cdr cur) then (cdr next)
          while next
          do (when (funcall test (funcall key (car next)) (funcall key (car min)))
               (setf min-prev cur)
               (setf min next))
          finally (progn
                    (if min-prev
                        (setf (cdr min-prev) (cdr min))
                        (setf (car queue) (cdr min)))
                    (unless (cdr min)
                      (setf (cdr queue) min-prev))
                    (return (car min))))))

(defun queue-as-list (queue &optional copy)
  (if copy (copy-list (car queue)) (car queue)))

(defun queue-copy (queue)
  (let ((copy (queue-make)))
    (loop for item in (queue-as-list queue)
          do (queue-push item copy)
          finally (return copy))))

(defun queue-find (item queue &optional (key #'identity) (test #'eql))
  (find item (car queue) :key key :test test))
