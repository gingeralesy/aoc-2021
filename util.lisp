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

(defun queue-as-list (queue &optional copy)
  (if copy (copy-list (car queue)) (car queue)))

(defun queue-copy (queue)
  (let ((copy (queue-make)))
    (loop for item in (queue-as-list queue)
          do (queue-push item copy)
          finally (return copy))))

(defun queue-find (item queue)
  (find item (car queue)))
