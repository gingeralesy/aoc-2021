(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/20

(defparameter *day20-input* (local-file #P"day20.txt" :error T))

(defun d20-copy-bits (old-data old-size new-data new-size)
  (declare (type (simple-bit-vector *) old-data new-data))
  (declare (type (unsigned-byte 16) old-size new-size))
  (declare (optimize (speed 3)))
  (let* ((diff (- new-size old-size))
         (old-index 0)
         (new-index (+ (floor diff 2) (* new-size (floor diff 2)))))
    (declare (type (unsigned-byte 16) diff old-index new-index))
    (dotimes (y old-size)
      (dotimes (x old-size)
        (setf (bit new-data new-index) (bit old-data old-index))
        (incf old-index)
        (incf new-index))
      (incf new-index diff))))

(defstruct (d20-image (:constructor %make-d20-image ()))
  "Image data to enhance.

SIDE - size of the side of the actual image in the middle of the buffer
SIZE - size of the side of the entire buffer always at least 2 larger than SIDE
ALGORITHM - 512 bits to enhance by
DATA - the entire image buffer"
  (side 0 :type (unsigned-byte 16))
  (size 128 :type (unsigned-byte 16))
  (algorithm (make-array 512 :element-type 'bit) :type (simple-bit-vector 512))
  (data (make-array (* 128 128) :element-type 'bit) :type (simple-bit-vector *)))

(defmacro do-d20-image ((bit image &optional return-value) &body body)
  (let ((image-var (gensym "D20-IMAGE-"))
        (index-var (gensym "INDEX"))
        (side-var (gensym "SIDE"))
        (data-var (gensym "DATA"))
        (diff-var (gensym "DIFF")))
    `(let* ((,image-var ,image)
            (,side-var (d20-image-side ,image-var))
            (,data-var (d20-image-data ,image-var))
            (,index-var (d20-coord->index ,image-var 0 0))
            (,diff-var (- (d20-image-size ,image-var) ,side-var)))
       (declare (type (unsigned-byte 16) ,side-var ,diff-var ,index-var))
       (declare (type (simple-bit-vector *) ,data-var))
       (loop repeat ,side-var
             do (loop repeat ,side-var
                      for ,bit = (bit ,data-var ,index-var)
                      do (progn ,@body)
                      do (incf ,index-var))
             do (incf ,index-var ,diff-var)
             finally (return ,return-value)))))

(defmethod d20-coord->index ((image d20-image) (x integer) (y integer))
  (declare (type (signed-byte 32) x y))
  (declare (optimize (speed 3)))
  (with-slots (side size) image
    (declare (type (unsigned-byte 16) side size))
    (let ((diff (- size side)))
      (+ (floor diff 2) (* size (floor diff 2)) (* y size) x))))

(defmethod d20-expand ((image d20-image))
  (declare (optimize (speed 3)))
  (with-slots (size data) image
    (declare (type (unsigned-byte 16) size))
    (declare (type (simple-bit-vector *) data))
    (let* ((new-size (* size 2))
           (new-data (make-array (* new-size new-size) :element-type 'bit
                                                       :initial-element (bit data 0))))
      (declare (type (unsigned-byte 16) new-size))
      (d20-copy-bits data size new-data new-size)
      (setf data new-data)
      (setf size new-size)
      image)))

(defmethod d20-part ((image d20-image) (x integer) (y integer))
  (declare (type (signed-byte 32) x y))
  (declare (optimize (speed 3)))
  (with-slots (side size data) image
    (declare (type (unsigned-byte 16) side size))
    (declare (type (simple-bit-vector *) data))
    (loop with value of-type (unsigned-byte 9) = 0
          with shift of-type (signed-byte 8) = 9
          with diff of-type (unsigned-byte 16) = (- size side)
          with index of-type (signed-byte 32) = (d20-coord->index image (1- x) (1- y))
          with outside = (bit data (d20-coord->index image -1 -1))
          repeat 3
          do (loop repeat 3
                   do (decf shift)
                   do (setf value (logior value (logand #x1ff (ash (bit data index) shift))))
                   do (incf index))
          do (incf index (- size 3))
          finally (return value))))

(defmethod d20-enhance ((image d20-image))
  (declare (optimize (speed 3)))
  (with-slots (side size algorithm data) image
    (declare (type (unsigned-byte 16) side size))
    (declare (type (simple-bit-vector 512) algorithm))
    (declare (type (simple-bit-vector *) data))
    (setf side (+ side 2))
    (unless (< side size)
      (d20-expand image))
    (let* ((diff (- size side))
           (index (d20-coord->index image 0 0))
           (outside (bit algorithm (if (zerop (bit data 0)) 0 511)))
           (new-data (make-array (* size size) :element-type 'bit :initial-element outside)))
      (declare (type (unsigned-byte 16) diff index))
      (dotimes (y side)
        (dotimes (x side)
          (let ((part (d20-part image x y)))
            (declare (type (unsigned-byte 9) part))
            (setf (bit new-data index) (bit algorithm part)))
          (incf index))
        (incf index diff))
      (bit-xor data data T)
      (bit-ior data new-data T))
    image))

(defmethod d20-to-string ((image d20-image))
  (declare (optimize (speed 3)))
  (with-slots (side size data) image
    (declare (type (unsigned-byte 16) side size))
    (declare (type (simple-bit-vector *) data))
    (declare (optimize (speed 3)))
    (let* ((diff (- size side))
           (index (+ (floor diff 2) (* size (floor diff 2))))
           (result (make-array (+ (* side side) (1- side)) :element-type 'character
                                                           :initial-element #\Newline)))
      (declare (type (unsigned-byte 16) diff index))
      (dotimes (y side result)
        (declare (type (unsigned-byte 16) y))
        (dotimes (x side)
          (declare (type (unsigned-byte 16) x))
          (setf (char result (+ x (* y (1+ side))))
                (if (= 1 (bit data index)) #\# #\.))
          (incf index))
        (incf index diff)))))

(defun d20-data ()
  (declare (optimize (speed 3)))
  (with-open-file (stream *day20-input* :if-does-not-exist :error)
    (let ((image (%make-d20-image)))
      (with-slots (side size algorithm data) image
        (let ((tmp (make-array (* size size) :element-type 'bit)))
          (loop with raw of-type (simple-array character (*)) = (read-line stream)
                for i from 0 below 512
                for ch across raw
                when (or (char= #\. ch) (char= #\# ch))
                do (setf (bit algorithm i) (if (char= ch #\#) 1 0)))
          (read-line stream) ;; Empty line.
          (loop with width of-type (unsigned-byte 16) = 0
                with height of-type (unsigned-byte 16) = 0
                with i of-type (unsigned-byte 16) = 0
                for line of-type (or (simple-array character (*)) (eql :eof)) =
                (read-line stream NIL :eof)
                until (eql line :eof)
                do (loop with line-width of-type (unsigned-byte 16) = 0
                         for ch across line
                         when (or (char= #\. ch) (char= #\# ch))
                         do (progn
                              (setf (bit tmp i) (if (char= ch #\#) 1 0))
                              (incf line-width)
                              (incf i))
                         finally (when (< 0 line-width)
                                   (incf height)
                                   (cond
                                     ((< line-width width)
                                      (incf i (- width line-width)))
                                     ((< width line-width)
                                      (setf width line-width)
                                      (unless (zerop (mod width 2))
                                        (incf width)
                                        (incf height)
                                        (incf i))))))
                finally (if (= width height)
                            (setf side width)
                            (error "Width and height mismatch: ~d x ~d" width height)))
          (when (< size side)
            (loop for new-size = (* 2 size)
                  while (< new-size side)
                  finally (setf size new-size))
            (setf data (make-array (* size size) :element-type 'bit)))
          (d20-copy-bits tmp side data size)))
      image)))

(defun d20p1 ()
  (let ((image (d20-data))
        (count 0))
    (loop repeat 2 do (d20-enhance image))
    (do-d20-image (bit image count)
      (unless (zerop bit) (incf count)))))

;; 5846
