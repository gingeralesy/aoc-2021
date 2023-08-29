(in-package #:aoc-2021)

;; https://adventofcode.com/2021/day/22

(defparameter *day22-input* (local-file #P"day22.txt" :error T))

(defparameter *day22-input-re*
  (cl-ppcre:create-scanner
   "^(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)$"))

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

(defstruct (d22-cuboid (:constructor %make-d22-cuboid (on-p x y z width height depth)))
  (on-p NIL :type boolean)
  (x 0 :type (signed-byte 32))
  (y 0 :type (signed-byte 32))
  (z 0 :type (signed-byte 32))
  (width 0 :type (signed-byte 32))
  (height 0 :type (signed-byte 32))
  (depth 0 :type (signed-byte 32)))

(declaim (inline make-d22-null-cuboid))
(declaim (ftype (function () d22-cuboid) make-d22-null-cuboid))
(defun make-d22-null-cuboid ()
  (declare (optimize (speed 3)))
  (%make-d22-cuboid NIL 0 0 0 0 0 0))

(defmethod d22-cuboid-nullp ((cuboid d22-cuboid))
  (declare (optimize (speed 3)))
  (not (and (< 0 (d22-cuboid-width cuboid))
            (< 0 (d22-cuboid-height cuboid))
            (< 0 (d22-cuboid-depth cuboid)))))

(defmethod d22-cuboid-count ((cuboid d22-cuboid))
  ;; (declare (optimize (speed 3)))
  (if (d22-cuboid-nullp cuboid)
      0
      (* (d22-cuboid-width cuboid)
         (d22-cuboid-height cuboid)
         (d22-cuboid-depth cuboid))))

(defmethod d22-cuboid-max-x ((cuboid d22-cuboid))
  (declare (optimize (speed 3)))
  (+ (d22-cuboid-x cuboid) (d22-cuboid-width cuboid)))

(defmethod d22-cuboid-max-y ((cuboid d22-cuboid))
  (declare (optimize (speed 3)))
  (+ (d22-cuboid-y cuboid) (d22-cuboid-height cuboid)))

(defmethod d22-cuboid-max-z ((cuboid d22-cuboid))
  (declare (optimize (speed 3)))
  (+ (d22-cuboid-z cuboid) (d22-cuboid-depth cuboid)))

(defmacro with-d22-cuboid ((x y z width height depth &optional on-p) cuboid &body body)
  (let ((cuboid-var (gensym "D22-CUBOID-")))
    `(let* ((,cuboid-var ,cuboid)
            (,x (d22-cuboid-x ,cuboid-var))
            (,y (d22-cuboid-y ,cuboid-var))
            (,z (d22-cuboid-z ,cuboid-var))
            (,width (d22-cuboid-width ,cuboid-var))
            (,height (d22-cuboid-height ,cuboid-var))
            (,depth (d22-cuboid-depth ,cuboid-var))
            ,@(when on-p `((,on-p (d22-cuboid-on-p ,cuboid-var)))))
       (declare (type (signed-byte 32) ,x ,y ,z ,width ,height ,depth))
       ,@(when on-p `((declare (type boolean ,on-p))))
       (when (d22-cuboid-nullp ,cuboid-var) (error "Invalid cuboid: ~a" ,cuboid-var))
       ,@body)))

(defmacro with-d22-cuboid-edges ((min-x min-y min-z max-x max-y max-z) cuboid &body body)
  (let ((width-var (gensym "D22-WIDTH-"))
        (height-var (gensym "D22-HEIGHT-"))
        (depth-var (gensym "D22-DEPTH-")))
    `(with-d22-cuboid (,min-x ,min-y ,min-z ,width-var ,height-var ,depth-var) ,cuboid
       (let ((,max-x (+ ,min-x ,width-var))
             (,max-y (+ ,min-y ,height-var))
             (,max-z (+ ,min-z ,depth-var)))
         (declare (type (signed-byte 32) ,max-x ,max-y ,max-z))
         ,@body))))

(defmacro with-d22-cuboid-points ((x0 y0 z0 x1 y1 z1 x2 y2 z2 x3 y3 z3
                                   x4 y4 z4 x5 y5 z5 x6 y6 z6 x7 y7 z7)
                                  cuboid &body body)
  (let ((x-var (gensym "D22-X-"))
        (y-var (gensym "D22-Y-"))
        (z-var (gensym "D22-Z-"))
        (max-x-var (gensym "D22-MAX-X-"))
        (max-y-var (gensym "D22-MAX-Y-"))
        (max-z-var (gensym "D22-MAX-Z-")))
    `(with-d22-cuboid-edges (,x-var ,y-var ,z-var ,max-x-var ,max-y-var ,max-z-var) ,cuboid
       (let ((,x0 ,x-var)     (,y0 ,y-var)     (,z0 ,z-var)
             (,x1 ,max-x-var) (,y1 ,y-var)     (,z1 ,z-var)
             (,x2 ,x-var)     (,y2 ,max-y-var) (,z2 ,z-var)
             (,x3 ,x-var)     (,y3 ,y-var)     (,z3 ,max-z-var)
             (,x4 ,max-x-var) (,y4 ,max-y-var) (,z4 ,z-var)
             (,x5 ,max-x-var) (,y5 ,y-var)     (,z5 ,max-z-var)
             (,x6 ,x-var)     (,y6 ,max-y-var) (,z6 ,max-z-var)
             (,x7 ,max-x-var) (,y7 ,max-y-var) (,z7 ,max-z-var))
         (declare (type (signed-byte 32)
                        ,x0 ,y0 ,z0 ,x1 ,y1 ,z1 ,x2 ,y2 ,z2 ,x3 ,y3 ,z3
                        ,x4 ,y4 ,z4 ,x5 ,y5 ,z5 ,x6 ,y6 ,z6 ,x7 ,y7 ,z7))
         (declare (ignorable ,x0 ,y0 ,z0 ,x1 ,y1 ,z1 ,x2 ,y2 ,z2 ,x3 ,y3 ,z3
                             ,x4 ,y4 ,z4 ,x5 ,y5 ,z5 ,x6 ,y6 ,z6 ,x7 ,y7 ,z7))
         ,@body))))

(defmethod d22-cuboid-eql ((cuboid-a d22-cuboid) (cuboid-b d22-cuboid))
  (declare (optimize (speed 3)))
  (or (eql cuboid-a cuboid-b)
      (with-d22-cuboid (x-a y-a z-a width-a height-a depth-a) cuboid-a
        (with-d22-cuboid (x-b y-b z-b width-b height-b depth-b) cuboid-b
          (and (= x-a x-b) (= y-a y-b) (= z-a z-b)
               (= width-a width-b) (= height-a height-b) (= depth-a depth-b))))))

(defmethod d22-cuboid-contains-point-p ((cuboid d22-cuboid) (x integer) (y integer) (z integer))
  (declare (type (signed-byte 32) x y z))
  (declare (optimize (speed 3)))
  (with-d22-cuboid-edges (x0 y0 z0 x1 y1 z1) cuboid
    (and (<= x0 x) (< x x1)
         (<= y0 y) (< y y1)
         (<= z0 z) (< z z1))))

(defmethod d22-cuboid-contains-p ((cuboid-a d22-cuboid) (cuboid-b d22-cuboid))
  (declare (optimize (speed 3)))
  (with-d22-cuboid-edges (x0-a y0-a z0-a x1-a y1-a z1-a) cuboid-a
    (with-d22-cuboid-edges (x0-b y0-b z0-b x1-b y1-b z1-b) cuboid-b
      (and (<= x0-a x0-b) (<= x1-b x1-a)
           (<= y0-a y0-b) (<= y1-b y1-a)
           (<= z0-a z0-b) (<= z1-b z1-a)))))

(defmethod d22-cuboid-collides-p ((cuboid-a d22-cuboid) (cuboid-b d22-cuboid))
  (declare (optimize (speed 3)))
  (with-d22-cuboid-edges (x0-a y0-a z0-a x1-a y1-a z1-a) cuboid-a
    (with-d22-cuboid-edges (x0-b y0-b z0-b x1-b y1-b z1-b) cuboid-b
      (and (< x0-a x1-b) (< x0-b x1-a)
           (< y0-a y1-b) (< y0-b y1-a)
           (< z0-a z1-b) (< z0-b z1-a)))))

(defmethod d22-cuboid-intersection ((cuboid-a d22-cuboid) (cuboid-b d22-cuboid))
  (declare (optimize (speed 3)))
  (with-d22-cuboid-edges (x0-a y0-a z0-a x1-a y1-a z1-a) cuboid-a
    (with-d22-cuboid-edges (x0-b y0-b z0-b x1-b y1-b z1-b) cuboid-b
      (let ((x0 (max x0-a x0-b)) (y0 (max y0-a y0-b)) (z0 (max z0-a z0-b))
            (x1 (min x1-a x1-b)) (y1 (min y1-a y1-b)) (z1 (min z1-a z1-b)))
        (if (and (< x0 x1) (< y0 y1) (< z0 z1))
            (%make-d22-cuboid (d22-cuboid-on-p cuboid-a)
                              x0 y0 z0 (- x1 x0) (- y1 y0) (- z1 z0))
            (make-d22-null-cuboid))))))

(defmethod d22-cuboid-union ((cuboid-a d22-cuboid) (cuboid-b d22-cuboid))
  (declare (optimize (speed 3)))
  (with-d22-cuboid-edges (x0-a y0-a z0-a x1-a y1-a z1-a) cuboid-a
    (with-d22-cuboid-edges (x0-b y0-b z0-b x1-b y1-b z1-b) cuboid-b
      (let ((x0 (min x0-a x0-b)) (y0 (min y0-a y0-b)) (z0 (min z0-a z0-b))
            (x1 (max x1-a x1-b)) (y1 (max y1-a y1-b)) (z1 (max z1-a z1-b)))
        (%make-d22-cuboid (d22-cuboid-on-p cuboid-a)
                          x0 y0 z0 (- x1 x0) (- y1 y0) (- z1 z0))))))

(defmethod d22-cuboid-try-merge ((cuboid-a d22-cuboid) (cuboid-b d22-cuboid))
  (declare (optimize (speed 3)))
  (unless (eql (d22-cuboid-on-p cuboid-a) (d22-cuboid-on-p cuboid-b))
    (return-from d22-cuboid-try-merge NIL))
  (with-d22-cuboid-edges (x0-a y0-a z0-a x1-a y1-a z1-a) cuboid-a
    (with-d22-cuboid-edges (x0-b y0-b z0-b x1-b y1-b z1-b) cuboid-b
      (cond
        ((and (or (= x1-a x0-b) (= x1-b x0-a))
              (= y0-a y0-b) (= y1-a y1-b) (= z0-a z0-b) (= z1-a z1-b))
         (setf (d22-cuboid-x cuboid-a) (min x0-a x0-b))
         (setf (d22-cuboid-width cuboid-a) (- (max x1-a x1-b) (min x0-a x0-b)))
         T)
        ((and (or (= y1-a y0-b) (= y1-b y0-a))
              (= x0-a x0-b) (= x1-a x1-b) (= z0-a z0-b) (= z1-a z1-b))
         (setf (d22-cuboid-y cuboid-a) (min y0-a y0-b))
         (setf (d22-cuboid-height cuboid-a) (- (max y1-a y1-b) (min y0-a y0-b)))
         T)
        ((and (or (= z1-a z0-b) (= z1-b z0-a))
              (= x0-a x0-b) (= x1-a x1-b) (= y0-a y0-b) (= y1-a y1-b))
         (setf (d22-cuboid-z cuboid-a) (min z0-a z0-b))
         (setf (d22-cuboid-depth cuboid-a) (- (max z1-a z1-b) (min z0-a z0-b)))
         T)
        (T NIL)))))

(defmethod d22-cuboid-combine ((cuboid-a d22-cuboid) (cuboid-b d22-cuboid) output)
  (declare (type (vector d22-cuboid) output))
  (declare (optimize (speed 3)))
  (let ((intersection (d22-cuboid-intersection cuboid-a cuboid-b)))
    (unless intersection
      (vector-push-extend cuboid-a output)
      (vector-push-extend cuboid-b output)
      (return-from d22-cuboid-combine NIL))
    (with-d22-cuboid-edges (x0-i y0-i z0-i x1-i y1-i z1-i) intersection
      (with-d22-cuboid-edges (x0-u y0-u z0-u x1-u y1-u z1-u) (d22-cuboid-union cuboid-a cuboid-b)
        (let ((tmp (make-array 27 :element-type 'd22-cuboid
                                  :initial-element (make-d22-null-cuboid)))
              (count 0))
          (declare (type (unsigned-byte 16) count))
          (loop for x0 of-type (signed-byte 32) in (list x0-u x0-i x1-i)
                for x1 of-type (signed-byte 32) in (list x0-i x1-i x1-u)
                do (loop for y0 of-type (signed-byte 32) in (list y0-u y0-i y1-i)
                         for y1 of-type (signed-byte 32) in (list y0-i y1-i y1-u)
                         do (loop for z0 of-type (signed-byte 32) in (list z0-u z0-i z1-i)
                                  for z1 of-type (signed-byte 32) in (list z0-i z1-i z1-u)
                                  for contains-a-p = (d22-cuboid-contains-point-p cuboid-a x0 y0 z0)
                                  for contains-b-p = (d22-cuboid-contains-point-p cuboid-b x0 y0 z0)
                                  do (when (and (or contains-a-p contains-b-p)
                                                (< x0 x1) (< y0 y1) (< z0 z1))
                                       (let ((piece
                                               (%make-d22-cuboid
                                                (d22-cuboid-on-p
                                                 (if contains-a-p cuboid-a cuboid-b))
                                                x0 y0 z0 (- x1 x0) (- y1 y0) (- z1 z0))))
                                         (setf (aref tmp count) piece)
                                         (setf count (1+ count)))))))
          (loop for merged-p = NIL
                do (loop for i from 0 below count
                         do (loop with j = (1+ i)
                                  while (< j count)
                                  do (cond ;; Updates the cuboid at i.
                                       ((d22-cuboid-try-merge (aref tmp i) (aref tmp j))
                                        ;; Move the end of the array to the merged slot.
                                        (setf count (1- count))
                                        (setf (aref tmp j) (aref tmp count))
                                        (setf merged-p T))
                                       (T ;; Only now increment j.
                                        (setf j (1+ j))))))
                while merged-p)
          (dotimes (i count T)
            (vector-push-extend (aref tmp i) output)))))))

(defstruct (d22-space (:constructor %make-d22-space ()))
  (cuboids (make-array 4 :element-type 'd22-cuboid
                         :initial-element (make-d22-null-cuboid)
                         :adjustable T :fill-pointer 0)
   :type (vector d22-cuboid))
  (swap-cuboids (make-array 4 :element-type 'd22-cuboid
                              :initial-element (make-d22-null-cuboid)
                              :adjustable T :fill-pointer 0)
   :type (vector d22-cuboid)))

(defmethod d22-space-swap ((space d22-space))
  (declare (optimize (speed 3)))
  (with-slots (cuboids swap-cuboids) space
    (declare (type (vector d22-cuboid) cuboids swap-cuboids))
    (when (< 0 (length cuboids))
      (error "Content in cuboid vector still"))
    (let ((tmp cuboids))
      (setf cuboids swap-cuboids)
      (setf swap-cuboids tmp))))

(defmethod d22-space-cuboids-array ((space d22-space))
  (declare (optimize (speed 3)))
  (with-slots (cuboids swap-cuboids) space
    (declare (type (vector d22-space) cuboids swap-cuboids))
    (let* ((count (length cuboids))
           (arr (make-array count :element-type 'd22-cuboid
                                  :initial-element (make-d22-null-cuboid))))
      (dotimes (i count)
        (let ((cuboid (vector-pop cuboids)))
          (vector-push-extend cuboid swap-cuboids)
          (setf (aref arr i) cuboid)))
      (d22-space-swap space)
      (values arr count))))

(defmethod d22-space-push ((space d22-space) (cuboid d22-cuboid))
  (declare (optimize (speed 3)))
  (with-slots (cuboids swap-cuboids) space
    (declare (type (vector d22-cuboid) cuboids swap-cuboids))
    (vector-push-extend cuboid cuboids)
    (unless (< 1 (length cuboids))
      (return-from d22-space-push))
    ;; The loop is pretty inoptimal but in most cases there won't be any changes.
    (loop with original = cuboid
          with rerun-p = NIL
          with tmp = (make-array 4 :element-type 'd22-cuboid
                                   :initial-element (make-d22-null-cuboid)
                                   :adjustable T :fill-pointer 0)
          for cuboid = (vector-pop cuboids) ;; The newly pushed cuboid is on top.
          do (if (d22-cuboid-collides-p original cuboid)
                 (loop while (< 0 (length cuboids))
                       for other = (vector-pop cuboids)
                       for collides-p = (d22-cuboid-collides-p cuboid other)
                       ;; Into temporary space if the other doesn't collide.
                       unless collides-p do (vector-push-extend other tmp)
                       until collides-p
                       finally (cond
                                 (collides-p ;; New cuboids go to stack, the passed two are lost.
                                  (setf rerun-p T) ;; And there's more to handle.
                                  (d22-cuboid-combine cuboid other cuboids))
                                 (T ;; No collision, put it to swap.
                                  (vector-push-extend cuboid swap-cuboids))))
                 (vector-push-extend cuboid swap-cuboids))
          do (loop while (< 0 (length tmp)) ;; Return to be tested.
                   do (vector-push-extend (vector-pop tmp) cuboids))
          do (when (and rerun-p (= 0 (length cuboids)))
               (setf rerun-p NIL)
               (d22-space-swap space))
          while (< 0 (length cuboids))
          finally (d22-space-swap space))))

(defmethod check-d22-space-collisions ((space d22-space))
  (with-slots (cuboids swap-cuboids) space
    (multiple-value-bind (arr count) (d22-space-cuboids-array space)
      (dotimes (i count)
        (loop for j from (1+ i) below count
              do (when (d22-cuboid-collides-p (aref arr i) (aref arr j))
                   (format T "~&Collision between~%    ~a and~%    ~a~%"
                           (aref arr i) (aref arr j))
                   (format T "~&~a~%" arr)
                   (error "Collision between ~a and ~a" (aref arr i) (aref arr j)))))))
  space)

(defun d22p2-data ()
  (declare (optimize (speed 3)))
  (with-open-file (stream *day22-input* :if-does-not-exist :error)
    (loop with space = (%make-d22-space)
          for line = (read-line stream NIL :eof)
          until (eql line :eof)
          do (multiple-value-bind (match groups)
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
                 (declare (type (signed-byte 32) x0 x1 y0 y1 z0 z1))
                 (let ((cuboid (%make-d22-cuboid
                                (string= "on" (aref groups 0))
                                x0 y0 z0 (- (1+ x1) x0) (- (1+ y1) y0) (- (1+ z1) z0))))
                   (d22-space-push space cuboid))))
          finally (return (check-d22-space-collisions space)))))

(defun d22p2 (&optional area)
  (declare (type (or null d22-cuboid) area))
  ;; (declare (optimize (speed 3)))
  (let ((space (d22p2-data)))
    (multiple-value-bind (cuboids count) (d22-space-cuboids-array space)
      (declare (type (simple-array d22-cuboid (*)) cuboids))
      (declare (type (unsigned-byte 32) count))
      (let ((on-count 0))
        (declare (type (unsigned-byte 64) on-count))
        (dotimes (i count)
          (let ((cuboid (aref cuboids i)))
            (when (d22-cuboid-on-p cuboid)
              (setf on-count (logand #xffffffffffffffff
                                     (+ on-count
                                        (the (unsigned-byte 64)
                                             (d22-cuboid-count
                                              (if area
                                                  (d22-cuboid-intersection cuboid area)
                                                  cuboid)))))))))
        on-count))))

;; Answer: 1201259791805392
