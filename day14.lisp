#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day14-input* (local-file #P"day14.txt" :error T))

(defun d14-data ()
  (with-open-file (stream *day14-input* :if-does-not-exist :error)
    (values
     (loop for ch across (read-line stream NIL) collecting ch)
     (loop with scanner = (cl-ppcre:create-scanner "^(\\w+) -> (\\w+)$")
           for line = (read-line stream NIL)
           while line
           when (cl-ppcre:scan scanner line)
           collect (cl-ppcre:register-groups-bind (from to)
                       (scanner line)
                     (unless (and from to) (error "Invalid pair insertion rule: ~a" line))
                     (cons (cons (char from 0) (char from 1))
                           (char to 0)))))))

#|
--- Day 14: Extended Polymerization ---

The incredible pressures at this depth are starting to put a strain on your submarine. The submarine
has polymerization equipment that would produce suitable materials to reinforce the submarine, and
the nearby volcanically-active caves should even have the necessary input elements in sufficient
quantities.

The submarine manual contains instructions for finding the optimal polymer formula; specifically, it
offers a polymer template and a list of pair insertion rules (your puzzle input). You just need to
work out what polymer would result after repeating the pair insertion process a few times.

For example:

NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C

The first line is the polymer template - this is the starting point of the process.

The following section defines the pair insertion rules. A rule like AB -> C means that when elements
A and B are immediately adjacent, element C should be inserted between them. These insertions all
happen simultaneously.

So, starting with the polymer template NNCB, the first step simultaneously considers all three
pairs:

- The first pair (NN) matches the rule NN -> C, so element C is inserted between the first N and the
  second N.
- The second pair (NC) matches the rule NC -> B, so element B is inserted between the N and the C.
- The third pair (CB) matches the rule CB -> H, so element H is inserted between the C and the B.

Note that these pairs overlap: the second element of one pair is the first element of the next pair.
Also, because all pairs are considered simultaneously, inserted elements are not considered to be
part of a pair until the next step.

After the first step of this process, the polymer becomes NCNBCHB.

Here are the results of a few steps using the above rules:

Template:     NNCB
After step 1: NCNBCHB
After step 2: NBCCNBBBCBHCB
After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

This polymer grows quickly. After step 5, it has length 97; After step 10, it has length 3073. After
step 10, B occurs 1749 times, C occurs 298 times, H occurs 161 times, and N occurs 865 times; taking
the quantity of the most common element (B, 1749) and subtracting the quantity of the least common
element (H, 161) produces 1749 - 161 = 1588.

Apply 10 steps of pair insertion to the polymer template and find the most and least common elements
in the result. What do you get if you take the quantity of the most common element and subtract the
quantity of the least common element?
|#

(defun d14-match (a b rules)
  (loop for (from . to) in rules
        when (and (char= a (car from)) (char= b (cdr from)))
        do (return-from d14-match to)))

(defun d14p1 (&optional (steps 10))
  (multiple-value-bind (template rules)
      (d14-data)
    (let ((polymer (copy-list template)))
      (dotimes (i steps)
        (loop for prev = polymer then next
              for next = (rest polymer) then (rest next)
              while next
              for new = (cons (d14-match (car prev) (car next) rules) next)
              when new
              do (setf (cdr prev) new)))
      (loop with counts = NIL
            for ch in polymer
            for count = (assoc ch counts :test #'char=)
            do (if count
                   (rplacd (assoc ch counts :test #'char=) (1+ (cdr count)))
                   (push (cons ch 1) counts))
            finally (return (loop with min = (first counts)
                                  with max = (first counts)
                                  for count in (rest counts)
                                  when (< (cdr count) (cdr min))
                                  do (setf min count)
                                  when (< (cdr max) (cdr count))
                                  do (setf max count)
                                  finally (return (values (- (cdr max) (cdr min))
                                                          min max counts))))))))

;; Answer: 2223

#|
--- Part Two ---

The resulting polymer isn't nearly strong enough to reinforce the submarine. You'll need to run more
steps of the pair insertion process; a total of 40 steps should do it.

In the above example, the most common element is B (occurring 2192039569602 times) and the least
common element is H (occurring 3849876073 times); subtracting these produces 2188189693529.

Apply 40 steps of pair insertion to the polymer template and find the most and least common elements
in the result. What do you get if you take the quantity of the most common element and subtract the
quantity of the least common element?
|#

(defun d14p2 (&optional (steps 40))
  (multiple-value-bind (template rules)
      (d14-data)
    ;; The part 1 solution obviously won't work. Instead keep track of the number of pairs.
    (flet ((add-pair (a b to &optional (increase 1))
             (loop for slot in to
                   for pair = (car slot)
                   do (when (and (char= a (car pair)) (char= b (cdr pair)))
                        (incf (cdr slot) increase)
                        (return-from add-pair to)))
             (push (cons (cons a b) increase) to)
             to))
      (let ((pairs NIL))
        (loop for prev = template then next
              for next = (rest template) then (rest next)
              while next
              do (setf pairs (add-pair (car prev) (car next) pairs)))
        (dotimes (i steps)
          (loop with new-pairs = NIL
                for ((a . b) . count) in pairs
                when (< 0 count)
                do (let ((new (d14-match a b rules)))
                     (when new
                       (setf new-pairs (add-pair a new new-pairs count))
                       (setf new-pairs (add-pair new b new-pairs count))))
                finally (setf pairs new-pairs)))
        (let ((counts (list (cons (first template) 1))))
          (loop for (pair . count) in pairs
                for ch = (cdr pair)
                for old = (assoc ch counts :test #'char=)
                when (< 0 count)
                do (if old
                       (rplacd (assoc ch counts :test #'char=) (+ (cdr old) count))
                       (push (cons ch count) counts)))
          (loop with min = (first counts)
                with max = (first counts)
                for slot in counts
                for count = (cdr slot)
                when (< count (cdr min)) do (setf min slot)
                when (< (cdr max) count) do (setf max slot)
                finally (return (values (- (cdr max) (cdr min))
                                        min max counts))))))))

;; Answer: 2566282754493
