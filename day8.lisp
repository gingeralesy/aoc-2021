#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day8-input* (local-file #P"day8.txt" :error T))

(defun d8-ch-to-signal (ch)
  "Converts the individual code character into its signal value."
  (ecase ch (#\a 1) (#\b 2) (#\c 4) (#\d 8) (#\e 16) (#\f 32) (#\g 64)))

(defun d8-signal-to-ch (number)
  "Converts the individual code character's signal value into its character."
  (ecase number (0 NIL) (1 #\a) (2 #\b) (4 #\c) (8 #\d) (16 #\e) (32 #\f) (64 #\g)))

(defun d8-code-to-signal (code)
  "Converts a code to its signal value."
  (loop with segment = 0
        for ch across code
        do (incf segment (d8-ch-to-signal ch))
        finally (return (logand segment #x7f))))

(defun d8-signal-to-code (number)
  "Converts a signal value to its code."
  (loop for n from 0 below 7
        for ch = (d8-signal-to-ch (logand number (ash 1 n)))
        when ch collect ch into characters
        finally (return (format NIL "~{~c~}" characters))))

(defun d8-data ()
  "Returns a list of entries where each entry is an array of ten signal, one for each digit, and
four codes to solve."
  (with-open-file (stream *day8-input* :if-does-not-exist :error)
    (let ((scanner (cl-ppcre:create-scanner "\\w+")))
      (loop for line = (read-line stream NIL)
            for entry = (make-array 14 :element-type '(unsigned-byte 7) :initial-element 0)
            for count = 0
            while line
            do (cl-ppcre:do-matches-as-strings (match scanner line)
                 (setf (aref entry count) (d8-code-to-signal match))
                 (incf count))
            collect entry into values
            finally (return values)))))

#|
--- Day 8: Seven Segment Search ---

You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it.
Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to
press on.

As your submarine slowly makes its way through the cave system, you notice that the four-digit
seven-segment displays in your submarine are malfunctioning; they must have been damaged during the
escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.

Each digit of a seven-segment display is rendered by turning on or off any of seven segments named
a through g:

0:      1:      2:      3:      4:
aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
gggg    ....    gggg    gggg    ....

5:      6:      7:      8:      9:
aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
gggg    gggg    ....    gggg    gggg

So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7,
only segments a, c, and f would be turned on.

The problem is that the signals which control the segments have been mixed up on each display. The
submarine is still trying to display numbers by producing output on signal wires a through g, but
those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up
separately for each four-digit display! (All of the digits within a display use the same
connections, though.)

So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b
and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f
are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which
segment (c/f). For that, you'll need to collect more information.

For each display, you watch the changing signals for a while, make a note of all ten unique signal
patterns you see, and then write down a single four digit output value (your puzzle input). Using
the signal patterns, you should be able to work out which pattern corresponds to which digit.

For example, here is what you might see in a single entry in your notes:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf

Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output
value. Within an entry, the same wire/segment connections are used (but you don't know what the
connections actually are). The unique signal patterns correspond to the ten different ways the
submarine tries to render a digit using the current wire/segment connections. Because 7 is the only
digit that uses three segments, dab in the above example means that to render a 7, signal lines d,
a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4,
signal lines e, a, f, and b are on.

Using this information, you should be able to work out which combination of signal wires corresponds
to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the
above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and
are more difficult to deduce.

For now, focus on the easy digits. Consider this larger example:

be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce

Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell
which combinations of signals correspond to those digits. Counting only digits in the output values
(the part after | on each line), in the above example, there are 26 instances of digits that use a
unique number of segments (highlighted above).

In the output values, how many times do digits 1, 4, 7, or 8 appear?
|#

(defun d8-count-active (value)
  (loop for n from 0 below 7 count (< 0 (logand value (ash 1 n)))))

(defun d8p1 ()
  (loop for entry in (d8-data)
        sum (loop for i from 10 below 14
                  for segment = (aref entry i)
                  for signals = (d8-count-active segment)
                  count (or (= signals 2) (= signals 3) (= signals 4) (= signals 7)))))

;; Answer: 301

#|
--- Part Two ---

Through a little deduction, you should now be able to determine the remaining digits. Consider again
the first example above:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf

After some careful analysis, the mapping between signal wires and segments only make sense in the
following configuration:

dddd
e    a
e    a
ffff
g    b
g    b
cccc

So, the unique signal patterns would correspond to the following digits:

acedgfb: 8
cdfbe:   5
gcdfa:   2
fbcad:   3
dab:     7
cefabd:  9
cdfgeb:  6
eafb:    4
cagedb:  0
ab:      1

Then, the four digits of the output value can be decoded:

cdfeb: 5
fcadb: 3
cdfeb: 5
cdbaf: 3

Therefore, the output value for this entry is 5353.

Following this same process for each entry in the second, larger example above, the output value of
each entry can be determined:

fdgacbe cefdb cefbgd gcbe: 8394
fcgedb cgb dgebacf gc:     9781
cg cg fdcagb cbg:          1197
efabcd cedba gadfec cb:    9361
gecf egdcabf bgf bfgea:    4873
gebdcfa ecba ca fadegcb:   8418
cefg dcbef fcge gbcadfe:   4548
ed bcgafe cdgba cbgef:     1625
gbdfcae bgc cg cgb:        8717
fgae cfgab fg bagce:       4315

Adding all of the output values in this larger example produces 61229.

For each entry, determine all of the wire/segment connections and decode the four-digit output
values. What do you get if you add up all of the output values?
|#

(defun d8-solve (entry)
  "This is the silly solution where we deduce everything step by step."
  ;; TODO: The cool solution where we traverse a tree of possibilities.
  (let ((digit-signal (make-array 10 :element-type '(unsigned-byte 7) :initial-element 0))
        (digit-side (make-array 7 :element-type '(unsigned-byte 7) :initial-element 0)))
    (labels ((get-signal (number)
               (aref digit-signal number))
             (set-signal (number value)
               (let ((value (logand value #x7f)))
                 (loop for n from 0
                       until (= value (aref entry n))
                       when (= 10 n) do (error "Invalid signal: ~a" value))
                 (setf (aref digit-signal number) value)))
             (get-side-index (side)
               (ecase side
                 (:top 0)
                 (:top-left 1)
                 (:top-right 2)
                 (:middle 3)
                 (:bottom-left 4)
                 (:bottom-right 5)
                 (:bottom 6)))
             (get-side (side)
               (let ((value (aref digit-side (get-side-index side))))
                 (unless (< 0 value) (error "Side not found yet: ~a" side))
                 value))
             (get-sides (side &rest sides)
               (let ((value (get-side side)))
                 (loop for another in sides
                       do (incf value (get-side another)))
                 value))
             (set-side (side value)
               (let ((value (logand value #x7f)))
                 (unless (= 1 (d8-count-active value))
                   (error "Invalid side value: ~a ~a" side value))
                 (setf (aref digit-side (get-side-index side)) value))))
      ;; 1. Find 1, 4, 7, and 8.
      (loop for n from 0 below 10
            for signal = (aref entry n)
            for active = (d8-count-active signal)
            do (case active
                 (2 (set-signal 1 signal))
                 (4 (set-signal 4 signal))
                 (3 (set-signal 7 signal))
                 (7 (set-signal 8 signal))))
      ;; We have signals: 1 4 7 8
      ;; We have sides  :
      ;; 2. Compare 1 and 7 to find the top.
      (set-side :top (logxor (get-signal 1) (get-signal 7)))
      ;; We have signals: 1 4 7 8
      ;; We have sides  : top
      ;; 3. Use 4 and the top to find 9.
      ;;    9 only has one extra, which is the bottom.
      ;;    The missing one from 9 is bottom-left.
      (loop for n from 0 below 10
            for nine = (aref entry n)
            until (< 0 (get-signal 9))
            when (= 6 (d8-count-active nine))
            do (let ((four-top (logior (get-signal 4) (get-side :top))))
                 (when (= 1 (d8-count-active (logxor four-top nine)))
                   (set-signal 9 nine)
                   (set-side :bottom (logxor four-top nine))
                   (set-side :bottom-left (lognot nine)))))
      ;; We have signals: 1 4 7 8 9
      ;; We have sides  : top bottom-left bottom
      ;; 4. 7 + bottom + some other that is mid = 3.
      ;;    With middle and eight we can get zero.
      (loop for n from 0 below 10
            for three = (aref entry n)
            until (< 0 (get-signal 3))
            when (= 5 (d8-count-active three))
            do (let ((seven-bottom (logior (get-signal 7) (get-side :bottom))))
                 (when (= 1 (d8-count-active (logxor seven-bottom three)))
                   (set-signal 3 three)
                   (set-side :middle (logxor seven-bottom three))
                   (set-signal 0 (logxor (get-signal 8) (get-side :middle))))))
      ;; We have signals: 0 1 3 4 7 8 9
      ;; We have sides  : top middle bottom-left bottom
      ;; 5. Top + mid + bottom + bottom-left + some other = 2, some other is top-right.
      ;;    Top right and one reveal bottom right.
      (loop for n from 0 below 10
            for two = (aref entry n)
            until (< 0 (get-signal 2))
            when (= 5 (d8-count-active two))
            do (let ((t-m-b-bl (get-sides :top :middle :bottom :bottom-left)))
                 (when (= 1 (d8-count-active (logxor t-m-b-bl two)))
                   (set-signal 2 two)
                   (set-side :top-right (logxor t-m-b-bl two))
                   (set-side :bottom-right (logxor (get-side :top-right) (get-signal 1))))))
      ;; We have signals: 0 1 2 3 4 7 8 9
      ;; We have sides  : top top-right middle bottom-left bottom-right bottom
      ;; 6. Rest we can deduce. 4 - (1 + mid) = top-left
      ;;    And then we can just build number five.
      (let ((one-mid (logior (get-signal 1) (get-side :middle))))
        (set-side :top-left (logxor (get-signal 4) one-mid)))
      (set-signal 5 (get-sides :top :top-left :middle :bottom-right :bottom))
      (set-signal 6 (logxor (get-signal 8) (get-side :top-right)))
      ;; We have signals: 0 1 2 3 4 5 6 7 8 9 ;; Done.
      ;; We have sides  : top top-left top-right middle bottom-left bottom-right bottom ;; Done.
      (let ((number 0))
        (dotimes (i 4)
          (loop with digit = (aref entry (+ i 10))
                for n from 0
                while (/= digit (get-signal n))
                finally (incf number (* (/ 1000 (expt 10 i)) n))))
        number))))

(defun d8p2 ()
  (loop for entry in (d8-data)
        sum (d8-solve entry)))

;; Answer: 908067
