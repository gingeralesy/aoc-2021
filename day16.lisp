#|
This file is a part of aoc-2021
(c) 2021 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:aoc-2021)

(defparameter *day16-input* (local-file #P"day16.txt" :error T))

(defun d16-data ()
  (with-open-file (stream *day16-input* :if-does-not-exist :error)
    (loop with line = (read-line stream NIL)
          for ch across line
          counting ch into count
          collect (parse-integer (format NIL "~c" ch) :radix 16) into nibbles
          finally (return
                    (values (make-array count :element-type '(unsigned-byte 4)
                                              :initial-contents nibbles)
                            count)))))

#|
--- Day 16: Packet Decoder ---

As you leave the cave and reach open waters, you receive a transmission from the Elves back on the
ship.

The transmission was sent using the Buoyancy Interchange Transmission System (BITS), a method of
packing numeric expressions into a binary sequence. Your submarine's computer has saved the
transmission in hexadecimal (your puzzle input).

The first step of decoding the message is to convert the hexadecimal representation into binary.
Each character of hexadecimal corresponds to four bits of binary data:

0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111

The BITS transmission contains a single packet at its outermost layer which itself contains many
other packets. The hexadecimal representation of this packet might encode a few extra 0 bits at the
end; these are not part of the transmission and should be ignored.

Every packet begins with a standard header: the first three bits encode the packet version, and the
next three bits encode the packet type ID. These two values are numbers; all numbers encoded in any
packet are represented as binary with the most significant bit first. For example, a version encoded
as the binary sequence 100 represents the number 4.

Packets with type ID 4 represent a literal value. Literal value packets encode a single binary
number. To do this, the binary number is padded with leading zeroes until its length is a multiple
of four bits, and then it is broken into groups of four bits. Each group is prefixed by a 1 bit
except the last group, which is prefixed by a 0 bit. These groups of five bits immediately follow
the packet header. For example, the hexadecimal string D2FE28 becomes:

| 1101 0010 1111 1110 0010 1000
| VVVT TTAA AAAB BBBB CCCC C

Below each bit is a label indicating its purpose:

- The three bits labeled V (110) are the packet version, 6.
- The three bits labeled T (100) are the packet type ID, 4, which means the packet is a literal
  value.
- The five bits labeled A (10111) start with a 1 (not the last group, keep reading) and contain the
  first four bits of the number, 0111.
- The five bits labeled B (11110) start with a 1 (not the last group, keep reading) and contain four
  more bits of the number, 1110.
- The five bits labeled C (00101) start with a 0 (last group, end of packet) and contain the last
  four bits of the number, 0101.
- The three unlabeled 0 bits at the end are extra due to the hexadecimal representation and should
  be ignored.

So, this packet represents a literal value with binary representation 011111100101, which is 2021 in
decimal.

Every other type of packet (any packet with a type ID other than 4) represent an operator that
performs some calculation on one or more sub-packets contained within. Right now, the specific
operations aren't important; focus on parsing the hierarchy of sub-packets.

An operator packet contains one or more packets. To indicate which subsequent binary data represents
its sub-packets, an operator packet can use one of two modes indicated by the bit immediately after
the packet header; this is called the length type ID:

- If the length type ID is 0, then the next 15 bits are a number that represents the total length in
  bits of the sub-packets contained by this packet.
- If the length type ID is 1, then the next 11 bits are a number that represents the number of
  sub-packets immediately contained by this packet.

Finally, after the length type ID bit and the 15-bit or 11-bit field, the sub-packets appear.

For example, here is an operator packet (hexadecimal string 38006F45291200) with length type ID 0
that contains two sub-packets:

| 0011 1000 0000 0000 0110 1111 0100 0101 0010 1001 0001 0010 0000 0000
| VVVT TTIL LLLL LLLL LLLL LLAA AAAA AAAA ABBB BBBB BBBB BBBB B

- The three bits labeled V (001) are the packet version, 1.
- The three bits labeled T (110) are the packet type ID, 6, which means the packet is an operator.
- The bit labeled I (0) is the length type ID, which indicates that the length is a 15-bit number
  representing the number of bits in the sub-packets.
- The 15 bits labeled L (000000000011011) contain the length of the sub-packets in bits, 27.
- The 11 bits labeled A contain the first sub-packet, a literal value representing the number 10.
- The 16 bits labeled B contain the second sub-packet, a literal value representing the number 20.

After reading 11 and 16 bits of sub-packet data, the total length indicated in L (27) is reached,
and so parsing of this packet stops.

As another example, here is an operator packet (hexadecimal string EE00D40C823060) with length type
ID 1 that contains three sub-packets:

| 1110 1110 0000 0000 1101 0100 0000 1100 1000 0010 0011 0000 0110 0000
| VVVT TTIL LLLL LLLL LLAA AAAA AAAA ABBB BBBB BBBB CCCC CCCC CCC

- The three bits labeled V (111) are the packet version, 7.
- The three bits labeled T (011) are the packet type ID, 3, which means the packet is an operator.
- The bit labeled I (1) is the length type ID, which indicates that the length is a 11-bit number
  representing the number of sub-packets.
- The 11 bits labeled L (00000000011) contain the number of sub-packets, 3.
- The 11 bits labeled A contain the first sub-packet, a literal value representing the number 1.
- The 11 bits labeled B contain the second sub-packet, a literal value representing the number 2.
- The 11 bits labeled C contain the third sub-packet, a literal value representing the number 3.

After reading 3 complete sub-packets, the number of sub-packets indicated in L (3) is reached, and
so parsing of this packet stops.

For now, parse the hierarchy of the packets throughout the transmission and add up all of the
version numbers.

Here are a few more examples of hexadecimal-encoded transmissions:

- 8A004A801A8002F478 represents an operator packet (version 4) which contains an operator packet
  (version 1) which contains an operator packet (version 5) which contains a literal value
  (version 6); this packet has a version sum of 16.
- 620080001611562C8802118E34 represents an operator packet (version 3) which contains two
  sub-packets; each sub-packet is an operator packet that contains two literal values. This packet
  has a version sum of 12.
- C0015000016115A2E0802F182340 has the same structure as the previous example, but the outermost
  packet uses a different length type ID. This packet has a version sum of 23.
- A0016C880162017C3686B18A3D4780 is an operator packet that contains an operator packet that
  contains an operator packet that contains five literal values; it has a version sum of 31.

Decode the structure of your hexadecimal-encoded BITS transmission; what do you get if you add up
the version numbers in all packets?
|#

(defstruct d16-packet
  (version 0 :type (unsigned-byte 3))
  (type 0 :type (unsigned-byte 3))
  (literal 0 :type (unsigned-byte 64))
  (sub-packets NIL :type (or null list)))

(defmethod d16-packet-value ((packet d16-packet))
  (ecase (d16-packet-type packet)
    (0 ;; Sum.
     (loop for sub in (d16-packet-sub-packets packet)
           summing (d16-packet-value sub)))
    (1 ;; Product.
     (loop with product = 1
           for sub in (d16-packet-sub-packets packet)
           do (setf product (* product (d16-packet-value sub)))
           finally (return product)))
    (2 ;; Minimum.
     (loop for sub in (d16-packet-sub-packets packet)
           minimizing (d16-packet-value sub)))
    (3 ;; Maximum.
     (loop for sub in (d16-packet-sub-packets packet)
           maximizing (d16-packet-value sub)))
    (4 ;; Literal.
     (d16-packet-literal packet))
    (5 ;; Greater than.
     (let ((subs (d16-packet-sub-packets packet)))
       (when (third subs) (error "Too many sub-packets."))
       (if (< (d16-packet-value (second subs)) (d16-packet-value (first subs))) 1 0)))
    (6 ;; Less than.
     (let ((subs (d16-packet-sub-packets packet)))
       (when (third subs) (error "Too many sub-packets."))
       (if (< (d16-packet-value (first subs)) (d16-packet-value (second subs))) 1 0)))
    (7 ;; Equal to.
     (let ((subs (d16-packet-sub-packets packet)))
       (when (third subs) (error "Too many sub-packets."))
       (if (= (d16-packet-value (first subs)) (d16-packet-value (second subs))) 1 0)))))

(defun d16-get-packet (nibbles &optional (offset 0) max-bits)
  (labels ((bits (count offset)
             "Finds the wanted bits as an integer.
count - number of bits to read
offset - number of bits skipped from the start of the array
max-bits - how many bits this packet can at most be in the input (an error check)
return - integer value with the wanted bits"
             (loop with value = 0
                   with read = 0
                   with index = (floor offset 4)
                   with offset = (mod offset 4)
                   for n from index
                   for first = T then NIL
                   for to-read = (min (- count read) (if first (min count (- 4 offset)) 4))
                   for mask = #xF
                   while (< 0 to-read)
                   for nibble = (aref nibbles n)
                   do (setf value (ash value to-read))
                   do (when first
                        (setf mask (ash mask (- offset)))
                        (when (< (+ offset count) 4)
                          (setf mask (logand mask (ash #xF (- 4 (+ offset count)))))))
                   do (unless first (setf mask (ash mask (- 4 to-read))))
                   do (setf mask (logand mask #xF))
                   do (setf nibble (logand nibble mask))
                   do (when (and first (< (+ offset count) 4))
                        (setf nibble (ash nibble (- (+ offset count) 4))))
                   do (when (and (not first) (< to-read 4))
                        (setf nibble (ash nibble (- to-read 4))))
                   do (setf value (logior value nibble))
                   do (incf read to-read)
                   finally (return value)))
           (version (offset)
             "Gets the packet version number (three bits)."
             (bits 3 offset))
           (type (offset)
             "Gets the three-bit packet type number."
             (bits 3 (+ offset 3)))
           (length-type (offset)
             "Gets the one bit flag for an operator's length type.
return - 0 for number of bits, 1 for number of sub-packets"
             (bits 1 (+ offset 6)))
           (sub-length (offset)
             "Gets the bit length of the operator's sub-sections."
             (let ((type (length-type  offset)))
               (values
                (if (/= type 0)
                    (bits 11 (+ offset 7))
                    (bits 15 (+ offset 7)))
                type)))
           (literal (offset)
             "Gets the value of a literal section."
             (loop with value = 0
                   for n from 0
                   for bits = (bits 5 (+ offset 6 (* n 5)))
                   do (setf value (logior (ash value 4) (logand bits #xF)))
                   while (< 0 (logand bits #x10))
                   finally (return (values value (* 5 (1+ n)))))))
    (let ((packet (make-d16-packet :version (version offset)
                                   :type (type offset)))
          (size 6))
      (case (d16-packet-type packet)
        (4 ;; Literal.
         (multiple-value-bind (literal bits-read)
             (literal offset)
           (setf (d16-packet-literal packet) literal)
           (incf size bits-read)))
        (T ;; Operation
         (multiple-value-bind (length length-type)
             (sub-length offset)
           (incf size)
           (ecase length-type
             (0 ;; Subs of size.
              (incf size 15)
              (when (and max-bits (< max-bits (+ length size)))
                (error "Length too long: ~a < ~a" size length))
              (loop with remaining = length
                    while (< 0 remaining)
                    collect (multiple-value-bind (sub-packet sub-size)
                                (d16-get-packet nibbles (+ offset size) remaining)
                              (decf remaining sub-size)
                              (incf size sub-size)
                              sub-packet)
                    into sub-packets
                    finally (setf (d16-packet-sub-packets packet) sub-packets)))
             (1 ;; Subs of count.
              (incf size 11)
              (dotimes (i length))
              (loop for i from 1 to length
                    collect (multiple-value-bind (sub-packet sub-size)
                                (d16-get-packet nibbles (+ offset size)
                                                (and max-bits (- max-bits size)))
                              (incf size sub-size)
                              sub-packet)
                    into sub-packets
                    finally (setf (d16-packet-sub-packets packet) sub-packets)))))))
      (when (and max-bits (< max-bits size)) (error "Too big: ~a < ~a" max-bits size))
      (values packet size))))

;; 0110 0000 0101 0101 0010 1111 0001 0000 0000 0110 1001 0011 0010 1001 1000 1010
;; VVVT TTIL LLLL LLLL LLLL LLVV VTTT ILLL LLLL LLLL LLLL VVVT TTAA AAAB BBBB CCCC ...

(defun d16p1 ()
  (let ((version-count 0))
    (labels ((inc-count (packet)
               (incf version-count (d16-packet-version packet))
               (loop for sub in (d16-packet-sub-packets packet)
                     do (inc-count sub))))
      (inc-count (d16-get-packet (d16-data))))
    version-count))

#|
--- Part Two ---

Now that you have the structure of your transmission decoded, you can calculate the value of the
expression it represents.

Literal values (type ID 4) represent a single number as described above. The remaining type IDs are
more interesting:

- Packets with type ID 0 are sum packets - their value is the sum of the values of their
  sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
- Packets with type ID 1 are product packets - their value is the result of multiplying together the
  values of their sub-packets. If they only have a single sub-packet, their value is the value of
  the sub-packet.
- Packets with type ID 2 are minimum packets - their value is the minimum of the values of their
  sub-packets.
- Packets with type ID 3 are maximum packets - their value is the maximum of the values of their
  sub-packets.
- Packets with type ID 5 are greater than packets - their value is 1 if the value of the first
  sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These
  packets always have exactly two sub-packets.
- Packets with type ID 6 are less than packets - their value is 1 if the value of the first
  sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These
  packets always have exactly two sub-packets.
- Packets with type ID 7 are equal to packets - their value is 1 if the value of the first
  sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These
  packets always have exactly two sub-packets.

Using these rules, you can now work out the value of the outermost packet in your BITS transmission.

For example:

- C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
- 04005AC33890 finds the product of 6 and 9, resulting in the value 54.
- 880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
- CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
- D8005AC2A8F0 produces 1, because 5 is less than 15.
- F600BC2D8F produces 0, because 5 is not greater than 15.
- 9C005AC2F8F0 produces 0, because 5 is not equal to 15.
- 9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.

What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?
|#

(defun d16p2 ()
  (let ((root (d16-get-packet (d16-data))))
    (d16-packet-value root)))

;; Answer: 194435634456
