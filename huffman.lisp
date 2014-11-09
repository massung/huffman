;;;; Huffman Encoding for Common Lisp
;;;;
;;;; Copyright (c) 2014 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :huffman
  (:use :cl)
  (:nicknames :huff)
  (:export
   #:huffman-encode
   #:huffman-decode))

(in-package :huffman)

(defstruct node "Huffman tree node." count leaf value left right)

(defun make-leaf (value &optional count)
  "Create a Huffman tree leaf node."
  (make-node :count count :leaf t :value value))

(defun make-root (left right)
  "Create a Huffman tree root node."
  (make-node :count (+ (node-count left) (node-count right)) :left left :right right))

(defun make-tree (seq)
  "Generate a Huffman tree from a sequence of data."
  (let* ((xs (remove-duplicates seq))

         ;; count how many of each unique elements is in the sequence
         (cs (map 'list #'(lambda (x) (make-leaf x (count x seq :test 'equal))) xs))

         ;; sort the nodes by count
         (ns (sort cs #'< :key #'node-count)))

    ;; build a binary tree with all the huffman codes
    (loop while (second ns)
          
          ;; get the two least frequeny nodes (lower count is on left)
          for left = (pop ns)
          for right = (pop ns)

          ;; push a root and resort to get the next
          do (setf ns (sort (cons (make-root left right) ns) #'< :key #'node-count))

          ;; return the root
          finally (return (first ns)))))

(defun map-of-tree (tree)
  "Return a hash-table mapping of values to bit-vector codes."
  (let ((map (make-hash-table :test 'equal)))
    (labels ((build-map (node bits)
               (if (node-leaf node)
                   (setf (gethash (node-value node) map) bits)
                 (progn
                   (build-map (node-left node) (concatenate '(vector bit) bits #*0))
                   (build-map (node-right node) (concatenate '(vector bit) bits #*1))))))
      (prog1
          map
        (build-map tree #*)))))

(defun tree-of-map (map)
  "Return a binary tree of Huffman codes from a hash map."
  (let ((root (make-node)))
    (labels ((insert-value (x bits &optional (node root) (i 0))
               (let ((b (aref bits i)))
                 (if (= (incf i) (length bits))

                     ;; at the end of the value, create a leaf
                     (if (plusp b)
                         (setf (node-right node) (make-leaf x))
                       (setf (node-left node) (make-leaf x)))

                   ;; traverse down the tree, possibly creating child nodes
                   (let ((child (if (plusp b)
                                    (setf (node-right node) (or (node-right node) (make-node)))
                                  (setf (node-left node) (or (node-left node) (make-node))))))
                     (insert-value x bits child i))))))
      (prog1
          root
        (maphash #'insert-value map)))))

(defun huffman-encode (seq)
  "Generate a Huffman map of values to bit encodings and encode the sequence."
  (let ((map (map-of-tree (make-tree seq))))
    (flet ((encode (s x)
             (concatenate '(vector bit) s (gethash x map))))
      (values (reduce #'encode seq :initial-value #*) map))))

(defun huffman-decode (bits map &optional (type 'list))
  "Decode a bit vector with a Huffman encoding map."
  (let* ((tree (tree-of-map map))
         (head (list nil))

         ;; target node and value destination
         (node tree)
         (tail head))

    ;; traverse to the next node in the tree
    (flet ((decode (bit)
             (setf node (slot-value node (if (zerop bit) 'left 'right)))

             ;; if it's a leaf node, then collect the value
             (when (node-leaf node)
               (setf tail (cdr (rplacd tail (list (node-value node)))))

               ;; reset back to the tree root
               (setf node tree))))
      (map nil #'decode bits))

    ;; return the collected values
    (coerce (rest head) type)))
