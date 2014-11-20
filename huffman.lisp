;;;; Huffman Coding for Common Lisp
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
   #:huffman-decode

   ;; creating a huffman coding map from a sequence
   #:huffman-map-sequence

   ;; huffman coding slot readers
   #:huffman-coding-sequence
   #:huffman-coding-map))

(in-package :huffman)

(defclass huffman-coding ()
  ((coding :initarg :coding :reader huffman-coding-sequence)
   (map    :initarg :map    :reader huffman-coding-map)
   (type   :initarg :type   :reader huffman-coding-type))
  (:documentation "Return value from huffman-encode."))

(defmethod print-object ((obj huffman-coding) stream)
  "Output a Huffman coding."
  (print-unreadable-object (obj stream :type t)
    (with-slots (coding map)
        obj
      (format stream "~d bits, ~d unique values" (length coding) (hash-table-count map)))))

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

          ;; push a new root back onto the list, but in sorted order
          do (loop with node = (make-root left right)

                   ;; keep popping smaller node values
                   while (and ns (> (node-count node) (node-count (first ns))))
                   collect (pop ns)
                   into less
                   
                   ;; construct the new tree nodes
                   finally (setf ns (append less (cons node ns))))

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
        (build-map tree (make-array 0 :element-type 'bit :adjustable t :fill-pointer t))))))

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

(defun huffman-map-sequence (seq)
  "Return a hash-table of Huffman codes from a sequence."
  (map-of-tree (make-tree seq)))

(defun huffman-encode (seq &optional (map (huffman-map-sequence seq)))
  "Generate a Huffman map of values to bit encodings and encode the sequence."
  (let ((coding (make-array 0 :element-type 'bit :adjustable t :fill-pointer t)))
    (flet ((encode (x)
             (let ((bits (gethash x map)))
               (if bits
                   (loop for b across bits do (vector-push-extend b coding))
                 (warn "~s not found in coding map; skipping..." x)))))
      (map nil #'encode seq))
    (make-instance 'huffman-coding :type (type-of seq) :coding coding :map map)))

(defun huffman-decode (coding)
  "Decode a bit vector with a Huffman encoding map."
  (let ((value (loop with tree = (tree-of-map (huffman-coding-map coding))

                     ;; always start a the root
                     with node = tree
                     
                     ;; loop over every bit
                     for b across (huffman-coding-sequence coding)
                     
                     ;; traverse the tree
                     do (setf node (if (zerop b)
                                       (node-left node)
                                     (node-right node)))
                     
                     ;; if reached a leaf node, then collect the value and reset back to the root
                     when (node-leaf node)
                     collect (prog1
                                 (node-value node)
                               (setf node tree)))))

    ;; return the type desired
    (coerce value (huffman-coding-type coding))))

