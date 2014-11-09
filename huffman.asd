(defpackage :huffman-asd
  (:use :cl :asdf))

(in-package :huffman-asd)

(defsystem :huffman
  :name "huffman"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Huffman encoding and decoding for Common Lisp."
  :serial t
  :components ((:file "huffman")))
