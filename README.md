# Huffman Coding for Common Lisp

A very simple implementation of [Huffman coding](http://en.wikipedia.org/wiki/Huffman_coding) for Common Lisp.

## Quickstart

Just use the `huffman-encode` and `huffman-decode` functions to encode and decode any Common Lisp sequence.

	CL-USER > (huffman-encode "This is a test.")
	#<HUFFMAN-CODING 46 bits, 9 unique values>
	
	CL-USER > (huffman-encode '(0 3 4 4 6 2 1 6 2 6 0 6 7 7 7 6 0 9 5 1))
	#<HUFFMAN-CODING 60 bits, 9 unique values>

The return value for `huffman-encode` is a `huffman-coding` object that contains the encoded bit-vector, a mapping of values to bits, and the type of the originally encoded sequence for the decoder.

The coding is then passed to `huffman-decode` to obtain the original sequence of values.
	
	CL-USER > (huffman-decode *)
	(0 3 4 4 6 2 1 6 2 6 0 6 7 7 7 6 0 9 5 1)

That's it.
