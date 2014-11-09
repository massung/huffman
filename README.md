# Huffman Coding for Common Lisp

A very simple implementation of [Huffman coding](http://en.wikipedia.org/wiki/Huffman_coding) for Common Lisp.

## Quickstart

Just use the `huffman-encode` and `huffman-decode` functions to encode and decode any Common Lisp sequence.

	CL-USER > (huffman-encode "This is a test.")
	#*1000100110101001010100111100011011111011101110
	#<EQUAL Hash Table{9} 200BD383>
	
	CL-USER > (huffman-encode '(0 3 4 4 6 2 1 6 2 6 0 6 7 7 7 6 0 9 5 1))
	#*111011100000001000101010001101111011011011010111011110110010
	#<EQUAL Hash Table{9} 200A618B>

The return values for `huffman-encode` is the bit vector and a hash table for the Huffman tree, which is needed for decoding.
	
	CL-USER > (multiple-value-bind (bits map)
	              (huffman-encode "Testing decoding.")
	            (huffman-decode bits map 'string))
	"Testing decoding."

The last argument to `huffman-decode` is an optional type to `coerce` to. The default is to just return a list.

That's it.

*NOTE: While `huffman-encode` returns a bit vector, the `huffman-decode` function can take any sequence of bits (a list, vector, bit vector, etc).*
