HASCAR is a free unzip utility for SAP's SAPCAR format.

It is implemented 100% in haskell, including the lzh decompression
routine.

This is not yet in a stable state. It should successfully decompress
lzh compressed files and directly stored files.

The is partly based on research by
Martin Gallo (https://github.com/CoreSecurity/pysap).

What is supported:

* Reading SAPCAR archives version 2.01 only
* Unpacking files that are LZH compressed
* Unpacking files that are not compressed

TODO:

* Implement LZC
* Implement packing functionality

To compile, get stack ( http://docs.haskellstack.org/en/stable/README/
), then issue:

stack build && stack install

hascar will be installed to ~/.local/bin
