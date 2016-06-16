HASCAR is a free unzip utility for SAP's SAPCAR format.

It is implemented 100% in haskell, including the lzh decompression
routine.

This is not yet in a stable state. It will fail to LZH decompress
files that span multiple blocks.

The is partly based on research by
Martin Gallo (https://github.com/CoreSecurity/pysap).

What is supported:

* Reading SAPCAR archives version 2.01 only
* Unpacking small files that are LZH compressed
* Unpacking large uncompressed files

TODO:

* Implement LZC
* Files that span multiple SAPCAR blocks and are LZH compressed do not
  decompress correctly
* Implement packing functionality
* Implement unpacking directories
* Implement

To compile, get stack ( http://docs.haskellstack.org/en/stable/README/
), then issue:

stack build && stack install

hascar will be installed to ~/.local/bin
