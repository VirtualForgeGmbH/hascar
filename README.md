HASCAR is a free unzip utility for SAP's SAPCAR format. This is
currently not in a stable state. This is partly based on research by
Martin Gallo.

What is supported:

* Reading SAPCAR archives version 2.01 only
* Unpacking small files that are LZH compressed
* Unpacking large uncompressed files

TODO:

* Implement LZC
* Files that span multiple SAPCAR blocks and are LZH compressed do not
  decompress correctly
* Implement Packing functionality
* Implement unpacking directories

To compile, get stack ( http://docs.haskellstack.org/en/stable/README/
), then issue:

stack build && stack install

hascar will be installed to ~/.local/bin
