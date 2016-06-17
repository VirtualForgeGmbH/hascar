# What is hascar?

HASCAR is a free unzip utility for SAP's SAPCAR format.

It is implemented 100% in haskell, including the lzh decompression
routine.

This is not yet in a stable state. It should successfully unpack lzh
compressed files and uncompressed files.

The SAPCAR container format decoder is based on research done by
Martin Gallo (https://github.com/CoreSecurity/pysap) with further
investigation by Hans-Christian Esperer <hc@hcesperer.org>, who also
did the LZH decompressor reimplementation.

What is supported:

* Reading SAPCAR archives version 2.01 only
* Unpacking files that are LZH compressed
* Unpacking files that are not compressed

TODO:

* Implement LZC
* Implement packing functionality
* Maybe implement signature checking

# Installing hascar

To compile and install, first get stack (
http://docs.haskellstack.org/en/stable/README/), then issue:

stack build && stack install

hascar will be installed to ~/.local/bin

# Usage

Run hascar with the -h flag to get help. Basically, the usage should be the
same as with SAP(R)'s sapcar tool.

# Example run:

hc@espererh-pc ~/I/hascar λ hascar -xtvf /home/hc/test.sar 
┌────────────────────────────────────────────────────────────────────┐
│          hascar, Copyright (C) 2016, Virtual Forge GmbH.           │
│                                                                    │
│                   Maint.: Hans-Christian Esperer                   │
│             <hans-christian.esperer@virtualforge.com>              │
│                                                                    │
│             hascar comes with ABSOLUTELY NO WARRANTY;              │
│                 for details read the LICENSE file.                 │
│     This is free software, and you are welcome to redistribute     │
│   it under certain conditions; see the LICENSE file for details.   │
└────────────────────────────────────────────────────────────────────┘

5 entrie(s) in the archive.

All entries:
-rw-r--r-- 0 root root 9302     Jun 10 00:00 sapcar-usage
-rw-r--r-- 0 root root 267468   Jun 10 00:00 pg244.txt
-rw-r--r-- 0 root root 34857    Jun 10 00:00 man.txt
-rw-r--r-- 0 root root 10485764 Jun 10 00:00 foo
-rw-r--r-- 0 root root 30       Jun 10 00:00 date

Extracting "sapcar-usage"
Extracting "pg244.txt"
Extracting "man.txt"
Extracting "foo"
Extracting "date"

hc@espererh-pc ~/I/hascar λ
