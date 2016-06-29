[![Build Status](https://travis-ci.org/VirtualForgeGmbH/hascar.svg?branch=master)](https://travis-ci.org/VirtualForgeGmbH/hascar)

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
* Make the LZH algorithm more efficient (the author just about
  started to learn haskell when he embarked on implementing
  that algorithm :-)
  => Some work on this has been done; more is required.

# Performance

The lzh algorithm is implemented in pure haskell. I have spent some time
optimizing it, but more can certainly be done. Currently the performance is
probably acceptable for most cases, but it is still a factor ten compared to
the reference C implementation:

Decompressing a 136 MB payload (34MB compressed), the performance looks like
this:

    hascar: 7.94user 0.10system 0:08.05elapsed 99%CPU (0avgtext+0avgdata 102676maxresident)k

    sapcar: 0.84user 0.07system 0:00.92elapsed 99%CPU (0avgtext+0avgdata 8244maxresident)k

Both executed on a single CPU core. (Intel(R) Core(TM) i7-4770 CPU @ 3.40GHz)

# Installing hascar

To compile and install, first get stack (
http://docs.haskellstack.org/en/stable/README/), then issue:

stack build && stack install

hascar will be installed to ~/.local/bin

# Verifying signatures

You can use hascar to decompress and subsequently inspect the contents
of a SAR file. You would normally not install the contents from
untrusted SAR files, so there is no need to verify the signature.

If, OTOH, you do trust the original SAR file but wish to verify the
signature to ensure you are indeed dealing with the original SAR
file's contents, like when installing patches to your SAP system, then
you will need to do the following: Since hascar at this time does not
support verifying signatures, you need to use SAP's own tool for that
purpose. You can use hascar to initially decompress the archive. This
step ensures that only archives with a correct and untampered file
header and compressed contents are accpted. Then, use SAP's own sapcar
tool to create a new archive from the decompressed archive. You will
now have a trusted archive, because you created it yourself. You can
then use SAP's original tool to decompress it again, while verifying
the signature.

The only attack vector left is the signature checking algorithm.

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
