[![Build Status](https://travis-ci.org/VirtualForgeGmbH/hascar.svg?branch=master)](https://travis-ci.org/VirtualForgeGmbH/hascar)

# What is hascar?

HASCAR is a free unzip utility for SAP's SAPCAR format. You can use it to
decompress sap car files on the command line. You can also use it as a library
in your own haskell programs. So far, only the latest version of sapcar files
are is supported, which is 2.01.

It is implemented 100% in haskell, including the lzh decompression
routine.

This is not yet in a stable state. It should successfully unpack lzh
compressed files and uncompressed files.

The SAPCAR container format decoder is based on [research done by
Martin Gallo](https://github.com/CoreSecurity/pysap) with further
investigation by Hans-Christian Esperer <hc@hcesperer.org>, who also
did the LZH decompressor reimplementation.

What is supported:

* Reading SAPCAR archives version 2.01 only
* Unpacking files that are LZH compressed
* Unpacking files that are not compressed
* Unpacking transport files wrapped inside PAT files

TODO:

* Implement LZC
* Implement packing functionality
* Maybe implement signature checking
* Make the LZH algorithm more efficient (the author just about
  started to learn haskell when he embarked on implementing
  that algorithm :-)
  => Some work on this has been done; more is required.
* Implement CRC checking

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

# nixos

Users of [nixos](https://nixos.org) can simply install hascar by issuing
"nix-env -iaP haskellPackages.hascar". Or if you only need it temporarily, open
a nix-shell like this: "nix-shell -p haskellPackages.hascar"

# FreeBSD/GNU_Linux/OS X/Windows

To compile and install, first [get stack](
http://docs.haskellstack.org/en/stable/README/), then issue:

stack build && stack install

hascar will be installed to ~/.local/bin  . You should set your PATH variable to point
to this directory.

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
header and compressed contents are accepted. Then, use SAP's own
sapcar tool to create a new archive from the decompressed archive. You
will now have a trusted archive, because you created it yourself. You
can then use SAP's original tool to decompress it again, while
verifying the signature.

The only attack vector left is the signature checking algorithm.

# Usage

Run hascar with the -h flag to get help. The basic usage should be the
same as with SAP(R)'s sapcar tool. It should be noted that the used
command line parser is a bit more strict than what you might be used
to.

# Extracting transport files from PAT files

SAR files can contain transport files that are wrapped inside PAT (patch)
files. Since hascar 0.2.2.0 the option -p is offered, that will try to
automatically detect PAT files. If such a file is detected, the contained
transport file is extracted. In verbose mode, this is denoted by a 'P' next to
the filename instead of an 'x'.
