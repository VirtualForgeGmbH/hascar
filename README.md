[![Build Status](https://travis-ci.org/VirtualForgeGmbH/hascar.svg?branch=master)](https://travis-ci.org/VirtualForgeGmbH/hascar)

# What is hascar?

HASCAR is a free unzip utility for SAP's SAPCAR format. You can use it to
decompress sap car files on the command line. The command line utility also
supports unwrapping of transport files that are contained inside patch files.
(Option -p, --depat) You can also use it as a library in your own haskell programs. So
far, only the latest version of sapcar files is supported, which is 2.01.

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
* Unpacking transport files are wrapped inside a PAT file

TODO:

* Implement LZC
* Implement packing functionality
* Maybe implement signature checking
* Make the LZH algorithm more efficient (the author just about
  started to learn haskell when he embarked on implementing
  that algorithm :-)
  => Some work on this has been done; more is required.
* Implement CRC checking


# Installing hascar

## nixos

Users of [nixos](https://nixos.org) can simply install hascar by issuing
"nix-env -iaP haskellPackages.hascar". Or if you only need it temporarily, open
a nix-shell like this: "nix-shell -p haskellPackages.hascar"

## FreeBSD/GNU_Linux/OS X/Windows

To compile and install, first [get stack](
http://docs.haskellstack.org/en/stable/README/), then issue:

stack build && stack install

hascar will be installed to ~/.local/bin  . You should set your PATH variable to point
to this directory.

# Verifying signatures and encrypted archives

Since both use a proprietary crypto API, this is currently not supported.

# Usage

Run hascar with the -h flag to get help. The basic usage should be the
same as with SAP(R)'s sapcar tool. It should be noted that the used
command line parser is a bit more strict than what you might be used
to.

# Extracting transport files from PAT files

SAR files can contain transport files that are wrapped inside PAT (patch)
files. Since hascar 0.2.2.0 the option -p is offered, that will try to
automatically detect PAT files. If such a file is detected, the contained
transport file is extracted.
