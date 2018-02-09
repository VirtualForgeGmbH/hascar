0.2.2.2
-------

- Import the semigroups package to be compatible
  with earlier versions of ghc

0.2.2.1
-------

- When unwrapping transport files from PAT files,
  name the transport files properly
- Add an option to specify the extraction directory
  (default: CWD)

0.2.2.0
-------

- Add an option to extract transport files from PAT
  files automatically

0.2.1.2
-------

- Remove version constraints, rely on stack/nixos instead
  for dependency management

0.2.1.1
-------

- Make it compile with ghc-8.0.2

0.2.1.0
-------

- Make "SapCar" a real type instead of a type synonym
- Use the type system to prevent "CarEntry"s to escape the monad

0.2.0.0
-------

- Use ST unboxed arrays instead of Data.Sequence and
  avoid usage of "temporary lists" during conversion
  to ByteStrings
- Add a parameter "size" to the decompressBlock
  function for more efficiency
- Rename the decompressBlock function to
  decompressBlocks for clarity
- Limit the maximum SAPCAR block size to make memory
  exhaustion attacks a bit less easy
- Compile the application single threadedly for more
  efficiency (yes, it does help in this case!)

0.1.1.0
-------

- Expose the decompressBlock function directly
