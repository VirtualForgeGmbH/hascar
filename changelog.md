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
