# Kaleidoscope

My implementation of the kaleidoscope tutorial by Stephen Diehl.

Switched to LLVM 9.0
https://www.reddit.com/r/haskell/comments/8c6zlf/how_can_i_link_c_source_using_stack/


# Differences from the original tutorial:
- ShortByteString v. String
 - This was the biggest issue; the current LLVM bindings use ShortByteStrings for AST Names, so I had to manually pack the parse tree into a ByteString form. A more robust solution would be to parse into ByteStrings directly.

- Codegen
 - Due to the differences in function v. value types, calling functions with `externf` requires determining the function ptr type, which isn't `double` as in the tutorial.
