# C FFI Implementation Notes
These notes contain ruminations on the implementation of a C-level FFI for the
integration of Luna with other C-FFI capable languages. 

## List of Proposed Language Changes
To properly implement the C FFI

- C Value Library:
  + `Std.FFI.C.Value`
  + This contains utilities for marshalling between the C types and Luna inbuilt
    types.
  + It contains type definitions for ALL of C's types (compliant with the GCC +
    clang view of the world as C's types aren't strictly defined).
  + These type definitions are automatically unboxed when passed to a foreign 
    call.
  + These types are pinned in memory as long as they are in use by a foreign
    call.
- Pointer Library:
  + `Std.FFI.C.Pointer`
  + Library for performing pointer arithmetic and general operations. 
  + Needs to be the right type based on ABI (32/64-bit).
- Memory Library:
  + `Std.FFI.C.Memory`
  + Wraps the C-level memory allocation and deallocation functions in Luna so
    you can use memory properly.
  + This should provide a `sizeOf` function that is a wrapper for `T.byteSize`
    for all types in `C.Value`.
  + Utilities for casting values from memory back into Luna values.
  + Utilities for manual pinning of values in memory. This is important for 
    enforcing correct semantics depending on the responsibility for allocation
    and deallocation of values. This should move between GHC's pinned and 
    unpinned heaps, but ideally it is allocated in the right place from the
    start.
  + Padding attributes for specifying alignment of struct attributes. This must
    allow users to query alignment, but also force certain alignments. 
- Automatic conversion of Luna types to raw structs:
  + As struct layout is common (though technically implementation defined), we 
    can automatically marshal a Luna class into a value struct.
  + As long as the layout matches what the foreign function expects, this will
    just work. Same memory pinning rules would apply.
  + This errors at compile time if the class contains values that aren't members
    of the C value library.
  + We need some way to manually specify struct alignment. 
  + Method for getting `this` pointer of Luna value. 
- Function Wrapper Library:
  + `Std.FFI.C.Function`
  + Functionality for wrapping Luna functions as function pointers that are 
    able to be called from C
  + This ensures that callbacks work.
- Wrapping of C-level functions to be called from within Luna.
  + `Std.FFI.C.Call`
  + Brings the `foreign import [unsafe] C` syntax into scope.
  + This takes arguments:
    * `symbol`: The name of the foreign function
    * `object`: The object in which the foreign function can be found. 
    * `name`: The Luna-side name for the function. 
    * `sig`: The type signature of the foreign function being imported. This is
      specified in Luna's C Types. Must include the return type.
  + It produces a binding in the scope containing the `foreign import` that can 
    be called as any normal Luna function. 
  + Potentially we could get away with not specifying the object file, but that
    runs the risk of breaking existing code in case of a symbol clash. 
  + For example, using Luna's block syntax:
    
    ```
    foreign import C:
      "Tensor.so":
        "allocTensor" allocTensor :: CInt64 -> CPtr
    ```

## Implementation Notes

- Lazy loading of symbols as needed. 
- Loaded symbols will require caching for performance.
- Utilising the GHC Runtime Linker to load objects.
- Utilising the GHC Runtime Linker for symbol discovery.
- Compiler internal changes to allow the marshalling to work as intended. 
- Implementation of marshalling functions by interface for all native types.
- Compiler changes to allow for pinning of values in memory.
- What is the best way to support variadic arguments.
- Need to check the standard for C calling convention. 
- Only going to support FFI on x86/64 for now (but we get this for free using
  Haskell _for now_)
- A lot of the actual calling can be implemented by Haskell FFI functionality 
  while Luna is implemented in Haskell. 
- What functionality do we want to use to hook the linker? 
- How are we going to reify the `FunPtr` into something callable?

## Resources

- [Haskell FFI](https://wiki.haskell.org/Foreign_Function_Interface)
- [GHC Runtime Linker API](https://hackage.haskell.org/package/plugins-1.5.7/docs/System-Plugins-Load.html)
- [CFFI Python Library](https://cffi.readthedocs.io/en/release-0.8/)
- [x86 Calling Convention](https://en.wikibooks.org/wiki/X86_Disassembly/Calling_Conventions#Standard_C_Calling_Conventions)
- [Win32 API](https://hackage.haskell.org/package/Win32)
- [Unix (POSIX)](https://hackage.haskell.org/package/unix-2.7.2.2)
- [Base](https://hackage.haskell.org/package/base-4.10.1.0)
- [GHC API](https://hackage.haskell.org/package/ghc)
- [GHCi API](https://hackage.haskell.org/package/ghci-8.0.2)
- [Hotswapping Haskell](https://simonmar.github.io/posts/2017-10-17-hotswapping-haskell.html)
- [System.Plugins.Load Source](https://hackage.haskell.org/package/plugins-1.5.7/docs/src/System-Plugins-Load.html)
