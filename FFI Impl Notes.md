# C FFI Implementation Notes
These notes contain ruminations on the implementation of a C-level FFI for the
integration of Luna with other C-FFI capable languages. 

## List of Proposed Language Changes
To properly implement the C FFI

- C Value Library:
  + `Std.FFI.C.Value`
  + This contains utilities for marshalling between the C types and Luna inbuilt
    types.
  + It contains type definitions for ALL of C's types (compliant with the GCC or
    clang view of the world as C's types aren't strictly defined)
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
  + Also wraps things like `sizeof` to ensure that memory can be allocated in a
    correct fashion. 
  + Utilities for casting values from memory back into Luna values.
  + Utilities for manual pinning of values in memory. This is important for 
    enforcing correct semantics depending on the responsibility for allocation
    and deallocation of values. 
  + Padding attributes for specifying alignment manually.
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
    * `object`: The object in which the foreign function can be found. Maybe?
    * `name`: The Luna-side name for the function. 
    * `sig`: The type signature of the foreign function being imported. This is
      specified in Luna's C Types. Must include the return type.
  + It produces a binding in the scope of the `foreign import` that can be 
    called as any normal Luna function. 
  + For example:
    
    ```
    foreign import C "allocTensor" "Tensor.o" :: CInt64 -> CPtr
    ```

## Implementation Notes

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

## Resources

- [https://wiki.haskell.org/Foreign_Function_Interface](Haskell FFI)
- [https://hackage.haskell.org/package/plugins-1.5.7/docs/System-Plugins-Load.html](GHC Runtime Linker API)
- [CFFI Python Library](https://cffi.readthedocs.io/en/release-0.8/)
- [x86 Calling Convention](https://en.wikibooks.org/wiki/X86_Disassembly/Calling_Conventions#Standard_C_Calling_Conventions)
