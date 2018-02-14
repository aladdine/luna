# FFI Implementation Notes
This file contains notes for the implementation of the C and Haskell native FFI
functionality for Luna.

## List of Proposed Language Changes

- Extend `native` keyword to denote the native interface that the class is 
  wrapping: `native Hs class Foo`, `native C class Foo`.
- Libraries are located from the `project/libs` folder, the global library 
  database.
- A builtin operator called `foreignCall` that has the following type:

	```
	foreignCall :: ForeignType -> ObjectName -> SymbolName -> a
	```
  where:
  + `ForeignType` is the language being called to.
  + `ObjectName` is the name of the object where the foreign symbol can be 
    located.
  + `SymbolName` is the name of the symbol to search for in the object.
  + `TypeSignature` is an annotated argument that specifies the (Luna) types of
    the foreign call, and the associated arguments, for example:

    ```
    foreignCall Hs "Foo" "Bar" a b :: Int
    ```

    The associated arguments are discovered from bindings in the enclosing 
    scope.

- All class types can be provided with marshalling methods. These are `toCType`,
  `fromCType`, `toHSType` and `fromHSType` that define how to marshal the class
  to and from the corresponding native data type. For `native` classes that 
  directly wrap foreign data, these are identity functions. These are implicitly
  called by `foreignCall`. They work as follows:
  + Define `data ForeignType = HSType | CType`
  + Define `data Resolver = Resolver ObjectName SymbolName`
  + `toHSType :: Type -> Resolver -> HSType`
  + `toCType :: Type -> Resolver -> CType`
  + `fromHSType :: Resolver -> HSType -> Type`
  + `fromCType :: Resolver -> CType -> Type`
  + The overhead of what seems like dynamic dispatch here can actually be 
    collapsed to static dispatch at compile time, as long as the implementation
    pattern-matches on `Resolver`.
- Built-in classes will have default implementations of the `to` and `from` 
  methods to assist in composition for user types. 
- A library to assist in the low-level details of marshalling will be provided
  (e.g. pointer arithmetic, pointer pinning, etc). 
- A library to assist in destructuring Haskell datatypes to create a 
  correspondence between a Luna type and a Haskell type.

## Implementation Notes

- Loaded symbols will require caching for performance.
- Utilising the GHC Runtime Linker to load objects.
- Utilising the GHC Runtime Linker for symbol discovery.
- Compiler internal changes to ensure typing of `foreignCall`
- Compiler internal changes to allow the marshalling to work as intended. 

## Resources

- [https://wiki.haskell.org/Foreign_Function_Interface](Haskell FFI)
- [https://hackage.haskell.org/package/plugins-1.5.7/docs/System-Plugins-Load.html](GHC Runtime Linker API)
