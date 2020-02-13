initSidebarItems({"fn":[["alloc","Allocate memory with the global allocator."],["alloc_zeroed","Allocate zero-initialized memory with the global allocator."],["dealloc","Deallocate memory with the global allocator."],["handle_alloc_error","Abort on memory allocation error or failure."],["realloc","Reallocate memory with the global allocator."],["set_alloc_error_hook","Registers a custom allocation error hook, replacing any that was previously registered."],["take_alloc_error_hook","Unregisters the current allocation error hook, returning it."]],"struct":[["AllocErr","The `AllocErr` error indicates an allocation failure that may be due to resource exhaustion or to something wrong when combining the given input arguments with this allocator."],["CannotReallocInPlace","The `CannotReallocInPlace` error is used when [`grow_in_place`] or [`shrink_in_place`] were unable to reuse the given memory block for a requested layout."],["Excess","Represents the combination of a starting address and a total capacity of the returned block."],["Global","The global memory allocator."],["Layout","Layout of a block of memory."],["LayoutErr","The parameters given to `Layout::from_size_align` or some other `Layout` constructor do not satisfy its documented constraints."],["System","The default memory allocator provided by the operating system."]],"trait":[["Alloc","An implementation of `Alloc` can allocate, reallocate, and deallocate arbitrary blocks of data described via `Layout`."],["GlobalAlloc","A memory allocator that can be registered as the standard library’s default through the `#[global_allocator]` attribute."]]});