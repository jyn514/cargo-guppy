(function() {var implementors = {};
implementors["console"] = [{"text":"impl&lt;D:&nbsp;Pointer&gt; Pointer for StyledObject&lt;D&gt;","synthetic":false,"types":[]}];
implementors["crossbeam_epoch"] = [{"text":"impl&lt;T:&nbsp;?Sized + Pointable&gt; Pointer for Atomic&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl&lt;T:&nbsp;?Sized + Pointable, '_&gt; Pointer for Shared&lt;'_, T&gt;","synthetic":false,"types":[]}];
implementors["env_logger"] = [{"text":"impl&lt;'a, T:&nbsp;Pointer&gt; Pointer for StyledValue&lt;'a, T&gt;","synthetic":false,"types":[]}];
implementors["itertools"] = [{"text":"impl&lt;'a, I&gt; Pointer for Format&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: Iterator,<br>&nbsp;&nbsp;&nbsp;&nbsp;I::Item: Pointer,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["supercow"] = [{"text":"impl&lt;'a, OWNED, BORROWED:&nbsp;?Sized, SHARED, STORAGE, PTR&gt; Pointer for Supercow&lt;'a, OWNED, BORROWED, SHARED, STORAGE, PTR&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;BORROWED: 'a,<br>&nbsp;&nbsp;&nbsp;&nbsp;*const BORROWED: PointerFirstRef,<br>&nbsp;&nbsp;&nbsp;&nbsp;STORAGE: OwnedStorage&lt;OWNED, SHARED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: PtrWrite&lt;BORROWED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;BORROWED: Pointer,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: PtrRead&lt;BORROWED&gt;,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["tinyvec"] = [{"text":"impl&lt;A:&nbsp;Array&gt; Pointer for ArrayVec&lt;A&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;A::Item: Pointer,&nbsp;</span>","synthetic":false,"types":[]},{"text":"impl&lt;'s, T&gt; Pointer for SliceVec&lt;'s, T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Pointer,&nbsp;</span>","synthetic":false,"types":[]},{"text":"impl&lt;A:&nbsp;Array&gt; Pointer for TinyVec&lt;A&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;A::Item: Pointer,&nbsp;</span>","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()