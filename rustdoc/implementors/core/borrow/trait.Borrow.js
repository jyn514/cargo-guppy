(function() {var implementors = {};
implementors["bstr"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;<a class=\"struct\" href=\"bstr/struct.BStr.html\" title=\"struct bstr::BStr\">BStr</a>&gt; for <a class=\"struct\" href=\"bstr/struct.BString.html\" title=\"struct bstr::BString\">BString</a>","synthetic":false,"types":["bstr::bstring::BString"]}];
implementors["bytes"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.slice.html\">[</a><a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.u8.html\">u8</a><a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.slice.html\">]</a>&gt; for <a class=\"struct\" href=\"bytes/struct.Bytes.html\" title=\"struct bytes::Bytes\">Bytes</a>","synthetic":false,"types":["bytes::bytes::Bytes"]},{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.slice.html\">[</a><a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.u8.html\">u8</a><a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.slice.html\">]</a>&gt; for <a class=\"struct\" href=\"bytes/struct.BytesMut.html\" title=\"struct bytes::BytesMut\">BytesMut</a>","synthetic":false,"types":["bytes::bytes_mut::BytesMut"]}];
implementors["camino"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;<a class=\"struct\" href=\"camino/struct.Utf8Path.html\" title=\"struct camino::Utf8Path\">Utf8Path</a>&gt; for <a class=\"struct\" href=\"camino/struct.Utf8PathBuf.html\" title=\"struct camino::Utf8PathBuf\">Utf8PathBuf</a>","synthetic":false,"types":["camino::Utf8PathBuf"]}];
implementors["crossbeam_epoch"] = [{"text":"impl&lt;T:&nbsp;?<a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/marker/trait.Sized.html\" title=\"trait core::marker::Sized\">Sized</a> + <a class=\"trait\" href=\"crossbeam_epoch/trait.Pointable.html\" title=\"trait crossbeam_epoch::Pointable\">Pointable</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;T&gt; for <a class=\"struct\" href=\"crossbeam_epoch/struct.Owned.html\" title=\"struct crossbeam_epoch::Owned\">Owned</a>&lt;T&gt;","synthetic":false,"types":["crossbeam_epoch::atomic::Owned"]}];
implementors["smallvec"] = [{"text":"impl&lt;A:&nbsp;<a class=\"trait\" href=\"smallvec/trait.Array.html\" title=\"trait smallvec::Array\">Array</a>&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;[&lt;A as <a class=\"trait\" href=\"smallvec/trait.Array.html\" title=\"trait smallvec::Array\">Array</a>&gt;::<a class=\"type\" href=\"smallvec/trait.Array.html#associatedtype.Item\" title=\"type smallvec::Array::Item\">Item</a>]&gt; for <a class=\"struct\" href=\"smallvec/struct.SmallVec.html\" title=\"struct smallvec::SmallVec\">SmallVec</a>&lt;A&gt;","synthetic":false,"types":["smallvec::SmallVec"]}];
implementors["supercow"] = [{"text":"impl&lt;'a, OWNED, BORROWED:&nbsp;?<a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/marker/trait.Sized.html\" title=\"trait core::marker::Sized\">Sized</a>, SHARED, STORAGE, PTR&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;BORROWED&gt; for <a class=\"struct\" href=\"supercow/struct.Supercow.html\" title=\"struct supercow::Supercow\">Supercow</a>&lt;'a, OWNED, BORROWED, SHARED, STORAGE, PTR&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;BORROWED: 'a,<br>&nbsp;&nbsp;&nbsp;&nbsp;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.pointer.html\">*const BORROWED</a>: <a class=\"trait\" href=\"supercow/ext/trait.PointerFirstRef.html\" title=\"trait supercow::ext::PointerFirstRef\">PointerFirstRef</a>,<br>&nbsp;&nbsp;&nbsp;&nbsp;STORAGE: <a class=\"trait\" href=\"supercow/ext/trait.OwnedStorage.html\" title=\"trait supercow::ext::OwnedStorage\">OwnedStorage</a>&lt;OWNED, SHARED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: <a class=\"trait\" href=\"supercow/ext/trait.PtrWrite.html\" title=\"trait supercow::ext::PtrWrite\">PtrWrite</a>&lt;BORROWED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: <a class=\"trait\" href=\"supercow/ext/trait.PtrRead.html\" title=\"trait supercow::ext::PtrRead\">PtrRead</a>&lt;BORROWED&gt;,&nbsp;</span>","synthetic":false,"types":["supercow::Supercow"]}];
implementors["toml"] = [{"text":"impl <a class=\"trait\" href=\"https://doc.rust-lang.org/1.54.0/core/borrow/trait.Borrow.html\" title=\"trait core::borrow::Borrow\">Borrow</a>&lt;<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.54.0/std/primitive.str.html\">str</a>&gt; for <a class=\"struct\" href=\"toml/struct.Spanned.html\" title=\"struct toml::Spanned\">Spanned</a>&lt;<a class=\"struct\" href=\"https://doc.rust-lang.org/1.54.0/alloc/string/struct.String.html\" title=\"struct alloc::string::String\">String</a>&gt;","synthetic":false,"types":["toml::spanned::Spanned"]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()