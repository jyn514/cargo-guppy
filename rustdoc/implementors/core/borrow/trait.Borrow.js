(function() {var implementors = {};
implementors["ascii"] = [{"text":"impl Borrow&lt;AsciiStr&gt; for AsciiString","synthetic":false,"types":[]}];
implementors["bstr"] = [{"text":"impl Borrow&lt;BStr&gt; for BString","synthetic":false,"types":[]}];
implementors["cargo"] = [{"text":"impl Borrow&lt;str&gt; for InternedString","synthetic":false,"types":[]}];
implementors["crossbeam_epoch"] = [{"text":"impl&lt;T:&nbsp;?Sized + Pointable&gt; Borrow&lt;T&gt; for Owned&lt;T&gt;","synthetic":false,"types":[]}];
implementors["openssl"] = [{"text":"impl Borrow&lt;Asn1GeneralizedTimeRef&gt; for Asn1GeneralizedTime","synthetic":false,"types":[]},{"text":"impl Borrow&lt;Asn1TimeRef&gt; for Asn1Time","synthetic":false,"types":[]},{"text":"impl Borrow&lt;Asn1StringRef&gt; for Asn1String","synthetic":false,"types":[]},{"text":"impl Borrow&lt;Asn1IntegerRef&gt; for Asn1Integer","synthetic":false,"types":[]},{"text":"impl Borrow&lt;Asn1BitStringRef&gt; for Asn1BitString","synthetic":false,"types":[]},{"text":"impl Borrow&lt;Asn1ObjectRef&gt; for Asn1Object","synthetic":false,"types":[]},{"text":"impl Borrow&lt;BigNumContextRef&gt; for BigNumContext","synthetic":false,"types":[]},{"text":"impl Borrow&lt;BigNumRef&gt; for BigNum","synthetic":false,"types":[]},{"text":"impl Borrow&lt;CmsContentInfoRef&gt; for CmsContentInfo","synthetic":false,"types":[]},{"text":"impl Borrow&lt;ConfRef&gt; for Conf","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; Borrow&lt;DhRef&lt;T&gt;&gt; for Dh&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; Borrow&lt;DsaRef&lt;T&gt;&gt; for Dsa&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl Borrow&lt;EcGroupRef&gt; for EcGroup","synthetic":false,"types":[]},{"text":"impl Borrow&lt;EcPointRef&gt; for EcPoint","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; Borrow&lt;EcKeyRef&lt;T&gt;&gt; for EcKey&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl Borrow&lt;EcdsaSigRef&gt; for EcdsaSig","synthetic":false,"types":[]},{"text":"impl Borrow&lt;OcspBasicResponseRef&gt; for OcspBasicResponse","synthetic":false,"types":[]},{"text":"impl Borrow&lt;OcspCertIdRef&gt; for OcspCertId","synthetic":false,"types":[]},{"text":"impl Borrow&lt;OcspResponseRef&gt; for OcspResponse","synthetic":false,"types":[]},{"text":"impl Borrow&lt;OcspRequestRef&gt; for OcspRequest","synthetic":false,"types":[]},{"text":"impl Borrow&lt;OcspOneReqRef&gt; for OcspOneReq","synthetic":false,"types":[]},{"text":"impl Borrow&lt;Pkcs12Ref&gt; for Pkcs12","synthetic":false,"types":[]},{"text":"impl Borrow&lt;Pkcs7Ref&gt; for Pkcs7","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; Borrow&lt;PKeyRef&lt;T&gt;&gt; for PKey&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; Borrow&lt;RsaRef&lt;T&gt;&gt; for Rsa&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl Borrow&lt;SrtpProtectionProfileRef&gt; for SrtpProtectionProfile","synthetic":false,"types":[]},{"text":"impl Borrow&lt;SslContextRef&gt; for SslContext","synthetic":false,"types":[]},{"text":"impl Borrow&lt;SslSessionRef&gt; for SslSession","synthetic":false,"types":[]},{"text":"impl Borrow&lt;SslRef&gt; for Ssl","synthetic":false,"types":[]},{"text":"impl&lt;T:&nbsp;Stackable&gt; Borrow&lt;StackRef&lt;T&gt;&gt; for Stack&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl Borrow&lt;OpensslStringRef&gt; for OpensslString","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509VerifyParamRef&gt; for X509VerifyParam","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509StoreBuilderRef&gt; for X509StoreBuilder","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; Borrow&lt;X509LookupRef&lt;T&gt;&gt; for X509Lookup&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl&lt;T&gt; Borrow&lt;X509LookupMethodRef&lt;T&gt;&gt; for X509LookupMethod&lt;T&gt;","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509StoreRef&gt; for X509Store","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509StoreContextRef&gt; for X509StoreContext","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509Ref&gt; for X509","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509ExtensionRef&gt; for X509Extension","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509NameRef&gt; for X509Name","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509NameEntryRef&gt; for X509NameEntry","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509ReqRef&gt; for X509Req","synthetic":false,"types":[]},{"text":"impl Borrow&lt;GeneralNameRef&gt; for GeneralName","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509AlgorithmRef&gt; for X509Algorithm","synthetic":false,"types":[]},{"text":"impl Borrow&lt;X509ObjectRef&gt; for X509Object","synthetic":false,"types":[]}];
implementors["sized_chunks"] = [{"text":"impl&lt;A, T&gt; Borrow&lt;[A]&gt; for InlineArray&lt;A, T&gt;","synthetic":false,"types":[]},{"text":"impl&lt;A, N&gt; Borrow&lt;[A]&gt; for Chunk&lt;A, N&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;N: ChunkLength&lt;A&gt;,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["smallvec"] = [{"text":"impl&lt;A:&nbsp;Array&gt; Borrow&lt;[&lt;A as Array&gt;::Item]&gt; for SmallVec&lt;A&gt;","synthetic":false,"types":[]}];
implementors["supercow"] = [{"text":"impl&lt;'a, OWNED, BORROWED:&nbsp;?Sized, SHARED, STORAGE, PTR&gt; Borrow&lt;BORROWED&gt; for Supercow&lt;'a, OWNED, BORROWED, SHARED, STORAGE, PTR&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;BORROWED: 'a,<br>&nbsp;&nbsp;&nbsp;&nbsp;*const BORROWED: PointerFirstRef,<br>&nbsp;&nbsp;&nbsp;&nbsp;STORAGE: OwnedStorage&lt;OWNED, SHARED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: PtrWrite&lt;BORROWED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: PtrRead&lt;BORROWED&gt;,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["tinyvec"] = [{"text":"impl&lt;A:&nbsp;Array&gt; Borrow&lt;[&lt;A as Array&gt;::Item]&gt; for ArrayVec&lt;A&gt;","synthetic":false,"types":[]},{"text":"impl&lt;'s, T&gt; Borrow&lt;[T]&gt; for SliceVec&lt;'s, T&gt;","synthetic":false,"types":[]},{"text":"impl&lt;A:&nbsp;Array&gt; Borrow&lt;[&lt;A as Array&gt;::Item]&gt; for TinyVec&lt;A&gt;","synthetic":false,"types":[]}];
implementors["toml"] = [{"text":"impl Borrow&lt;str&gt; for Spanned&lt;String&gt;","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()