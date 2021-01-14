(function() {var implementors = {};
implementors["console"] = [{"text":"impl&lt;D:&nbsp;Binary&gt; Binary for StyledObject&lt;D&gt;","synthetic":false,"types":[]}];
implementors["env_logger"] = [{"text":"impl&lt;'a, T:&nbsp;Binary&gt; Binary for StyledValue&lt;'a, T&gt;","synthetic":false,"types":[]}];
implementors["git2"] = [{"text":"impl Binary for Sort","synthetic":false,"types":[]},{"text":"impl Binary for CredentialType","synthetic":false,"types":[]},{"text":"impl Binary for IndexEntryFlag","synthetic":false,"types":[]},{"text":"impl Binary for IndexEntryExtendedFlag","synthetic":false,"types":[]},{"text":"impl Binary for IndexAddOption","synthetic":false,"types":[]},{"text":"impl Binary for RepositoryOpenFlags","synthetic":false,"types":[]},{"text":"impl Binary for RevparseMode","synthetic":false,"types":[]},{"text":"impl Binary for MergeAnalysis","synthetic":false,"types":[]},{"text":"impl Binary for MergePreference","synthetic":false,"types":[]},{"text":"impl Binary for Status","synthetic":false,"types":[]},{"text":"impl Binary for RepositoryInitMode","synthetic":false,"types":[]},{"text":"impl Binary for SubmoduleStatus","synthetic":false,"types":[]},{"text":"impl Binary for PathspecFlags","synthetic":false,"types":[]},{"text":"impl Binary for CheckoutNotificationType","synthetic":false,"types":[]},{"text":"impl Binary for DiffStatsFormat","synthetic":false,"types":[]},{"text":"impl Binary for StashApplyFlags","synthetic":false,"types":[]},{"text":"impl Binary for StashFlags","synthetic":false,"types":[]},{"text":"impl Binary for AttrCheckFlags","synthetic":false,"types":[]},{"text":"impl Binary for DiffFlags","synthetic":false,"types":[]},{"text":"impl Binary for ReferenceFormat","synthetic":false,"types":[]}];
implementors["itertools"] = [{"text":"impl&lt;'a, I&gt; Binary for Format&lt;'a, I&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;I: Iterator,<br>&nbsp;&nbsp;&nbsp;&nbsp;I::Item: Binary,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["nix"] = [{"text":"impl Binary for AtFlags","synthetic":false,"types":[]},{"text":"impl Binary for OFlag","synthetic":false,"types":[]},{"text":"impl Binary for SealFlag","synthetic":false,"types":[]},{"text":"impl Binary for FdFlag","synthetic":false,"types":[]},{"text":"impl Binary for SpliceFFlags","synthetic":false,"types":[]},{"text":"impl Binary for FallocateFlags","synthetic":false,"types":[]},{"text":"impl Binary for ModuleInitFlags","synthetic":false,"types":[]},{"text":"impl Binary for DeleteModuleFlags","synthetic":false,"types":[]},{"text":"impl Binary for MsFlags","synthetic":false,"types":[]},{"text":"impl Binary for MntFlags","synthetic":false,"types":[]},{"text":"impl Binary for MQ_OFlag","synthetic":false,"types":[]},{"text":"impl Binary for FdFlag","synthetic":false,"types":[]},{"text":"impl Binary for InterfaceFlags","synthetic":false,"types":[]},{"text":"impl Binary for PollFlags","synthetic":false,"types":[]},{"text":"impl Binary for CloneFlags","synthetic":false,"types":[]},{"text":"impl Binary for EpollFlags","synthetic":false,"types":[]},{"text":"impl Binary for EpollCreateFlags","synthetic":false,"types":[]},{"text":"impl Binary for EfdFlags","synthetic":false,"types":[]},{"text":"impl Binary for MemFdCreateFlag","synthetic":false,"types":[]},{"text":"impl Binary for ProtFlags","synthetic":false,"types":[]},{"text":"impl Binary for MapFlags","synthetic":false,"types":[]},{"text":"impl Binary for MsFlags","synthetic":false,"types":[]},{"text":"impl Binary for MlockAllFlags","synthetic":false,"types":[]},{"text":"impl Binary for Options","synthetic":false,"types":[]},{"text":"impl Binary for QuotaValidFlags","synthetic":false,"types":[]},{"text":"impl Binary for SaFlags","synthetic":false,"types":[]},{"text":"impl Binary for SfdFlags","synthetic":false,"types":[]},{"text":"impl Binary for SockFlag","synthetic":false,"types":[]},{"text":"impl Binary for MsgFlags","synthetic":false,"types":[]},{"text":"impl Binary for SFlag","synthetic":false,"types":[]},{"text":"impl Binary for Mode","synthetic":false,"types":[]},{"text":"impl Binary for FsFlags","synthetic":false,"types":[]},{"text":"impl Binary for InputFlags","synthetic":false,"types":[]},{"text":"impl Binary for OutputFlags","synthetic":false,"types":[]},{"text":"impl Binary for ControlFlags","synthetic":false,"types":[]},{"text":"impl Binary for LocalFlags","synthetic":false,"types":[]},{"text":"impl Binary for WaitPidFlag","synthetic":false,"types":[]},{"text":"impl Binary for AddWatchFlags","synthetic":false,"types":[]},{"text":"impl Binary for InitFlags","synthetic":false,"types":[]},{"text":"impl Binary for AccessFlags","synthetic":false,"types":[]}];
implementors["openssl"] = [{"text":"impl Binary for CMSOptions","synthetic":false,"types":[]},{"text":"impl Binary for OcspFlag","synthetic":false,"types":[]},{"text":"impl Binary for Pkcs7Flags","synthetic":false,"types":[]},{"text":"impl Binary for SslOptions","synthetic":false,"types":[]},{"text":"impl Binary for SslMode","synthetic":false,"types":[]},{"text":"impl Binary for SslVerifyMode","synthetic":false,"types":[]},{"text":"impl Binary for SslSessionCacheMode","synthetic":false,"types":[]},{"text":"impl Binary for ExtensionContext","synthetic":false,"types":[]},{"text":"impl Binary for ShutdownState","synthetic":false,"types":[]},{"text":"impl Binary for X509CheckFlags","synthetic":false,"types":[]},{"text":"impl Binary for X509VerifyFlags","synthetic":false,"types":[]}];
implementors["supercow"] = [{"text":"impl&lt;'a, OWNED, BORROWED:&nbsp;?Sized, SHARED, STORAGE, PTR&gt; Binary for Supercow&lt;'a, OWNED, BORROWED, SHARED, STORAGE, PTR&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;BORROWED: 'a,<br>&nbsp;&nbsp;&nbsp;&nbsp;*const BORROWED: PointerFirstRef,<br>&nbsp;&nbsp;&nbsp;&nbsp;STORAGE: OwnedStorage&lt;OWNED, SHARED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: PtrWrite&lt;BORROWED&gt;,<br>&nbsp;&nbsp;&nbsp;&nbsp;BORROWED: Binary,<br>&nbsp;&nbsp;&nbsp;&nbsp;PTR: PtrRead&lt;BORROWED&gt;,&nbsp;</span>","synthetic":false,"types":[]}];
implementors["tinyvec"] = [{"text":"impl&lt;A:&nbsp;Array&gt; Binary for ArrayVec&lt;A&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;A::Item: Binary,&nbsp;</span>","synthetic":false,"types":[]},{"text":"impl&lt;'s, T&gt; Binary for SliceVec&lt;'s, T&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;T: Binary,&nbsp;</span>","synthetic":false,"types":[]},{"text":"impl&lt;A:&nbsp;Array&gt; Binary for TinyVec&lt;A&gt; <span class=\"where fmt-newline\">where<br>&nbsp;&nbsp;&nbsp;&nbsp;A::Item: Binary,&nbsp;</span>","synthetic":false,"types":[]}];
if (window.register_implementors) {window.register_implementors(implementors);} else {window.pending_implementors = implementors;}})()