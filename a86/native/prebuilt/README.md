Prebuilt JIT libraries live here under:

`<os>/<arch>/liba86_jit.<suffix>`

Examples:

- `macosx/x86_64/liba86_jit.dylib`
- `unix/x86_64/liba86_jit.so`

To stage the library built in `a86/native/lib/` into this layout:

```bash
racket a86/native/stage-prebuilt.rkt
```

At package install time, `a86/pre-install.rkt` will copy a matching prebuilt
library into `a86/native/lib/` and will only fall back to invoking `make` when
no packaged prebuilt is available for the target OS/architecture.
