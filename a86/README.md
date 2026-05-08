# Developer Notes

This directory contains the `a86` collection, including the native JIT support under `native/`.

## Building Native Libraries Locally

Local native builds are only needed when you want to rebuild or restage the JIT libraries instead of using the packaged prebuilt artifacts.

You will need:

- `x86_64` Racket
- `make` or `gmake`
- a C++17 compiler (`clang++` by default in `native/Makefile`)
- LLVM with `llvm-config` available on `PATH`

The GitHub workflows currently build against LLVM 22.

From [`a86/native`](/Users/dvanhorn/git/a86/a86/native), run:

```bash
make
```

This produces the shared library in `a86/native/lib/`.

If you want to refresh the packaged prebuilt layout after building, run:

```bash
racket a86/native/stage-prebuilt.rkt
```

At package install time, `a86/pre-install.rkt` first tries to copy a matching prebuilt library and only falls back to `make` when no packaged binary is available for the target OS and architecture.
