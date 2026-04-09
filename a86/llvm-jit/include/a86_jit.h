#ifndef A86_LLVM_JIT_H
#define A86_LLVM_JIT_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Opaque JIT handle.
 */
typedef struct a86_jit a86_jit_t;

/*
 * Result of a run.
 *
 * On success:
 *   ok = 1
 *   value = returned machine integer
 *   error_message = NULL
 *
 * On failure:
 *   ok = 0
 *   error_message = pointer to a NUL-terminated message owned by the JIT
 *   value is unspecified
 */
typedef struct {
  int ok;
  int64_t value;
  const char* error_message;
} a86_jit_result_t;

/*
 * Create and destroy a JIT instance.
 *
 * a86_jit_create returns NULL on failure.
 */
a86_jit_t* a86_jit_create(void);
void a86_jit_destroy(a86_jit_t* jit);

/*
 * Clear the last error stored in the JIT, if any.
 * Mostly useful internally; Racket may not need this directly.
 */
void a86_jit_clear_error(a86_jit_t* jit);

/*
 * Return the last error message for this JIT, or NULL if none.
 *
 * The returned pointer is owned by the JIT and remains valid until the next
 * operation on that JIT or until the JIT is destroyed.
 */
const char* a86_jit_last_error(a86_jit_t* jit);

/*
 * Install a host symbol that JIT-compiled code may reference by name.
 *
 * Examples: "heap", "error_handler", etc.
 *
 * Returns 1 on success, 0 on failure.
 */
int a86_jit_define_symbol(a86_jit_t* jit, const char* name, void* addr);

/*
 * Remove all previously defined host symbols from this JIT instance.
 *
 * Returns 1 on success, 0 on failure.
 */
int a86_jit_clear_symbols(a86_jit_t* jit);

/*
 * Stage a global variable assignment for the next run.
 *
 * If the named symbol is defined by one of the linked object files, then
 * `a86_jit_run` will store `value` into that global before calling the entry
 * point. If the symbol is not present, the assignment is ignored.
 *
 * Returns 1 on success, 0 on failure.
 */
int a86_jit_set_global(a86_jit_t* jit, const char* name, void* value);

/*
 * Remove all global assignments previously staged for future runs.
 *
 * Returns 1 on success, 0 on failure.
 */
int a86_jit_clear_globals(a86_jit_t* jit);

/*
 * Stage an object file to be linked into the next run.
 *
 * The path must name a relocatable object file (for example, a `.o` file).
 * Staged object files are loaded into the JIT together with the assembled
 * program when `a86_jit_run` is called, and are discarded after that run
 * completes.
 *
 * Returns 1 on success, 0 on failure.
 */
int a86_jit_add_object_file(a86_jit_t* jit, const char* path);

/*
 * Remove all object files previously staged for future runs.
 *
 * This affects only object files queued for subsequent calls to
 * `a86_jit_run`; it does not modify code that has already been executed.
 *
 * Returns 1 on success, 0 on failure.
 */
int a86_jit_clear_object_files(a86_jit_t* jit);

/*
 * Assemble and execute one assembly program.
 *
 * asm_text must be a complete assembly source file in the target assembler
 * syntax expected by the LLVM MC layer.
 *
 * entry_name is the unmangled logical entry name, e.g. "entry".
 * On Mach-O, the assembly source will typically declare this as "_entry".
 *
 * heap is passed as the first argument to the entry point. Programs that do
 * not use a heap argument may ignore it.
 *
 * The program is loaded temporarily for this call and then discarded.
 */
a86_jit_result_t a86_jit_run(a86_jit_t* jit,
			     const char* asm_text,
			     const char* entry_name,
			     void* heap);

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* A86_LLVM_JIT_H */
