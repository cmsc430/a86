#ifndef A86_JIT_H
#define A86_JIT_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct a86_jit a86_jit_t;
typedef struct a86_program a86_program_t;

typedef struct {
  int ok;                 /* 1 on success, 0 on failure */
  int64_t value;          /* return value from called label */
  const char *error_message;
} a86_call_result_t;

/*
 * Extern binding kinds.
 *
 * FUNCTION:
 *   `value` is the address of a function to use for the named symbol.
 *
 * GLOBAL:
 *   `value` is the address of storage for the named symbol.
 *   This is for extern data symbols, not for patching already-defined globals.
 */
typedef enum {
  A86_EXTERN_FUNCTION = 0,
  A86_EXTERN_GLOBAL   = 1
} a86_extern_kind_t;

/*
 * Create / destroy a JIT session.
 *
 * A JIT session owns the long-lived LLVM/ORC state. Multiple programs may be
 * loaded into the same session over time.
 */
a86_jit_t *a86_jit_create(void);
void a86_jit_destroy(a86_jit_t *jit);

/*
 * Load one program into the JIT.
 *
 * `asm_text` is the assembly source for the program being loaded.
 *
 * `object_files` is an array of paths to relocatable object files (.o) that
 * should be linked with the program.
 *
 * `extern_names`, `extern_kinds`, and `extern_values` are parallel arrays of
 * host-provided external symbol bindings used to resolve names declared with
 * `Extern`.
 *
 * On success, returns a live program handle. On failure, returns NULL and the
 * error may be retrieved with `a86_jit_last_error`.
 */
a86_program_t *a86_jit_load(a86_jit_t *jit,
                            const char *asm_text,
                            const char *const *object_files,
                            int object_file_count,
                            const char *const *extern_names,
                            const int *extern_kinds,
                            void *const *extern_values,
                            int extern_count);

/*
 * Unload a previously loaded program.
 *
 * It is safe to call this at most once for a given program handle.
 */
void a86_program_unload(a86_program_t *program);

/*
 * Call a label in a loaded program with machine-word arguments.
 *
 * `label` is the unmangled label name to call, e.g. "entry".
 *
 * `argv` is an array of machine-word arguments. The first few are passed using
 * the platform ABI’s normal argument registers.
 *
 * The loaded program remains live after the call returns; static data referenced
 * by the returned value therefore remains valid until `a86_program_unload`.
 */
a86_call_result_t a86_program_call(a86_program_t *program,
                                   const char *label,
                                   const uint64_t *argv,
                                   int argc);

/*
 * Return the last session-level error string for the JIT, or NULL if none.
 *
 * The returned pointer is owned by the JIT and remains valid until the next JIT
 * operation or until the JIT is destroyed.
 */
const char *a86_jit_last_error(a86_jit_t *jit);

#ifdef __cplusplus
}
#endif

#endif
