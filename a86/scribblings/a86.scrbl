#lang scribble/manual

@(require (for-label (except-in racket compile)
                     a86))

@(require scribble/examples
	  ;redex/reduction-semantics
          ;redex/pict
	  ;(only-in pict scale)
	  (only-in racket system)
	  "fancyverb.rkt"
          "shell.rkt"
	  "utils.rkt"
	  "ev.rkt")

@(ev '(require rackunit a86))

@(ev `(current-directory ,(path->string a86)))

@(define (shellbox . s)
   (parameterize ([current-directory a86])
     (filebox (emph "shell")
              (fancyverbatim "fish" (apply shell s)))))

@; compile time generation of tri.s and main.c so they can be listed
@(require (for-syntax racket/base "utils.rkt"))
@(begin-for-syntax
   (require racket/system a86/ast a86/printer)
   (define (tri n)
      (list (Global 'entry)
         (Label 'entry)
         (Mov 'rbx n)      (% "the \"input\"")
         (%%% "tri: a recursive function for computing nth")
         (%%% "triangular number, where n is given in rbx.")
         (Label 'tri)
         (Cmp 'rbx 0)      (% "if rbx = 0, done")
         (Je 'done)
         (Push 'rbx)       (% "save rbx")
         (Sub 'rbx 1)
         (Call 'tri)       (% "compute tri(rbx-1) in rax")
         (Pop 'rbx)        (% "restore rbx")
         (Add 'rax 'rbx)   (% "result is rbx+tri(rbx-1)")
         (Ret)
         (Label 'done)     (% "jump here for base case")
         (Mov 'rax 0)      (% "return 0")
         (Ret)))

   (define main.c
     #<<HERE
#include <stdio.h>
#include <inttypes.h>

int64_t entry();

int main(int argc, char** argv) {
  int64_t result = entry();
  printf("%" PRId64 "\n", result);
  return 0;
}
HERE
     )

   (define gcd.c
     #<<HERE
int gcd(int n1, int n2) {
    return (n2 == 0) ? n1 : gcd(n2, n1 % n2);
}
HERE
     )

   (parameterize ([current-directory a86])
     (save-file "tri.s" (asm-string (tri 36)))
     (save-file "main.c" main.c)
     (save-file "gcd.c" gcd.c)))



@title{a86 Reference}

@defmodule[a86]

@margin-note{The a86 language may evolve some over the
 course of the semester, but we will aim to document any
 changes by updating this section. Also, because the run-time
 system changes for each language, you may need to do some
 work to have @racket[asm-interp] cooperate with your
 run-time system.}

This module provides all of the bindings from
@racketmodname[a86/ast], @racketmodname[a86/printer],
and @racketmodname[a86/interp], described below.


@section[#:tag "a86-flags"]{Flags}

As mentioned earlier, the processor makes use of @emph{flags} to
handle comparisons. For our purposes, there are four flags to
be aware of: zero (ZF), sign (SF), carry (CF), and overflow (OF).

These flags are set by each of the arithmetic operations, which
are appropriately annotated in the @secref{a86-instructions}.
Each of these operations is binary (meaning they take two
arguments), and the flags are set according to properties of
the result of the arithmetic operation. Many of these properties
look at the most-significant bit (MSB) of the inputs and output.

@itemlist[
 @item{@bold{ZF} is set when the result is @tt{0}.}
 @item{@bold{SF} is set when the MSB of the result is set.}
 @item{@bold{CF} is set when a bit was set beyond the MSB.}
 @item{@bold{OF} is set when one of two conditions is met:

   @itemlist[#:style 'ordered
    @item{The MSB of each input is @emph{set} and the MSB of
          the result is @emph{not set}.}
    @item{The MSB of each input is @emph{not set} and the MSB
          of the result is @emph{set}.}
   ]}
]

Note that CF is only useful for unsigned arithmetic, while OF
is only useful for signed arithmetic. In opposite cases, they
provide no interesting information.

These flags, along with many others, are stored in a special
FLAGS register that cannot be accessed by normal means. Each
flag is represented by a single bit in the register, and they
all have specific bits assigned by the x86 specification. For
example, CF is bit 0, ZF is bit 6, SF is bit 7, and OF is bit
11, as indexed from the least-significant bit position (but
you don't need to know these numbers).

The various conditions that can be tested for correspond to
combinations of the flags. For example, the @racket[Jc]
instruction will jump if CF is set, otherwise execution will
fall through to the next instruction. Most of the condition
suffixes are straightforward to deduce from their spelling,
but some are not. The suffixes (e.g., the @tt{c} in @tt{Jc})
and their meanings are given below. For brevity's sake the
flags' names are abbreviated by ommitting the F suffix and
prefixing them with either @tt{+} or @tt{-} to indicate set
and unset positions, respectively, as needed. Some of the
meanings require use of the bitwise operators @tt{|} (OR),
@tt{&} (AND), @tt{^} (XOR), and @tt{=?} (equality).

@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
  (list (list @bold{Suffix} @bold{Flag}      @bold{Suffix} @bold{Flag})
        (list @tt{z}        @tt{+Z}          @tt{nz}       @tt{-Z})
        (list @tt{e}        @tt{+Z}          @tt{ne}       @tt{-Z})
        (list @tt{s}        @tt{+S}          @tt{ns}       @tt{-S})
        (list @tt{c}        @tt{+C}          @tt{nc}       @tt{-C})
        (list @tt{o}        @tt{+O}          @tt{no}       @tt{-O})
        (list @tt{l}        @tt{      (S ^ O)}    @tt{g}        @tt{(-Z & (S =? O))})
        (list @tt{le}       @tt{(+Z | (S ^ O))}  @tt{ge}       @tt{      (S =? O)}))]

The @tt{e} suffix (``equal?'') is just a synonym
for the @tt{z} suffix (``zero?''). This is because it is
common to use the @racket[Cmp] instruction to perform
comparisons, but @racket[Cmp] is actually identical to
@racket[Sub] with the exception that the result is not
stored anywhere (i.e., it is only used for setting flags
according to subtraction). If two values are subtracted
and the resulting difference is zero (ZF is set), then the
values are equal.

@section[#:tag "a86-instructions"]{Instruction set}

@defmodule[a86/ast]

This section describes the instruction set of a86.

There are 16 registers: @racket['rax], @racket['rbx], @racket['rcx],
@racket['rdx], @racket['rbp], @racket['rsp], @racket['rsi],
@racket['rdi], @racket['r8], @racket['r9], @racket['r10],
@racket['r11], @racket['r12], @racket['r13], @racket['r14], and
@racket['r15]. These registers are 64-bits wide.  There is also
@racket['eax] which accesses the lower 32-bits of @racket['rax].
This is useful in case you need to read or write 32-bits of memory.

The registers @racket['rbx], @racket['rsp], @racket['rbp], and
@racket['r12] through @racket['r15] are ``callee-saved'' registers,
meaning they are preserved across function calls (and must be saved
and restored by any callee code).

Each register plays the same role as in x86, so for example
@racket['rsp] holds the current location of the stack.

@defproc[(register? [x any/c]) boolean?]{
 A predicate for registers.
}

@defproc[(label? [x any/c]) boolean?]{
 A predicate for label @emph{names}, i.e. symbols which are not register names.

 Labels must also follow the NASM restrictions on label names: "Valid
 characters in labels are letters, numbers, @tt{_}, @tt{$}, @tt{#}, @tt{@"@"}, @tt{~}, @tt{.}, and
 @tt{?}. The only characters which may be used as the first character of an
 identifier are letters, @tt{.} (with special meaning), @tt{_}
 and @tt{?}."

 @ex[
 (label? 'foo)
 (label? "foo")
 (label? 'rax)
 (label? 'foo-bar)
 (label? 'foo.bar)
 ]

}

@defproc[(instruction? [x any/c]) boolean?]{
 A predicate for instructions.
}

@defproc[(offset? [x any/c]) boolean?]{
 A predicate for offsets.
}

@defproc[(64-bit-integer? [x any/c]) boolean?]{
 A predicate for determining if a value is an integer that fits in 64-bits.

 @ex[
 (64-bit-integer? 0)
 (64-bit-integer? (sub1 (expt 2 64)))
 (64-bit-integer? (expt 2 64))
 (64-bit-integer? (- (expt 2 63)))
 (64-bit-integer? (sub1 (- (expt 2 63))))]
}

@defproc[(32-bit-integer? [x any/c]) boolean?]{
 A predicate for determining if a value is an integer that fits in 64-bits.

 @ex[
 (32-bit-integer? 0)
 (32-bit-integer? (sub1 (expt 2 32)))
 (32-bit-integer? (expt 2 32))
 (32-bit-integer? (- (expt 2 32)))
 (32-bit-integer? (sub1 (- (expt 2 32))))]
}

@defproc[(seq [x (or/c instruction? (listof instruction?))] ...) (listof instruction?)]{
 A convenience function for splicing togeter instructions and lists of instructions.

  @ex[
 (seq)
 (seq (Label 'foo))
 (seq (list (Label 'foo)))
 (seq (list (Label 'foo)
            (Mov 'rax 0))
      (Mov 'rdx 'rax)
      (list (Call 'bar)
            (Ret)))
 ]
}

@defproc[(prog [x (or/c instruction? (listof instruction?))] ...) (listof instruction?)]{

 Like @racket[seq], but also checks that the instructions
 are well-formed in the following sense:

 @itemlist[

 @item{Programs have at least one label which is declared @racket[Global]; the first label is used as the entry point.}
 @item{All label declarations are unique.}
 @item{All label targets are declared.}
 @item{... other properties may be added in the future.}

 ]

 This function is useful to do some early error checking
 over whole programs and can help avoid confusing NASM
 errors. Unlike @racket[seq] it should be called at the
 outermost level of a function that produces a86 code and not
 nested.

 @ex[
 (prog (Global 'foo) (Label 'foo))
 (eval:error (prog (Label 'foo)))
 (eval:error (prog (list (Label 'foo))))
 (eval:error (prog (Mov 'rax 32)))
 (eval:error (prog (Label 'foo)
                   (Label 'foo)))
 (eval:error (prog (Jmp 'foo)))
 (prog (Global 'foo)
       (Label 'foo)
       (Jmp 'foo))
 ]
}

@defproc[(symbol->label [s symbol?]) label?]{

  Returns a modified form of a symbol that follows NASM label conventions.

  @ex[
  (let ([l (symbol->label 'my-great-label)])
    (seq (Label l)
         (Jmp l)))
  ]
}

@deftogether[(@defstruct*[% ([s string?])]
               @defstruct*[%% ([s string?])]
               @defstruct*[%%% ([s string?])])]{

 Creates a comment in the assembly code. The @racket[%]
 constructor adds a comment toward the right side of the
 current line; @racket[%%] creates a comment on its own line
 1 tab over; @racket[%%%] creates a comment on its own line
 aligned to the left.

 @#reader scribble/comment-reader
 (ex
 (asm-display
   (prog (Global 'foo)
         (%%% "Start of foo")
         (Label 'foo)
         ; Racket comments won't appear
         (%% "Inputs one argument in rdi")
         (Mov 'rax 'rdi)
         (Add 'rax 'rax)    (% "double it")
         (Sub 'rax 1)       (% "subtract one")
         (%% "we're done!")
         (Ret))))
}

@defstruct*[Offset ([r register?] [i exact-integer?])]{

 Creates an memory offset from a register. Offsets are used
 as arguments to instructions to indicate memory locations.
 An error is signalled when given invalid inputs.

 @ex[
 (Offset 'rax 0)
 (eval:error (Offset 'rax 4.1))
 ]
}

@defstruct*[Text ()]{

 Declares the start of a text section, which includes instructions to
 be executed.

}

@defstruct*[Data ()]{

 Declares the start of a data section, which includes data and constants.

}

@defstruct*[Label ([x label?])]{

 Creates a label from the given symbol. Each label in a
 program must be unique.  Labels must follow the NASM restrictions
 on valid label names (see @racket[label?] for details).

 @ex[
 (Label 'fred)
 (eval:error (Label "fred"))
 (eval:error (Label 'fred-wilma))
 ]

}

@defstruct*[Extern ([x label?])]{

 Declares an external label.

}

@defstruct*[Global ([x label?])]{

 Declares a label as global, i.e. linkable with other object files.

}


@defstruct*[Call  ([x (or/c label? register?)])]{

 A call instruction.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Call 'f)
   (Add 'rax 1)
   (Ret)
   (Label 'f)
   (Mov 'rax 41)
   (Ret)))
 ]
}

@defstruct*[Ret ()]{

 A return instruction.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Ret)))
 ]

}

@defstruct*[Mov ([dst (or/c register? offset?)] [src (or/c register? offset? 64-bit-integer?)])]{

 A move instruction. Moves @racket[src] to @racket[dst].

 Either @racket[dst] or @racket[src] may be offsets, but not both.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rbx 42)
   (Mov 'rax 'rbx)
   (Ret)))
 (eval:error (Mov (Offset 'rax 0) (Offset 'rbx 0)))
 ]

}

@defstruct*[Add ([dst register?] [src (or/c register? offset? 32-bit-integer?)])]{

 An addition instruction. Adds @racket[src] to @racket[dst]
 and writes the result to @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 32)
   (Add 'rax 10)
   (Ret)))
 ]
}

@defstruct*[Sub ([dst register?] [src (or/c register? offset? 32-bit-integer?)])]{

 A subtraction instruction. Subtracts @racket[src] from
 @racket[dst] and writes the result to @racket[dst].
 Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 32)
   (Sub 'rax 10)
   (Ret)))
 ]
}

@defstruct*[Cmp ([a1 (or/c register? offset?)] [a2 (or/c register? offset? 32-bit-integer?)])]{
 Compare @racket[a1] to @racket[a2] by subtracting @racket[a2] from @racket[a1]
 and updating the comparison flags. Does not store the result of subtraction.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jg 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jmp ([x (or/c label? register?)])]{
 Jump to label @racket[x].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Jmp 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Pop 'rbx)
   (Jmp 'rbx)))
 ]

}

@defstruct*[Jz ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the zero flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jz 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jnz ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the zero flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jnz 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Je ([x (or/c label? register?)])]{
 An alias for @racket[Jz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Je 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jne ([x (or/c label? register?)])]{
 An alias for @racket[Jnz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jne 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jl ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``less than'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jl 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jle ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``less than or equal'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 42)
   (Jle 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jg ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``greater than'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 2)
   (Jg 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jge ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``greater than or equal'' (see @secref{a86-flags}).

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Cmp 'rax 42)
   (Jg 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jo ([x (or/c label? register?)])]{
 Jump to @racket[x] if the overflow flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (sub1 (expt 2 63)))
   (Add 'rax 1)
   (Jo 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jno ([x (or/c label? register?)])]{
 Jump to @racket[x] if the overflow flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (sub1 (expt 2 63)))
   (Add 'rax 1)
   (Jno 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jc ([x (or/c label? register?)])]{
 Jump to @racket[x] if the carry flag is set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Add 'rax 1)
   (Jc 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Jnc ([x (or/c label? register?)])]{
 Jump to @racket[x] if the carry flag is @emph{not} set.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Add 'rax 1)
   (Jnc 'l1)
   (Mov 'rax 0)
   (Label 'l1)
   (Ret)))
 ]
}

@defstruct*[Cmovz ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the zero flag is set.

 Note that the semantics for conditional moves is not what many people expect.
 The @racket[src] is @emph{always} read, regardless of the condition's evaluation.
 This means that if your source is illegal (such as an offset beyond the bounds
 of memory allocated to the current process), a segmentation fault will arise
 even if the condition ``should have'' prevented the error.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovz 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovz 'rax 'r9)
   (Ret)))
 ]
}


@defstruct*[Cmove ([dst register?] [src (or/c register? offset?)])]{
 An alias for @racket[Cmovz]. See notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmove 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmove 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovnz ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the zero flag is @emph{not} set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovnz 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovnz 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovne ([dst register?] [src (or/c register? offset?)])]{
 An alias for @racket[Cmovnz]. See notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovne 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovne 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovl ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``less than'' (see @secref{a86-flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovl 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovl 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovle ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``less than or equal'' (see @secref{a86-flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovle 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovle 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovg ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``greather than'' (see @secref{a86-flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovg 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovg 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovge ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``greater than or equal'' (see @secref{a86-flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax -1)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovge 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 2)
   (Cmp 'rax 0)
   (Mov 'r9 1)
   (Cmovge 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovo ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the overflow flag is set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovo 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovo 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovno ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the overflow flag is @emph{not} set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovno 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 63) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovno 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovc ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the carry flag is set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovc 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovc 'rax 'r9)
   (Ret)))
 ]
}

@defstruct*[Cmovnc ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the carry flag is @emph{not} set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 1))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovnc 'rax 'r9)
   (Ret)))

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax (- (expt 2 64) 2))
   (Add 'rax 1)
   (Mov 'r9 1)
   (Cmovnc 'rax 'r9)
   (Ret)))
 ]
}


@defstruct*[And ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{

 Compute logical ``and'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1011) ; #b1011 = 11
   (And 'rax #b1110) ; #b1110 = 14
   (Ret)))           ; #b1010 = 10
 )
}

@defstruct*[Or ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{
 Compute logical ``or'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1011) ; #b1011 = 11
   (Or 'rax #b1110)  ; #b1110 = 14
   (Ret)))           ; #b1111 = 15
 )
}

@defstruct*[Xor ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{
 Compute logical ``exclusive or'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1011) ; #b1011 = 11
   (Xor 'rax #b1110) ; #b1110 = 14
   (Ret)))           ; #b0101 = 5
 )
}

@defstruct*[Sal ([dst register?] [i (integer-in 0 63)])]{
 Shift @racket[dst] to the left @racket[i] bits and put result in @racket[dst].
 The most-significant (leftmost) bits are discarded. Updates the conditional
 flags.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100) ; #b100 = 4 = 2^2
   (Sal 'rax 6)
   (Ret)))          ; #b100000000 = 256
 )
}

@defstruct*[Sar ([dst register?] [i (integer-in 0 63)])]{
 Shift @racket[dst] to the right @racket[i] bits and put result in @racket[dst].
 For each shift count, the least-significant (rightmost) bit is shifted into
 the carry flag. The new most-significant (leftmost) bits are filled with the
 sign bit of the original @racket[dst] value. Updates the conditional flags.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100000000) ; #b100000000 = 256
   (Sar 'rax 6)
   (Ret)))        ; #b100 = 4

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100001101) ; #b100001101 = 269
   (Sar 'rax 6)
   (Ret)))        ; #b100 = 4

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1000000000000000000000000000000000000000000000000000000000000000) ; 1 in MSB
   (Sar 'rax 6)
   (Ret))) ; #b1111111000000000000000000000000000000000000000000000000000000000
 )
}

@defstruct*[Shl ([dst register?] [i (integer-in 0 63)])]{
 Alias for @racket[Sal].
}

@defstruct*[Shr ([dst register?] [i (integer-in 0 63)])]{
 Shift @racket[dst] to the right @racket[i] bits and put result in @racket[dst].
 For each shift count, the least-significant (rightmost) bit is shifted into
 the carry flag, and the most-significant bit is cleared. Updates the
 conditional flags.

 @#reader scribble/comment-reader
 (ex
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100000000) ; #b100000000 = 256
   (Shr 'rax 6)
   (Ret)))        ; #b100 = 4

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b100001101) ; #b100001101 = 269
   (Shr 'rax 6)
   (Ret)))        ; #b100 = 4

 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax #b1000000000000000000000000000000000000000000000000000000000000000) ; 1 in MSB
   (Shr 'rax 6)
   (Ret))) ; #b0000001000000000000000000000000000000000000000000000000000000000
 )
}

@defstruct*[Push ([a1 (or/c 32-bit-integer? register?)])]{

 Decrements the stack pointer and then stores the source
 operand on the top of the stack.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Push 'rax)
   (Mov 'rax 0)
   (Pop 'rax)
   (Ret)))
 ]
}

@defstruct*[Pop ([a1 register?])]{
 Loads the value from the top of the stack to the destination operand and then increments the stack pointer.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Push 'rax)
   (Mov 'rax 0)
   (Pop 'rax)
   (Ret)))
 ]
}

@defstruct*[Not ([a1 register?])]{
Perform bitwise not operation (each 1 is set to 0, and each 0 is set to 1) on the destination operand.

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 0)
   (Not 'rax)
   (Ret)))
 ]
}

@defstruct*[Lea ([dst (or/c register? offset?)] [x label?])]{
 Loads the address of the given label into @racket[dst].

 @ex[
 (asm-interp
  (prog
   (Global 'entry)
   (Label 'entry)
   (Lea 'rbx 'done)
   (Mov 'rax 42)
   (Jmp 'rbx)
   (Mov 'rax 0)
   (Label 'done)
   (Ret)))
 ]
}

@defstruct*[Db ([d integer?])]{
 Psuedo-instruction for declaring 8-bits of initialized static memory.
}

@defstruct*[Dw ([d integer?])]{
 Psuedo-instruction for declaring 16-bits of initialized static memory.
}

@defstruct*[Dd ([d integer?])]{
 Psuedo-instruction for declaring 32-bits of initialized static memory.
}

@defstruct*[Dq ([d integer?])]{
 Psuedo-instruction for declaring 64-bits of initialized static memory.
}

@section{From a86 to x86}

@defmodule[a86/printer]

@defproc[(asm-display [is (listof instruction?)]) void?]{

 Prints an a86 program to the current output port in nasm syntax.

 @ex[
 (asm-display (prog (Global 'entry)
                    (Label 'entry)
                    (Mov 'rax 42)
                    (Ret)))
 ]

}

@defproc[(asm-string [is (listof instruction?)]) string?]{

 Converts an a86 program to a string in nasm syntax.

 @ex[
 (asm-string (prog (Global 'entry)
                   (Label 'entry)
                   (Mov 'rax 42)
                   (Ret)))
 ]

}

@section{An Interpreter for a86}

@defmodule[a86/interp]

As you've seen throughout this chapter, @racketmodname[a86]
is equiped with an interpreter, which enables you to run
assembly programs from within Racket. This won't be directly
useful in building a compiler, but it will be very handy for
interactively exploring assembly programs and making examples
and test cases for your compiler.

The simplest form of interpreting an a86 program is to use
@racket[asm-interp].

@defproc[(asm-interp [is (listof instruction?)]) integer?]{

 Assemble, link, and execute an a86 program.

 @ex[
 (asm-interp (prog (Global 'entry)
                   (Label 'entry)
                   (Mov 'rax 42)
                   (Ret)))
 ]

 Programs do not have to start with @racket['entry]. The
 interpreter will jump to whatever the first label in the
 program is:

@ex[
 (asm-interp (prog (Global 'f)
                   (Label 'f)
                   (Mov 'rax 42)
                   (Ret)))
 ]

 The argument of @racket[asm-interp] should be a complete,
 well-formed a86 program. For best results, always use
 @racket[prog] to construct the program so that error
 checking is done early. If you use @racket[prog] and
 @racket[asm-interp] and you get a NASM syntax error message,
 please report it to the course staff as this is a bug in the
 interpreter.

 While we try to make syntax errors impossible, it is
 possible---quite easy, in fact---to write well-formed, but
 erroneous assembly programs. For example, this program tries
 to jump to null, which causes a segmentation fault:

 @ex[
 (eval:error (asm-interp (prog (Global 'crash)
                               (Label 'crash)
                               (Mov 'rax 0)
                               (Jmp 'rax))))
 ]

}

It is often the case that we want our assembly programs to
interact with the oustide or to use functionality
implemented in other programming languages. For that reason,
it is possible to link in object files to the running of an
a86 program.

The mechanism for controlling which objects should be linked
in is a parameter called @racket[current-objs], which
contains a list of paths to object files which are linked to
the assembly code when it is interpreted.

@defparam[current-objs objs (listof path-string?) #:value '()]{

Parameter that controls object files that will be linked in to
assembly code when running @racket[asm-interp].

}

For example, let's implement a GCD function in C:

@filebox-include[fancy-c a86 "gcd.c"]

First, compile the program to an object file:

@shellbox["gcc -fPIC -c gcd.c -o gcd.o"]

The option @tt{-fPIC} is important; it causes the C compiler
to emit ``position independent code,'' which is what enables
Racket to dynamically load and run the code.

Once the object file exists, using the @racket[current-objs]
parameter, we can run code that uses things defined in the C
code:

@ex[
(parameterize ((current-objs '("gcd.o")))
  (asm-interp (prog (Extern 'gcd)
                    (Global 'f)
                    (Label 'f)
                    (Mov 'rdi 11571)
                    (Mov 'rsi 1767)
                    (Sub 'rsp 8)
                    (Call 'gcd)
                    (Add 'rsp 8)
                    (Ret))))]

This will be particularly relevant for writing a compiler
where emitted code will make use of functionality defined in
a runtime system.

Note that if you forget to set @racket[current-objs], you will get a
linking error saying a symbol is undefined:

@ex[
(eval:error
  (asm-interp (prog (Extern 'gcd)
                    (Global 'f)
                    (Label 'f)
                    (Mov 'rdi 11571)
                    (Mov 'rsi 1767)
                    (Sub 'rsp 8)
                    (Call 'gcd)
                    (Add 'rsp 8)
                    (Ret))))]


@defproc[(asm-interp/io [is (listof instruction?)] [in string?]) (cons integer? string?)]{

 Like @racket[asm-interp], but uses @racket[in] for input and produce the result along
 with any output as a string.

}
