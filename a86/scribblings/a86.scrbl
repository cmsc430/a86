#lang scribble/manual
@(require scribble/bnf)

@(require (for-label (except-in racket compile)
		     a86/ast
		     a86/registers
		     a86/printer
		     a86/interp))

@(require scribble/examples
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

@;defmodule[a86 #:no-declare]

The @racketmodname[a86] library provides functions for composing,
printing, and running x86-64 assembly programs in Racket.

@table-of-contents[]

@section{Overview}

@defmodule[a86 #:no-declare]

This library provides functions for composing, printing, and running
x86-64 assembly programs in Racket:

@#reader scribble/comment-reader
(ex
 ; Natural -> Asm
 ; Produce representation of assembly program to compute n! recursively
 (define (fact-program n)
   (list (Global 'run)
	 (Label 'run)
	 (Mov 'rax n)
	 (Label 'fact)
	 (Cmp 'rax 0)
	 (Je 'done)
	 (Push 'rax)
	 (Sub 'rax 1)
	 (Call 'fact)
	 (Pop 'r9)
	 (Mul 'r9)
	 (Ret)
	 (Label 'done)
	 (Mov 'rax 1)
	 (Ret)))

 ; compute 5!
 (asm-interp (fact-program 5))

 ; render 5! program in asm syntax
 (asm-display (fact-program 5))
)

@secref["Programs"] consist of a list of
@secref["Instructions"] and @secref["Psuedo-Instructions"].
Instructions can take as arguments @secref["Labels"],
@secref["Immediates"], @secref["Registers"] and
@secref["Memory_Expressions"].

Executing instructions can read and modify @secref["Registers"]
and @secref["Memory"], including the @secref["Stack"].

The @racketmodname[a86] module provides all of the bindings
from @racketmodname[a86/ast], @racketmodname[a86/registers],
@racketmodname[a86/printer], and @racketmodname[a86/interp],
described below.

@section{Programs}

@defmodule[a86/ast]

An a86 program is a list of instructions. To be
interpretable with @racket[asm-interp], the program must
be well-formed, which means:

 @itemlist[

 @item{Programs have at least one label which is declared @racket[Global]; the first such label is used as the entry point.}
 @item{All label definitions are unique.}
 @item{All used labels are declared.}

 ]


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
 form a program.

 This function is useful to do some early error checking
 over whole programs and can help avoid confusing assembler
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


@subsection{Psuedo-Instructions}

Psuedo-instructions are elements of @secref{Programs} that
make declarations and directives to the assembler, but don't
correspond to actual execuable @secref{Instructions}.

@defstruct*[Text ()]{

 Declares the start of a text section, which includes instructions to
 be executed.

}

@defstruct*[Data ()]{

 Declares the start of a data section, which includes data and constants.

}

@defstruct*[Label ([x label?])]{

 Defines the given label, which is used as a symbolic name for @emph{this}
 location in the program. Each defined label in a
 program must be unique.  Label names must follow the restrictions
 on valid label names (see @racket[label?] for details).

 @ex[
 (Label 'fred)
 (eval:error (Label "fred"))
 (eval:error (Label 'fred-wilma))
 ]

}

@defstruct*[Extern ([x label?])]{

 Declares an external label.  External labels may be used, but not defined
 within the program.

}

@defstruct*[Global ([x label?])]{

 Declares a label as global, i.e. linkable with other object files.

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


@subsection{Instructions}

Instructions are represented as structures and can take as
arguments @secref["Immediates"], @secref["Registers"],
@secref["Labels"], @secref["Memory_Expressions"], or
@secref["Assembly_Expressions"].

For example, @racket[(Mov 'rax 42)] is a "move" instruction
that when executed will move the immediate value @racket[42]
into the @racket[rax] register.

See @secref["Instruction_Set"] for a complete listing of the
instruction set and instruction constructor signatures.

@subsection{Immediates}

Immediates are represented as exact integers of a certain
bit width, which will depend upon the particular
instruction and other arguments.

For example, @racket[Mov] can take a 64-bit immediate source
argument @emph{if} the destination register is a 64-bit
register. If the destination is a 32-bit register, the
immediate must fit in 32-bits, etc. @racket[Cmp] can take at
most a 32-bit immediate argument. Instruction constructors
check the size constraints of immediate arguments and signal
an error when out of range.

@ex[(Mov rax (sub1 (expt 2 64)))
    (eval:error (Mov eax (sub1 (expt 2 64))))
    (eval:error (Cmp rax (sub1 (expt 2 64))))]

Note that x86 doesn't have a notion of signed or unsigned
integers. Some instructions compute either signed or
unsigned operations, but the values in registers are simply
bits. For this reason, a 64-bit immediate can be any exact
integer in the range @racket[(- (expt 2 63))] and
@racket[(sub1 (expt 2 64))], but keep in mind that, for
example @racket[(- (expt 2 63))] and @racket[(expt 2 23)]
are represented by @emph{the same} bits. Also note that
@racket[asm-interp] interprets the result of an assembly
program as a @emph{signed} integer. If you want to interpret
the result as an unsigned integer, you will need add code to
do so.

Here is an example where you can see different immediate arguments
resulting in the same result from @racket[asm-interp], and that the
result is signed:
@ex[(asm-interp (Mov rax -1)
		(Ret))
    (asm-interp (Mov rax (sub1 (expt 2 64)))
		(Ret))
    (asm-interp (Mov rax (- (expt 2 63)))
		(Ret))
    (asm-interp (Mov rax (expt 2 63))
		(Ret))]

@defproc*[([(64-bit-integer? [x any/c]) boolean?]
	   [(32-bit-integer? [x any/c]) boolean?]
	   [(16-bit-integer? [x any/c]) boolean?]
	   [(8-bit-integer? [x any/c]) boolean?])]{
 Predicates for determining if a value is an integer that fits in some number of bits.

 @ex[
 (64-bit-integer? 0)
 (64-bit-integer? (sub1 (expt 2 64)))
 (64-bit-integer? (expt 2 64))
 (64-bit-integer? (- (expt 2 63)))
 (64-bit-integer? (sub1 (- (expt 2 63))))
 (32-bit-integer? 0)
 (32-bit-integer? (sub1 (expt 2 32)))
 (32-bit-integer? (expt 2 32))
 (32-bit-integer? (- (expt 2 32)))
 (32-bit-integer? (sub1 (- (expt 2 32))))]
}

@subsection{Registers}

@defmodule[a86/registers]

Registers are represented as symbols, but this module also
provides bindings corresponding to each register name,
e.g. @racket[rax] is bound to @racket['rax].

There are 16 64-bit registers.

@defthing*[([rax register?]
	    [rbx register?]
	    [rcx register?]
	    [rdx register?]
	    [rbp register?]
	    [rsp register?]
	    [rsi register?]
	    [rdi register?]
	    [r8  register?]
	    [r9  register?]
	    [r10 register?]
	    [r11 register?]
	    [r12 register?]
	    [r13 register?]
	    [r14 register?]
	    [r15 register?])]{
Names for corresponding 64-bit registers.
@ex[rax rbx rsp]

The registers @racket[rbx], @racket[rsp], @racket[rbp], and
@racket[r12] through @racket[r15] are ``callee-saved'' registers,
meaning they are preserved across function calls (and must be saved
and restored by any callee code).

Each register plays the same role as in x86, so for example
@racket[rsp] holds the current location of the stack.
}

There are 16 @emph{aliases} for the lower 32-bits of
the above registers.  These are not separate registers,
but instead provide access to the least signficant 32-bits of the
64-bits register.

@defthing*[([eax  register?]
	    [ebx  register?]
	    [ecx  register?]
	    [edx  register?]
	    [ebp  register?]
	    [esp  register?]
	    [esi  register?]
	    [edi  register?]
	    [r8d  register?]
	    [r9d  register?]
	    [r10d register?]
	    [r11d register?]
	    [r12d register?]
	    [r13d register?]
	    [r14d register?]
	    [r15d register?])]{
Names for corresponding 32-bit alias registers.
@ex[eax ebx esp]
}

There are 16 @emph{aliases} for the lower 16-bits of
the above registers (and thus the lower 16-bits
of the 64-bit registers).  These are not separate registers,
but instead provide access to the least signficant 16-bits of the
64-bits register.

@defthing*[([ax  register?]
	    [bx  register?]
	    [cx  register?]
	    [dx  register?]
	    [bp  register?]
	    [sp  register?]
	    [si  register?]
	    [di  register?]
	    [r8w  register?]
	    [r9w  register?]
	    [r10w register?]
	    [r11w register?]
	    [r12w register?]
	    [r13w register?]
	    [r14w register?]
	    [r15w register?])]{
Names for corresponding 16-bit alias registers.
@ex[ax bx sp]
}

There are 16 @emph{aliases} for the lower 8-bits of
the above registers (and thus the lower 8-bits
of the 64-bit registers).  These are not separate registers,
but instead provide access to the least signficant 8-bits of the
64-bits register.

@defthing*[([al  register?]
	    [bl  register?]
	    [cl  register?]
	    [dl  register?]
	    [bpl  register?]
	    [spl  register?]
	    [sil  register?]
	    [dil  register?]
	    [r8b  register?]
	    [r9b  register?]
	    [r10b register?]
	    [r11b register?]
	    [r12b register?]
	    [r13b register?]
	    [r14b register?]
	    [r15b register?])]{
Names for corresponding 8-bit alias registers.
@ex[al bl spl]
}

Finally, there are 4 @emph{aliases} for next higher 8-bits
of the above registers (and thus the lower 9th-16th bits
of @emph{some} of the 64-bit registers}.
Only @racket[rax], @racket[rbx], @racket[rcx], and @racket[rdx]
have such aliases.

@defthing*[([ah  register?]
	    [bh  register?]
	    [ch  register?]
	    [dh  register?])]{
Names for the corresponding 8-bit alias registers.
@ex[ah bh]}


@defproc[(register? [x any/c]) boolean?]{
 A predicate for registers.
 @ex[(register? 'rax) (register? 'al) (register? 'bmx)]
}

@defproc[(register-size [x register?]) (or/c 8 16 32 64)]{
Returns the size of a given register.
@ex[(register-size rax)
    (register-size eax)
    (register-size ax)
    (register-size al)
    (register-size ah)]
}

@defproc*[([(reg-64-bit [r register?]) register?]
	   [(reg-32-bit [r register?]) register?]
	   [(reg-16-bit [r register?]) register?]
	   [(reg-8-bit-low [r register?]) register?]
	   [(reg-8-bit-high [r register?]) register?])]{
Functions for computing alias of a given register.  These
functions can be given any register (including alias registers)
and compute the corresponding register name of the appropriate
bit-width.
@ex[(reg-8-bit-low rax)
    (reg-8-bit-high rax)
    (reg-64-bit eax)
    (reg-32-bit eax)]

In the case of @racket[reg-8-bit-high], an error is signalled
if the given register has no corresponding alias.
@ex[(reg-8-bit-high ebx)
    (eval:error (reg-8-bit-high r8))]
}

@subsection{Labels}

Labels are represented as symbols (or @racket[$] structures)
that must conform to the naming restriction imposed by the assembler,
so not all symbols are valid label names.

@defproc[(label? [x any/c]) boolean?]{
 A predicate for label @emph{names}, i.e. symbols which are not register names.

 Labels must also follow the restrictions on label names: "Valid
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

@defstruct*[$ ([l symbol?])]{
Structure for representing labels.  Useful when you need to refer to a label
that has a name conflicting with register name or other reserved keyword.

@ex[(Label ($ 'rax))]

}

@subsection{Memory Expressions}

Memory expressions are represented with @racket[Offset]
structures. A memory expression signals that a quantity
should be interpreted as a location in memory, rather than
the bits itself. For example, the @racket[rsp] holds a
pointer the stack memory; @racket[(Mov rax rsp)] will move
the pointer held in @racket[rsp] into @racket[rax], while
@racket[(Mov rax (Offset rsp))] will read 64-bits of memory
at the location pointed at by the pointer in @racket[rsp]
into @racket[rax]. On the other hand, @racket[(Mov rsp rax)]
will move the value in @racket[rax] into the @racket[rsp]
register (overwriting the stack pointer), while
@racket[(Mov (Offset rsp) rax)] will write the value in
@racket[rax] into the memory pointed at by the @racket[rsp]
register.

Memory expression can take as arguments either registers or
@secref["Assembly_Expressions"], which are commonly used to
indicate offsets from a given memory location, e.g.
@racket[(Mov rax (Offset (|@| (+ rsp 8))))] reads the 64-bits of
memory at the location held in @racket[rsp] + @racket[8],
i.e. @racket[8] bytes past wherever @racket[rsp] points.


@defstruct*[Offset ([e (or/c exp? register?)])]{

 Creates an memory offset from a register. Offsets are used
 as arguments to instructions to indicate memory locations.

 @ex[
 (Offset 'rax)
 ]
}

@defproc[(offset? [x any/c]) boolean?]{
 A predicate for offsets.
}



@subsection{Assembly Expressions}

Assembly expressions are represented by s-expressions
conforming to the following grammar:

@BNF[(list @nonterm{expr}
	   @nonterm{register}
	   @nonterm{immediate}
	   @nonterm{label}
	   @racket['$]
	   @racket['$$]
	   @racket[#:escape ESC (list (ESC @nonterm{unop}) (ESC @nonterm{expr}))]
	   @racket[#:escape ESC (list (ESC @nonterm{binop}) (ESC @nonterm{expr}) (ESC @nonterm{expr}))]
	   @racket[#:escape ESC (list '? (ESC @nonterm{expr}) (ESC @nonterm{expr}) (ESC @nonterm{expr}))])
     (list @nonterm{unop}
	   @racket['+]
	   @racket['-]
	   @racket['~]
	   @racket['!]
	   @racket['SEG])
     (list @nonterm{binop}
	   @racket['<<<]
	   @racket['<<]
	   @racket['<]
	   @racket['<=]
	   @racket['<=>]
	   @racket['>=]
	   @racket['>]
	   @racket['>>]
	   @racket['>>>]
	   @racket['=]
	   @racket['==]
	   @racket['!=]
	   @racket['||]
	   @racket['\|]
	   @racket['&]
	   @racket['&&]
	   @racket['^^]
	   @racket['^]
	   @racket['+]
	   @racket['-]
	   @racket['*]
	   @racket['/]
	   @racket['//]
	   @racket['%]
	   @racket['%%])]

For the meaning of assembly instructions, refer to the
@link["https://www.nasm.us/doc/nasmdoc3.html#section-3.5"]{NASM
 docs}.

@defproc[(exp? [x any/c]) boolean?]{
 A predicate for assembly expressions.

 @ex[(exp? 0)
     (exp? '(+ rax 8))
     (exp? '(? lab1 0 1))
     ]
}

@defform[(|@| e)]{

 A convenience form for constructing assembly expressions.
 This form implicitly quotes @racket[e] and implicitly
 unquotes when encountering bound identifiers or forms that
 are not part of the assembly expression grammar. So for
 example @racket[(|@| (+ 1 2))] is just @racket['(+ 1 2)],
 but @racket[(|@| (+ 1 (add1 2)))] is @racket['(+ 1 3)]. Note
 that identifiers that are not bound are assumed to refer to
 labels, so they are quoted, but identifiers that are bound
 are replaced with their value.

 This form is useful for referencing bound variables or
 Racket functions within assembly expression. If the Racket
 identifier you want to reference conflicts with an assembly
 expression keyword, e.g. @racket[+], you can use
 @racket[begin] to escape into Racket expression mode, e.g.
 @racket[(|@| (+ 1 (begin (+ 2 3))))] is @racket['(+ 1 5)].

 If any unquoted expression evaluates to something that is
 not an assembly expression, an error is signalled.

 @ex[(|@| (+ 1 2))
     (|@| (+ x 1))
     (let ((x 100)) (|@| (+ x 1)))
     (let ((+ 100)) (|@| (+ + +)))
     (eval:error (|@| (+ + +)))]
 }



@subsection{Instruction Set}

This section describes the instruction set of a86.

@defproc[(instruction? [x any/c]) boolean?]{
 A predicate for instructions.
}


@defproc[(symbol->label [s symbol?]) label?]{

  Returns a modified form of a symbol that follows assembler label conventions.

  @ex[
  (let ([l (symbol->label 'my-great-label)])
    (seq (Label l)
	 (Jmp l)))
  ]
}


@defstruct*[Call  ([x (or/c label? register?)])]{

 A call instruction.

 @ex[
 (asm-interp
   (Global 'entry)
   (Label 'entry)
   (Call 'f)
   (Add 'rax 1)
   (Ret)
   (Label 'f)
   (Mov 'rax 41)
   (Ret))
 ]
}

@defstruct*[Ret ()]{

 A return instruction.

 @ex[
 (asm-interp
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 42)
   (Ret))
 ]

}

@defstruct*[Mov ([dst (or/c register? offset?)] [src (or/c register? offset? 64-bit-integer?)])]{

 A move instruction. Moves @racket[src] to @racket[dst].

 Either @racket[dst] or @racket[src] may be offsets, but not both.

 @ex[
 (asm-interp
   (Global 'entry)
   (Label 'entry)
   (Mov 'rbx 42)
   (Mov 'rax 'rbx)
   (Ret))
 (eval:error (Mov (Offset 'rax 0) (Offset 'rbx 0)))
 ]

}

@defstruct*[Add ([dst register?] [src (or/c register? offset? 32-bit-integer?)])]{

 An addition instruction. Adds @racket[src] to @racket[dst]
 and writes the result to @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 32)
   (Add 'rax 10)
   (Ret))
 ]
}

@defstruct*[Sub ([dst register?] [src (or/c register? offset? 32-bit-integer?)])]{

 A subtraction instruction. Subtracts @racket[src] from
 @racket[dst] and writes the result to @racket[dst].
 Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 32)
   (Sub 'rax 10)
   (Ret))
 ]
}

@defstruct*[Mul ([src (or/c register? offset? 32-bit-integer?)])]{

 A multiplication instruction. Multiplies @racket[src] by @racket['rax]
 and writes the result to @racket['rax] and @racket['rdx]. Updates flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp
   (Global 'entry)
   (Label 'entry)
   (Mov 'rax 32)
   (Add 'rax 10)
   (Ret))
 ]
}

@defstruct*[Cmp ([a1 (or/c register? offset?)] [a2 (or/c register? offset? 32-bit-integer?)])]{
 Compare @racket[a1] to @racket[a2] by subtracting @racket[a2] from @racket[a1]
 and updating the comparison flags. Does not store the result of subtraction.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Cmp 'rax 2)
	     (Jg 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jmp ([x (or/c label? register?)])]{
 Jump to label @racket[x].

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Jmp 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))

 (asm-interp (Mov 'rax 42)
	     (Pop 'rbx)
	     (Jmp 'rbx))
 ]

}

@defstruct*[Jz ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the zero flag is set.

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Cmp 'rax 2)
	     (Jz 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jnz ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the zero flag is @emph{not} set.

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Cmp 'rax 2)
	     (Jnz 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Je ([x (or/c label? register?)])]{
 An alias for @racket[Jz].
}

@defstruct*[Jne ([x (or/c label? register?)])]{
 An alias for @racket[Jnz].
}

@defstruct*[Jl ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``less than'' (see @secref{Flags}).

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Cmp 'rax 2)
	     (Jl 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jle ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``less than or equal'' (see @secref{Flags}).

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Cmp 'rax 42)
	     (Jle 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jg ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``greater than'' (see @secref{Flags}).

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Cmp 'rax 2)
	     (Jg 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jge ([x (or/c label? register?)])]{
 Jump to label @racket[x] if the conditional flags are set to ``greater than or equal'' (see @secref{Flags}).

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Cmp 'rax 42)
	     (Jg 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jo ([x (or/c label? register?)])]{
 Jump to @racket[x] if the overflow flag is set.

 @ex[
 (asm-interp (Mov 'rax (sub1 (expt 2 63)))
	     (Add 'rax 1)
	     (Jo 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jno ([x (or/c label? register?)])]{
 Jump to @racket[x] if the overflow flag is @emph{not} set.

 @ex[
 (asm-interp (Mov 'rax (sub1 (expt 2 63)))
	     (Add 'rax 1)
	     (Jno 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jc ([x (or/c label? register?)])]{
 Jump to @racket[x] if the carry flag is set.

 @ex[
 (asm-interp (Mov 'rax -1)
	     (Add 'rax 1)
	     (Jc 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Jnc ([x (or/c label? register?)])]{
 Jump to @racket[x] if the carry flag is @emph{not} set.

 @ex[
 (asm-interp (Mov 'rax -1)
	     (Add 'rax 1)
	     (Jnc 'l1)
	     (Mov 'rax 0)
	     (Label 'l1)
	     (Ret))
 ]
}

@defstruct*[Cmovz ([dst register?] [src (or/c register? offset?)])]{
 Read from @racket[src], move to @racket[dst] if the zero flag is set.

 @ex[
 (asm-interp (Mov 'rax 0)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovz 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax 2)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovz 'rax 'r9)
	     (Ret))
 ]

 Note that the semantics for conditional moves is not what many people expect.
 The @racket[src] is @emph{always} read, regardless of the condition's evaluation.
 This means that if your source is illegal (such as an offset beyond the bounds
 of memory allocated to the current process), a segmentation fault will arise
 even if the condition ``should have'' prevented the error.

 @#reader scribble/comment-reader
 (ex (eval:error (asm-interp (Mov 'r9 0)
			     (Cmp 'r9 1)
			     (Mov 'rax 0)
			     ; doesn't move, but does read memory address 0
			     (Cmovz 'rax (Offset 'r9))
			     (Ret))))

}


@defstruct*[Cmove ([dst register?] [src (or/c register? offset?)])]{
 An alias for @racket[Cmovz]. See notes on @racket[Cmovz].
}

@defstruct*[Cmovnz ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the zero flag is @emph{not} set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax 0)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovnz 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax 2)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovnz 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovne ([dst register?] [src (or/c register? offset?)])]{
 An alias for @racket[Cmovnz]. See notes on @racket[Cmovz].

}

@defstruct*[Cmovl ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``less than'' (see @secref{Flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax 0)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovl 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax -1)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovl 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovle ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``less than or equal'' (see @secref{Flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax 0)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovle 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax 2)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovle 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovg ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``greather than'' (see @secref{Flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax 0)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovg 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax 2)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovg 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovge ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the conditional flags are set to ``greater than or equal'' (see @secref{Flags}).
 See also the notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax -1)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovge 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax 2)
	     (Cmp 'rax 0)
	     (Mov 'r9 1)
	     (Cmovge 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovo ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the overflow flag is set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax (- (expt 2 63) 1))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovo 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax (- (expt 2 63) 2))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovo 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovno ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the overflow flag is @emph{not} set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax (- (expt 2 63) 1))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovno 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax (- (expt 2 63) 2))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovno 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovc ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the carry flag is set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax (- (expt 2 64) 1))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovc 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax (- (expt 2 64) 2))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovc 'rax 'r9)
	     (Ret))
 ]
}

@defstruct*[Cmovnc ([dst register?] [src (or/c register? offset?)])]{
 Move from @racket[src] to @racket[dst] if the carry flag is @emph{not} set.
 See notes on @racket[Cmovz].

 @ex[
 (asm-interp (Mov 'rax (- (expt 2 64) 1))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovnc 'rax 'r9)
	     (Ret))

 (asm-interp (Mov 'rax (- (expt 2 64) 2))
	     (Add 'rax 1)
	     (Mov 'r9 1)
	     (Cmovnc 'rax 'r9)
	     (Ret))
 ]
}


@defstruct*[And ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{

 Compute logical ``and'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp (Mov 'rax #b1011) ; #b1011 = 11
	     (And 'rax #b1110) ; #b1110 = 14
	     (Ret))            ; #b1010 = 10
 )
}

@defstruct*[Or ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{
 Compute logical ``or'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp (Mov 'rax #b1011) ; #b1011 = 11
	     (Or 'rax #b1110)  ; #b1110 = 14
	     (Ret))            ; #b1111 = 15
 )
}

@defstruct*[Xor ([dst (or/c register? offset?)] [src (or/c register? offset? 32-bit-integer?)])]{
 Compute logical ``exclusive or'' of @racket[dst] and @racket[src] and put result in @racket[dst]. Updates the conditional flags.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 In the case of a 32-bit immediate, it is sign-extended to 64-bits.

 @#reader scribble/comment-reader
 (ex
 (asm-interp (Mov 'rax #b1011) ; #b1011 = 11
	     (Xor 'rax #b1110) ; #b1110 = 14
	     (Ret))            ; #b0101 = 5
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
 (asm-interp (Mov 'rax 42)
	     (Push 'rax)
	     (Mov 'rax 0)
	     (Pop 'rax)
	     (Ret))
 ]
}

@defstruct*[Pop ([a1 register?])]{
 Loads the value from the top of the stack to the destination operand and then increments the stack pointer.

 @ex[
 (asm-interp (Mov 'rax 42)
	     (Push 'rax)
	     (Mov 'rax 0)
	     (Pop 'rax)
	     (Ret))
 ]
}

@defstruct*[Not ([a1 register?])]{
Perform bitwise not operation (each 1 is set to 0, and each 0 is set to 1) on the destination operand.

 @ex[
 (asm-interp (Mov 'rax 0)
	     (Not 'rax)
	     (Ret))
 ]
}

@defstruct*[Lea ([dst (or/c register? offset?)] [x label?])]{
 Loads the address of the given label into @racket[dst].

 @ex[
 (asm-interp (Lea 'rbx 'done)
	     (Mov 'rax 42)
	     (Jmp 'rbx)
	     (Mov 'rax 0)
	     (Label 'done)
	     (Ret))
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


@section{Execution Model}

The execution model of @racketmodname[a86] programs is the same as
that of x86, but this section gives a brief overview of the most
important aspects.

Execution proceeds instruction by instructions.  Instructions may read
or modify the state of @secref{Registers}, @secref{Flags}, and
@secref{Memory} and, except for jumping instructions, execution
proceeds to the next instruction in memory.

@subsection{Flags}

The processor makes use of @emph{flags} to handle comparisons. For our
purposes, there are four flags to be aware of: zero (ZF), sign (SF),
carry (CF), and overflow (OF).

These flags are set by each of the arithmetic operations, which
are appropriately annotated in the @secref{Instruction_Set}.
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


@subsection{Stack}

The a86 execution model includes access to memory that can
be used as a stack data structure. There are operations that
manipulate the stack, such as @racket[Push], @racket[Pop],
@racket[Call], and @racket[Ret], and the stack register
pointer @racket['rsp] is dedicated to the stack. Stack
memory is allocated in ``low'' address space and grows
downward. So pushing an element on to the stack @emph{
 decrements} @racket['rsp].

The stack is useful as a way to save away values that may be
needed later. For example, let's say you have two
(assembly-level) functions and you want to produce the sum
of their results. By convention, functions return their
result in @racket['rax], so doing something like this
won't work:

@racketblock[
(seq (Call 'f)
     (Call 'g)
     (Add 'rax ...))
]

The problem is the return value of @racket['f] gets
clobbered by @racket['g]. You might be tempted to fix the
problem by moving the result to another register:

@racketblock[
(seq (Call 'f)
     (Mov 'rbx 'rax)
     (Call 'g)
     (Add 'rax 'rbx))
]

This works only so long as @racket['g] doesn't clobber
@racket['rbx]. In general, it might not be possible to avoid
that situation.  So the solution is to use the stack to save
the return value of @racket['f] while the call to @racket['g]
proceeds:

@racketblock[
(seq (Call 'f)
     (Push 'rax)
     (Call 'g)
     (Pop 'rbx)
     (Add 'rax 'rbx))
]

This code pushes the value in @racket['rax] on to the stack
and then pops it off and into @racket['rbx] after
@racket['g] returns. Everything works out so long as
@racket['g] maintains a stack-discipline, i.e. the stack
should be in the same state when @racket['g] returns as when
it was called.

We can make a complete example to confirm that this works as
expected. First let's set up a little function for letting
us try out examples:

@#reader scribble/comment-reader
(ex
(define (eg asm)
  (asm-interp
   (prog
    (Global 'entry)
    (Label 'entry)
    asm  ; the example code we want to try out
    (Ret)

    (Label 'f)      ; calling 'f returns 36
    (Mov 'rax 36)
    (Ret)

    (Label 'g)      ; calling 'g returns 6, but
    (Mov 'rbx 4)    ; it clobbers 'rbx just for the lulz
    (Mov 'rax 6)
    (Ret))))
)

Now let's try it, using the stack to confirm it does the
right thing:

@#reader scribble/comment-reader
(ex
(eg (seq (Call 'f)
	 (Push 'rax)
	 (Call 'g)
	 (Pop 'rbx)
	 (Add 'rax 'rbx)))
)

Compare that with the first version that used a register to
save the result of @racket['f]:

@#reader scribble/comment-reader
(ex
(eg (seq (Call 'f)
	 (Mov 'rbx 'rax)
	 (Call 'g)
	 (Add 'rax 'rbx)))
)


The @racket[Push] and @racket[Pop] instructions offer a
useful illusion, but of course, there's not really any data
structure abstraction here; there's just raw memory and
registers. But so long as code abides by conventions, the
illusion turns out to be the true state of affairs.

What's really going on under the hood of @racket[Push] and
@racket[Pop] is that the @racket['rsp] register is
decremented and the value is written to the memory location
pointed to by the value of @racket['rsp].

The following code is @emph{mostly} equivalent to what we wrote
above (and we will discuss the difference in the next section):

@#reader scribble/comment-reader
(ex
(eg (seq (Call 'f)
	 (Sub 'rsp 8)                ; "allocate" a word on the stack
	 (Mov (Offset 'rsp 0) 'rax)  ; write 'rax to top frame
	 (Call 'g)
	 (Mov 'rbx (Offset 'rsp 0))  ; load top frame into 'rbx
	 (Add 'rsp 8)                ; "deallocate" word on the stack
	 (Add 'rax 'rbx)))
)

As you can see from this code, it would be easy to violate
the usual invariants of stack data structure to, for
example, access elements beyond the top of the stack. The
value of @racket[Push] and @racket[Pop] is they make clear
that you are using things in a stack-like way and they keep
you from screwing up the accesses, offsets, and adjustments
to @racket['rsp].

Just as @racket[Push] and @racket[Pop] are useful illusions,
so too are @racket[Call] and @racket[Ret]. They give the
impression that there is a notion of a procedure and
procedure call mechanism in assembly, but actually there's
no such thing.

Think for a moment about what it means to ``call'' @racket['f]
in the examples above. When executing @racket[(Call 'f)],
control jumps to the instruction following
@racket[(Label 'f)]. When we then get to @racket[(Ret)],
somehow the CPU knows to jump @emph{back} to the instruction
following the @racket[(Call 'f)] that we started with.

What's really going on is that @racket[(Call 'f)] is pushing
the address of subsequent instruction on to the stack and
then jumping to the label @racket['f]. This works in concert
with @racket[Ret], which pops the return address off the
stack and jumping to it.

Just as we could write equivalent code without @racket[Push]
and @racket[Pop], we can write the same code without
@racket[Call] and @racket[Ret].

We do need one new trick, which is the @racket[Lea]
instruction, which loads an effective address. You can think
of it like @racket[Mov] except that it loads the address of
something rather than what is pointed to by an address.  For our
purposes, it is useful for loading the address of a label:

@racketblock[
 (Lea 'rax 'f)
 ]

This instruction puts @emph{the address} of label
@racket['f] into @racket[rax]. You can think of this as
loading a @emph{function pointer} into @racket['rax]. With
this new instruction, we can illuminate what is really going
on with @racket[Call] and @racket[Ret]:

@#reader scribble/comment-reader
(ex
(eg (seq (Lea 'rax 'fret)  ; load address of 'fret label into 'rax
	 (Push 'rax)       ; push the return pointer on to stack
	 (Jmp 'f)          ; jump to 'f
	 (Label 'fret)     ; <-- return point for "call" to 'f
	 (Push 'rax)       ; save result (like before)
	 (Lea 'rax 'gret)  ; load address of 'gret label into 'rax
	 (Push 'rax)       ; push the return pointer on to stack
	 (Jmp 'g)          ; jump to 'g
	 (Label 'gret)     ; <-- return point for "call" to 'g
	 (Pop 'rbx)        ; pop saved result from calling 'f
	 (Add 'rax 'rbx)))
)

@;{
Or to avoid the use of register to temporarily hold the
address to jump to, we could've also written it as:

@#reader scribble/comment-reader
(ex
(eg (seq (Sub 'rsp 8)      ; allocate a frame on the stack
			   ; load address of 'fret label into top of stack
	 (Lea (Offset 'rsp 0) 'fret)
	 (Jmp 'f)          ; jump to 'f
	 (Label 'fret)     ; <-- return point for "call" to 'f
	 (Push 'rax)       ; save result (like before)
	 (Sub 'rsp 8)      ; allocate a frame on the stack
			   ; load address of 'gret label into top of stack
	 (Lea (Offset 'rsp 0) 'gret)
	 (Jmp 'g)          ; jump to 'g
	 (Label 'gret)     ; <-- return point for "call" to 'g
	 (Pop 'rbx)        ; pop saved result from calling 'f
	 (Add 'rax 'rbx)))
)
}

The above shows how to encode @racket[Call] as @racket[Lea],
@racket[Push], and @racket[Jmp].  The encoding of @racket[Ret] is just:

@racketblock[
 (seq (Pop 'rbx)    ; pop the return pointer
      (Jmp 'rbx))   ; jump to it
 ]


While the @racket[Push] and @racket[Pop] operations are
essentially equivalent to manually adjusting the stack
pointer and target register. The one difference is that these
special stack-manipulation operations do not set any flags
like @racket[Add] and @racket[Sub] do. So while you can
often choose to manually implement stack manipulation, you'll
need to use these instructions specifically if you want to
preserve the condition flags after adjusting the stack.


@subsection{Memory}

The stack is really just a pointer some location in memory, but it is
possible to alloacte, read, and modify memory elsewhere too.  The
stack memory is allocated by the operating system and the location of
this memory is initially placed in the @racket[rsp] register.

It is possible to statically allocate memory within the program itself
using the @racket[Data] section and psuedo-instructions such as
@racket[Dq], etc.

For example, this program statically allocates a quad-word (64-bits),
initialized to 0.  The program then modifies the memory by writing 42
and then returning the value obtained by dereferencing that memory,
i.e. 42:

@ex[
(asm-interp (Mov r8 42)
	    (Mov (Offset 'm) r8)
	    (Mov rax (Offset 'm))
	    (Ret)
	    (Data)
	    (Label 'm)
	    (Dq 0))]

It is also possible to dynamically allocate memory.  This can be done by a
wrapper, written e.g. in C, that allocates memory and passes in a pointer to
that memory as an argument to the assembly code.  It's also possible to
call standard C library function like @tt{malloc} to allocate memory within
an assembly program.

This program is analogous to the one above, but instead of statically
allocating a quad-word of memory, it makes a call to @tt{malloc} with
an argument of 8 in order to allocate 8 bytes of memory.  A pointer to
the newly allocated memory is returned in @racket[rax], which is then
written to with the value 42, before being dereferenced and returned:

@ex[
(asm-interp (Mov rdi 8)
	    (Extern 'malloc)
	    (Call 'malloc)
	    (Mov r8 42)
	    (Mov (Offset rax) r8)
	    (Mov rax (Offset rax))
	    (Ret))]


@section{Printing}

@defmodule[a86/printer]

@defproc[(asm-display [is (listof instruction?)]) void?]{

 Prints an a86 program to the current output port in Intel syntax.

 @ex[
 (asm-display (prog (Global 'entry)
		    (Label 'entry)
		    (Mov 'rax 42)
		    (Ret)))
 ]

}

@defproc[(asm-string [is (listof instruction?)]) string?]{

 Converts an a86 program to a string in Intel syntax.

 @ex[
 (asm-string (prog (Global 'entry)
		   (Label 'entry)
		   (Mov 'rax 42)
		   (Ret)))
 ]

}

@section{Interpreting}

@defmodule[a86/interp]

It is possible to run a86 @secref["Programs"] from within
Racket using @racket[asm-interp].

Using @racket[asm-interp] comes with significant overhead,
so it's unlikely you'll want to implement Racket
functionality in assembly code via @racket[asm-interp].
Rather this is a utility for interactively exploring the
behavior of assembly code and writing tests for functions
that generate assembly code.

If you have code written in a86 that you would like to
execute directly, you should instead use the
@seclink["Printing"]{printing} facilities to save the
program to a file and then use an external assembler (e.g.
@tt{clang}) and linker to produce either object files or
executables. It's possible to use
@other-doc['(lib "scribblings/foreign/foreign.scrbl")] to
interact with those files from within Racket.


The simplest form of interpreting an a86 program is to use
@racket[asm-interp].

@defproc[(asm-interp [is (or/c instruction? (listof instruction?))] ...) integer?]{

 Assemble, link, and execute an a86 program.

 @ex[
 (asm-interp (prog (Global 'entry)
		   (Label 'entry)
		   (Mov 'rax 42)
		   (Ret)))
 ]

 Programs do not have to start with a label named
 @racket['entry]. The interpreter will jump to whatever the
 first label in the program is (which must be declared
 @racket[Global]):

@ex[
 (asm-interp (prog (Global 'f)
		   (Label 'f)
		   (Mov 'rax 42)
		   (Ret)))
 ]

 As a convenience, @racket[asm-interp] accepts any number of
 arguments that are either instructions or lists of
 instructions and it will splice them together to form a
 program, like @racket[seq]:

 @ex[(asm-interp (Global 'f)
		 (Label 'f)
		 (Mov 'rax 42)
		 (Ret))]

 As another convenience, if the first defined label of the
 instructions given to @racket[asm-interp] is not declared
 @racket[Global] or there is no first defined label,
 @racket[asm-interp] will generate a globally defined label
 at the beginning of the instructions and start executing
 there:

 @ex[(asm-interp (Mov 'rax 42)
		 (Ret))]

 With the exception of these conveniences, the argument of
 @racket[asm-interp] should form a complete, well-formed a86
 program in the sense of @racket[prog].

 While this library tries to make assembly syntax errors
 impossible, it is possible---quite easy, in fact---to write
 well-formed, but erroneous assembly programs. For example,
 this program tries to jump to null, which causes a
 segmentation fault:

 @ex[
 (eval:error (asm-interp (Mov rax 0)
			 (Jmp rax)))
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
  (asm-interp (Extern 'gcd)
	      (Mov 'rdi 11571)
	      (Mov 'rsi 1767)
	      (Sub 'rsp 8)
	      (Call 'gcd)
	      (Add 'rsp 8)
	      (Ret)))]

This will be particularly relevant for writing a compiler
where emitted code will make use of functionality defined in
a runtime system.

Note that if you forget to set @racket[current-objs], you will get a
linking error saying a symbol is undefined:

@ex[
(eval:error
 (asm-interp (Extern 'gcd)
	     (Mov 'rdi 11571)
	     (Mov 'rsi 1767)
	     (Sub 'rsp 8)
	     (Call 'gcd)
	     (Add 'rsp 8)
	     (Ret)))]


@defproc[(asm-interp/io [is (listof instruction?)] [in string?]) (cons integer? string?)]{

 Like @racket[asm-interp], but uses @racket[in] for input and produce the result along
 with any output as a string.

}
