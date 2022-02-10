#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef NANBOXING

#define HEAP 100*1024*1024

#else

#include <gc/gc.h>

#endif

#ifndef TRUE
#define TRUE (0==0)
#endif
#ifndef FALSE
#define FALSE (0!=0)
#endif

// enums, structs, typedefs, and unions

#ifdef NANBOXING

typedef volatile void *volatile thing;

#else

enum tag {true_tag, false_tag, null_tag, real_tag, dual_number_tag, tape_tag,
	  pair_tag, closure_tag, checkpoint_tag};

struct object;

typedef struct object *thing;

#endif

struct dual_number {
#ifdef NANBOXING
  thing forward;
#endif
  unsigned epsilon;
  thing primal;
  thing tangent;
};

struct argument {
  thing factor;
  thing tape;
};

struct tape {
#ifdef NANBOXING
  thing forward;
#endif
  unsigned epsilon;
  thing primal;
  unsigned n;
  unsigned fanout;
  thing cotangent;
  struct argument arguments[0];
};

struct pair {
  thing cache;
#ifdef NANBOXING
  thing forward;
#endif
  thing car;
  thing cdr;
};

struct closure {
  thing cache;
#ifdef NANBOXING
  thing forward;
#endif
  thing (*function)();
  unsigned n;
  thing environment[0];
};

struct checkpoint {
  thing cache;
#ifdef NANBOXING
  thing forward;
#endif
  thing continuation;
  thing closure;
};

#ifndef NANBOXING

union value {
  double real;
  struct dual_number dual_number;
  struct tape tape;
  struct pair pair;
  struct closure closure;
  struct checkpoint checkpoint;
};

struct object {
  enum tag tag;
  union value value;
};

#endif

// variables

#ifdef NANBOXING

unsigned critical = 0;		// debugging

#endif

thing true_constant;

thing false_constant;

thing null_constant;

unsigned epsilon = 0;

thing continuation_that_returns_x;

thing continuation_that_returns_n;

thing closure_that_resumes;

#ifdef NANBOXING

thing heap0[HEAP];

thing heap1[HEAP];

int zero_to_one = TRUE;

thing from_begin = (thing)&heap0[0];

thing from_free_pointer = (thing)&heap0[0];

thing from_limit = (thing)&heap0[HEAP];

thing to_begin = (thing)&heap1[0];

thing to_free_pointer = (thing)&heap1[0];

thing to_limit = (thing)&heap1[HEAP];

thing *stack_bottom;

int debugging1 = FALSE;

#endif

int debugging2 = FALSE;

#ifdef NANBOXING

int debugging3 = FALSE;

int debugging4 = FALSE;

int debugging5 = FALSE;

int debugging6 = FALSE;

int debugging7 = FALSE;

int debugging8 = FALSE;

int debugging9 = FALSE;

#endif

int debugging10 = FALSE;

unsigned long d = 3;

unsigned long t = 5;

unsigned long (*mid)(unsigned long delta, unsigned long tau,
		     unsigned long sigma, unsigned long phi);

// Must be such that dividing it by two and rounding down does not become <=4
// because that would checkpoint during a preamble.
unsigned long base_case_duration;

// externs

extern void get_registers(thing *registers);

extern void put_registers(thing *registers);

// prototypes

static void __attribute__ ((noreturn)) panic(char *message);

static void __attribute__ ((noreturn)) internal_error(void);

#ifdef NANBOXING

static int inline is_true(thing value);

static int inline is_false(thing value);

static int inline is_null(thing value);

static int inline is_real(thing value);

static int inline is_dual_number(thing value);

static int inline is_tape(thing value);

static int inline is_pair(thing value);

static int inline is_closure(thing value);

static int inline is_checkpoint(thing value);

static void inline set_true(thing *value);

static void inline set_false(thing *value);

static void inline set_null(thing *value);

static void inline set_real(thing *value);

static void inline set_dual_number(thing *value);

static void inline set_tape(thing *value);

static void inline set_pair(thing *value);

static void inline set_closure(thing *value);

static void inline set_checkpoint(thing *value);

static double inline as_real(thing value);

static struct dual_number inline *as_dual_number(thing value);

static struct tape inline *as_tape(thing value);

static struct pair inline *as_pair(thing value);

static struct closure inline *as_closure(thing value);

static struct checkpoint inline *as_checkpoint(thing value);

static struct dual_number inline *as_new_dual_number(thing value);

static struct tape inline *as_new_tape(thing value);

static struct pair inline *as_new_pair(thing value);

static struct closure inline *as_new_closure(thing value);

static struct checkpoint inline *as_new_checkpoint(thing value);

static thing inline denanify(thing value);

static int inline in_from_space(thing value);

static int inline in_to_space(thing value);

static void inline set_as_real(thing *value, double real);

static void inline
set_as_dual_number(thing *value, struct dual_number *dual_number);

static void inline set_as_tape(thing *value, struct tape *tape);

static void inline set_as_pair(thing *value, struct pair *pair);

static void inline set_as_closure(thing *value, struct closure *closure);

static void inline
set_as_checkpoint(thing *value, struct checkpoint *checkpoint);

static int inline is_number(thing value);

// paper: n l v
static thing inline continuation_apply(thing target,
				       thing count,
				       thing limit,
				       thing argument);

// paper: k n l v
static thing inline converted_apply(thing target,
				    thing continuation,
				    thing count,
				    thing limit,
				    thing argument);

#else

static int inline __attribute__ ((always_inline)) is_true(thing value);

static int inline __attribute__ ((always_inline)) is_false(thing value);

static int inline __attribute__ ((always_inline)) is_null(thing value);

static int inline __attribute__ ((always_inline)) is_real(thing value);

static int inline __attribute__ ((always_inline)) is_dual_number(thing value);

static int inline __attribute__ ((always_inline)) is_tape(thing value);

static int inline __attribute__ ((always_inline)) is_pair(thing value);

static int inline __attribute__ ((always_inline)) is_closure(thing value);

static int inline __attribute__ ((always_inline)) is_checkpoint(thing value);

static void inline __attribute__ ((always_inline)) set_true(thing value);

static void inline __attribute__ ((always_inline)) set_false(thing value);

static void inline __attribute__ ((always_inline)) set_null(thing value);

static void inline __attribute__ ((always_inline)) set_real(thing value);

static void inline __attribute__ ((always_inline)) set_dual_number(thing value);

static void inline __attribute__ ((always_inline)) set_tape(thing value);

static void inline __attribute__ ((always_inline)) set_pair(thing value);

static void inline __attribute__ ((always_inline)) set_closure(thing value);

static void inline __attribute__ ((always_inline)) set_checkpoint(thing value);

static double inline __attribute__ ((always_inline)) as_real(thing value);

static struct dual_number inline __attribute__ ((always_inline))
*as_dual_number(thing value);

static struct tape inline __attribute__ ((always_inline)) *as_tape(thing value);

static struct pair inline __attribute__ ((always_inline)) *as_pair(thing value);

static struct closure inline __attribute__ ((always_inline))
*as_closure(thing value);

static struct checkpoint inline __attribute__ ((always_inline))
*as_checkpoint(thing value);

static void inline __attribute__ ((always_inline)) set_as_real(thing value,
							       double real);

static int inline __attribute__ ((always_inline)) is_number(thing value);

// paper: n l v
static thing inline __attribute__ ((always_inline))
continuation_apply(thing target,
		   thing count,
		   thing limit,
		   thing argument);

// paper: k n l v
static thing inline __attribute__ ((always_inline))
converted_apply(thing target,
		thing continuation,
		thing count,
		thing limit,
		thing argument);

#endif

static thing make_real(double value);

static void externalize(thing value);

#ifdef NANBOXING

static void externalize_new(thing value);

#endif

static thing cons(thing car, thing cdr);

static thing make_dual_number(unsigned epsilon,
			      thing primal,
			      thing tangent);

static thing new_tape1(unsigned epsilon,
		       thing primal,
		       thing factor,
		       thing tape);

static thing new_tape2(unsigned epsilon,
		       thing primal,
		       thing factor0,
		       thing factor1,
		       thing tape0,
		       thing tape1);

static thing tapify(thing x);

static thing lift_real_to_real(double (*f)(double),
			       thing (*dfdx)(thing),
			       thing x);

static thing lift_real_cross_real_to_real
(double (*f)(double, double),
 thing (*dfdx1)(thing, thing),
 thing (*dfdx2)(thing, thing),
 thing x1,
 thing x2);

static thing primal_star(thing x);

static thing lift_real_to_boolean(int (*f)(double), thing x);

static thing lift_real_cross_real_to_boolean(int (*f)(double, double),
					     thing x1,
					     thing x2);

static double base_plus(double x1, double x2);

static thing dfdx1_plus(thing x1, thing x2);

static thing dfdx2_plus(thing x1, thing x2);

static thing binary_plus(thing x1, thing x2);

static double base_minus(double x1, double x2);

static thing dfdx1_minus(thing x1, thing x2);

static thing dfdx2_minus(thing x1, thing x2);

static thing binary_minus(thing x1, thing x2);

static double base_times(double x1, double x2);

static thing dfdx1_times(thing x1, thing x2);

static thing dfdx2_times(thing x1, thing x2);

static thing binary_times(thing x1, thing x2);

static double base_divide(double x1, double x2);

static thing dfdx1_divide(thing x1, thing x2);

static thing dfdx2_divide(thing x1, thing x2);

static thing binary_divide(thing x1, thing x2);

static thing dfdx_sqrt(thing x);

static thing unary_sqrt(thing x);

static thing dfdx_exp(thing x);

static thing unary_exp(thing x);

static thing dfdx_log(thing x);

static thing unary_log(thing x);

static thing dfdx_sin(thing x);

static thing unary_sin(thing x);

static thing dfdx_cos(thing x);

static thing unary_cos(thing x);

static double base_atan(double x1, double x2);

static thing dfdx1_atan(thing x1, thing x2);

static thing dfdx2_atan(thing x1, thing x2);

static thing binary_atan(thing x1, thing x2);

static int base_eq(double x1, double x2);

static thing binary_eq(thing x1, thing x2);

static int base_lt(double x1, double x2);

static thing binary_lt(thing x1, thing x2);

static int base_gt(double x1, double x2);

static thing binary_gt(thing x1, thing x2);

static int base_le(double x1, double x2);

static thing binary_le(thing x1, thing x2);

static int base_ge(double x1, double x2);

static thing binary_ge(thing x1, thing x2);

static int base_zero(double x);

static thing unary_zero(thing x);

static int base_positive(double x);

static thing unary_positive(thing x);

static int base_negative(double x);

static thing unary_negative(thing x);

static thing unary_null(thing x);

static thing unary_boolean(thing x);

static thing unary_real(thing x);

static thing unary_pair(thing x);

static thing unary_procedure(thing x);

static thing unary_read_real(thing x);

static thing unary_real_coercion(thing x);

static thing unary_write_real(thing x);

static thing binary_expt(thing x1, thing x2);

static thing unary_floor(thing x);

static thing binary_modulo(thing x1, thing x2);

static thing unary_increment(thing x);

static thing binary_limit_check(thing x1, thing x2);

static thing binary_make_checkpoint(thing x1, thing x2);

static void determine_fanout(thing tape);

static void initialize_cotangent(thing tape);

static void reverse_phase(thing cotangent, thing tape);

static void clear_cache(thing x);

static thing walk1(thing (*f)(thing), thing x);

static thing walk2(thing (*f)(thing, thing), thing x, thing x_prime);

static void walk1b(void (*f)(thing), thing x);

static void walk2b(void (*f)(thing, thing), thing x, thing x_prime);

static thing forward_mode
(thing (*map_independent)
 (thing (*)(thing, thing), thing, thing),
 thing (*map_dependent)(thing (*)(thing), thing),
 thing (*f)(thing),
 thing x,
 thing x_tangent);

static thing reverse_mode
(thing (*map_independent)(thing (*)(thing), thing),
 thing (*map_dependent)(thing (*)(thing), thing),
 void (*for_each_dependent1)(void (*)(thing), thing),
 void (*for_each_dependent2)(void (*)(thing, thing), thing, thing),
 thing (*f)(thing),
 thing x,
 thing y_cotangent);

static thing lambda_expression_that_returns_x
(thing target, thing count, thing limit, thing argument);

static thing lambda_expression_that_returns_n
(thing target, thing count, thing limit, thing argument);

static thing lambda_expression_that_resumes
(thing target, thing continuation, thing count, thing limit, thing argument);

static thing lambda_expression_for_checkpoint
(thing target, thing continuation, thing count, thing limit, thing argument);

// paper: f l
static thing make_closure_for_checkpoint(thing value1, unsigned long limit);

// paper: v
static thing ternary_jstar(thing x1, thing x2, thing x3);

// paper: v
static thing ternary_starj(thing x1, thing x2, thing x3);

// paper: f x
static unsigned long primops(thing value1, thing value2);

// paper: f x l
static thing checkpoint(thing value1, thing value2, thing value3);

// paper: f x y_grave
static thing ternary_checkpoint_starj(thing value1, thing value2, thing value3);

static thing ternary_binary_checkpoint_starj(thing f, thing x, thing y_grave);

static thing treeverse(thing f, thing x, thing y_grave, unsigned long delta,
		       unsigned long tau, unsigned long beta,
		       unsigned long sigma, unsigned long phi);

static thing first(thing f, thing x, thing y_grave, unsigned long delta,
		   unsigned long tau, unsigned long beta, unsigned long sigma,
		   unsigned long phi);

static thing rest(thing f, thing x, thing y_grave, thing y, unsigned long delta,
		  unsigned long tau, unsigned long beta, unsigned long sigma,
		  unsigned long phi);

static thing ternary_treeverse_checkpoint_starj(thing f, thing x,
						thing y_grave);


unsigned long bisection_mid(unsigned long delta, unsigned long tau,
			    unsigned long sigma, unsigned long phi);

unsigned long binomial_mid(unsigned long delta, unsigned long tau,
			   unsigned long sigma, unsigned long phi);

unsigned long choose(unsigned long n, unsigned long r);

unsigned long eta(unsigned long d, unsigned long t);

unsigned long fixed_d(unsigned long d, unsigned long base_case_duration,
		      unsigned long l);

unsigned long fixed_t(unsigned long d, unsigned long base_case_duration,
		      unsigned long l);

unsigned long fixed_base_case_duration(unsigned long base_case_duration,
				       unsigned long l);

static void initialize_constants(void);

#ifdef NANBOXING

static thing *stack_pointer(thing x);

static thing allocate(size_t n);

static thing gc_allocate(size_t n);

static thing copy(thing old);

static void gc(void);

#endif

// functions

static void __attribute__ ((noreturn)) panic(char *message) {
  fprintf(stderr, "%s\n", message);
  exit(EXIT_FAILURE);
}

static void __attribute__ ((noreturn)) internal_error(void) {
  panic("Internal error");
}

#ifdef NANBOXING

// I first learned of this trick from Richard O'Keefe in around 1996.
// This is hardwired to 64 bits with IEEE 754-1985.
// We assume that all pointers are 8 byte aligned. This allows using the low
// order 3 bits as tags.
// true        01000
// false       10000
// null        11000
// dual_number   001
// tape          010
// pair          011
// closure       100
// checkpoint    101
// unused        110
// unused        111
// We assume that all pointers have upper 12 bits zero.
//\needswork: To make sure that we don't encode anything as the kind of NaN
//            returned by primitives and to have is_real return true for such.
// These constants can appear on the stack.
// 0x0000000000000000ul
// 0x7ff0000000000000ul infinity
// 0x7ff0000000000001ul
// 0x7ff0000000000002ul
// 0x7ff0000000000003ul
// 0x7ff0000000000004ul
// 0x7ff0000000000005ul
// 0x7ff0000000000006ul
// 0x7ff0000000000007ul
// 0x7ff0000000000008ul
// 0x7ff0000000000010ul
// 0x7ff0000000000018ul
// These unsigned values can appear on the stack.
// unsigned epsilon
// unsigned n
// unsigned fanout
// unsigned long count
// unsigned base_case_duration
// unsigned long limit
// unsigned long primops()
// unsigned i
// Except for count, limit, and primops(), they should never be as large as
// 0x7ff00000u.

static int inline is_true(thing value) {
  return ((unsigned long)value)==0x7ff0000000000008ul;
}

static int inline is_false(thing value) {
  return ((unsigned long)value)==0x7ff0000000000010ul;
}

static int inline is_null(thing value) {
  return ((unsigned long)value)==0x7ff0000000000018ul;
}

static int inline is_real(thing value) {
  // NaN has ones for exponent and non-zero for mantissa.
  return ((((unsigned long)value)&0x7ff0000000000000ul)!=0x7ff0000000000000ul)||
    ((((unsigned long)value)&0x000ffffffffffffful)==0x0000000000000000ul);
}

static int inline is_dual_number(thing value) {
  return ((((unsigned long)value)&0x7ff0000000000000ul)==0x7ff0000000000000ul)&&
    ((((unsigned long)value)&0x0000000000000007ul)==0x0000000000000001ul);
}

static int inline is_tape(thing value) {
  return ((((unsigned long)value)&0x7ff0000000000000ul)==0x7ff0000000000000ul)&&
    ((((unsigned long)value)&0x0000000000000007ul)==0x0000000000000002ul);
}

static int inline is_pair(thing value) {
  return ((((unsigned long)value)&0x7ff0000000000000ul)==0x7ff0000000000000ul)&&
    ((((unsigned long)value)&0x0000000000000007ul)==0x0000000000000003ul);
}

static int inline is_closure(thing value) {
  return ((((unsigned long)value)&0x7ff0000000000000ul)==0x7ff0000000000000ul)&&
    ((((unsigned long)value)&0x0000000000000007ul)==0x0000000000000004ul);
}

static int inline is_checkpoint(thing value) {
  return ((((unsigned long)value)&0x7ff0000000000000ul)==0x7ff0000000000000ul)&&
    ((((unsigned long)value)&0x0000000000000007ul)==0x0000000000000005ul);
}

// All these are noops because setting the tags is done by set_as_*. This
// assumes the cliche of set_* followed by set_as_*.

static void inline set_true(thing *value) {
  *((unsigned long *)value) = 0x7ff0000000000008ul;
}

static void inline set_false(thing *value) {
  *((unsigned long *)value) = 0x7ff0000000000010ul;
}

static void inline set_null(thing *value) {
  *((unsigned long *)value) = 0x7ff0000000000018ul;
}

static void inline set_real(thing *value) {
  return;
}

static void inline set_dual_number(thing *value) {
  return;
}

static void inline set_tape(thing *value) {
  return;
}

static void inline set_pair(thing *value) {
  return;
}

static void inline set_closure(thing *value) {
  return;
}

static void inline set_checkpoint(thing *value) {
  return;
}

static double inline as_real(thing value) {
  if (!is_real(value)) internal_error();
  // This is not guaranteed to work. But can't cast void * to double.
  union mix {
    double real;
    thing thing;
  };
  union mix mix;
  mix.thing = value;
  return mix.real;
}

static struct dual_number inline *as_dual_number(thing value) {
  if (!is_dual_number(value)) internal_error();
  if (!in_from_space(value)) internal_error();
  return (struct dual_number *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct tape inline *as_tape(thing value) {
  if (!is_tape(value)) internal_error();
  if (!in_from_space(value)) internal_error();
  return (struct tape *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct pair inline *as_pair(thing value) {
  if (!is_pair(value)) internal_error();
  if (!in_from_space(value)) internal_error();
  return (struct pair *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct closure inline *as_closure(thing value) {
  if (!is_closure(value)) internal_error();
  if (!in_from_space(value)) {
    printf("not in from space %#018lx\n", (unsigned long)value);
    internal_error();
  }
  return (struct closure *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct checkpoint inline *as_checkpoint(thing value) {
  if (!is_checkpoint(value)) internal_error();
  if (!in_from_space(value)) internal_error();
  return (struct checkpoint *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct dual_number inline *as_new_dual_number(thing value) {
  if (!is_dual_number(value)) internal_error();
  if (!in_to_space(value)) internal_error();
  return (struct dual_number *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct tape inline *as_new_tape(thing value) {
  if (!is_tape(value)) internal_error();
  if (!in_to_space(value)) internal_error();
  return (struct tape *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct pair inline *as_new_pair(thing value) {
  if (!is_pair(value)) internal_error();
  if (!in_to_space(value)) internal_error();
  return (struct pair *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct closure inline *as_new_closure(thing value) {
  if (!is_closure(value)) internal_error();
  if (!in_to_space(value)) internal_error();
  return (struct closure *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static struct checkpoint inline *as_new_checkpoint(thing value) {
  if (!is_checkpoint(value)) internal_error();
  if (!in_to_space(value)) internal_error();
  return (struct checkpoint *)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static thing inline denanify(thing value) {
  return (thing)(((unsigned long)value)&0x800ffffffffffff8ul);
}

static int inline in_from_space(thing value) {
  return from_begin<=denanify(value)&&denanify(value)<from_free_pointer;
}

static int inline in_to_space(thing value) {
  return to_begin<=denanify(value)&&denanify(value)<to_free_pointer;
}

static void inline set_as_real(thing *value, double real) {
  // This is not guaranteed to work. But casting from double to any integer
  // type coerces and doesn't get the representation.
  union mix {
    double real;
    unsigned long bits;
  };
  union mix mix;
  mix.real = real;
  if (((mix.bits&0x7ff0000000000000ul)==0x7ff0000000000000ul)&&
      ((mix.bits&0x000ffffffffffffful)!=0x0000000000000000ul)) {
    panic("Result is NaN");
  }
  *((double *)value) = real;
}

static void inline
set_as_dual_number(thing *value, struct dual_number *dual_number) {
  if ((((unsigned long)dual_number)&0x7ff0000000000007ul)
      !=0x0000000000000000ul) {
    panic("Can't squash dual number");
  }
  *value = (thing)(((unsigned long)dual_number)|0x7ff0000000000001ul);
}

static void inline set_as_tape(thing *value, struct tape *tape) {
  if ((((unsigned long)tape)&0x7ff0000000000007ul)!=0x0000000000000000ul) {
    panic("Can't squash tape");
  }
  *value = (thing)(((unsigned long)tape)|0x7ff0000000000002ul);
}

static void inline set_as_pair(thing *value, struct pair *pair) {
  if ((((unsigned long)pair)&0x7ff0000000000007ul)!=0x0000000000000000ul) {
    panic("Can't squash pair");
  }
  *value = (thing)(((unsigned long)pair)|0x7ff0000000000003ul);
}

static void inline set_as_closure(thing *value, struct closure *closure) {
  if ((((unsigned long)closure)&0x7ff0000000000007ul)!=0x0000000000000000ul) {
    panic("Can't squash closure");
  }
  *value = (thing)(((unsigned long)closure)|0x7ff0000000000004ul);
}

static void inline
set_as_checkpoint(thing *value, struct checkpoint *checkpoint) {
  if ((((unsigned long)checkpoint)&0x7ff0000000000007ul)
      !=0x0000000000000000ul) {
    panic("Can't squash checkpoint");
  }
  *value = (thing)(((unsigned long)checkpoint)|0x7ff0000000000005ul);
}

// the next two are debugging

static void inline set_as_new_pair(thing *value, struct pair *pair) {
  if ((((unsigned long)pair)&0x7ff0000000000007ul)!=0x0000000000000000ul) {
    panic("Can't squash pair");
  }
  *value = (thing)(((unsigned long)pair)|0x7ff0000000000003ul);
}

static void inline set_as_new_closure(thing *value, struct closure *closure) {
  if ((((unsigned long)closure)&0x7ff0000000000007ul)!=0x0000000000000000ul) {
    panic("Can't squash closure");
  }
  *value = (thing)(((unsigned long)closure)|0x7ff0000000000004ul);
}

static int inline is_number(thing value) {
  return is_real(value)||is_dual_number(value)||is_tape(value);
}

// paper: n l v
static thing inline continuation_apply(thing target,
				       thing count,
				       thing limit,
				       thing argument) {
  if (!is_closure(target)) {
    externalize(target);
    printf("\n");
    panic("Target not a closure");
  }
  return (*as_closure(target)->function)(target, count, limit, argument);
}

// paper: k n l v
static thing inline converted_apply(thing target,
				    thing continuation,
				    thing count,
				    thing limit,
				    thing argument) {
  if (!is_closure(target)) {
    externalize(target);
    printf("\n");
    panic("Target not a closure");
  }
  return (*as_closure(target)->function)
    (target, continuation, count, limit, argument);
}

static thing make_real(double value) {
  thing object;
  set_real(&object);
  set_as_real(&object, value);
  return object;
}

#else

static int inline __attribute__ ((always_inline)) is_true(thing value) {
  return value->tag==true_tag;
}

static int inline __attribute__ ((always_inline)) is_false(thing value) {
  return value->tag==false_tag;
}

static int inline __attribute__ ((always_inline)) is_null(thing value) {
  return value->tag==null_tag;
}

static int inline __attribute__ ((always_inline)) is_real(thing value) {
  return value->tag==real_tag;
}

static int inline __attribute__ ((always_inline)) is_dual_number(thing value) {
  return value->tag==dual_number_tag;
}

static int inline __attribute__ ((always_inline)) is_tape(thing value) {
  return value->tag==tape_tag;
}

static int inline __attribute__ ((always_inline)) is_pair(thing value) {
  return value->tag==pair_tag;
}

static int inline __attribute__ ((always_inline)) is_closure(thing value) {
  return value->tag==closure_tag;
}

static int inline __attribute__ ((always_inline)) is_checkpoint(thing value) {
  return value->tag==checkpoint_tag;
}

static void inline __attribute__ ((always_inline)) set_true(thing value) {
  value->tag = true_tag;
}

static void inline __attribute__ ((always_inline)) set_false(thing value) {
  value->tag = false_tag;
}

static void inline __attribute__ ((always_inline)) set_null(thing value) {
  value->tag = null_tag;
}

static void inline __attribute__ ((always_inline)) set_real(thing value) {
  value->tag = real_tag;
}

static void inline __attribute__ ((always_inline))
  set_dual_number(thing value) {
  value->tag = dual_number_tag;
}

static void inline __attribute__ ((always_inline)) set_tape(thing value) {
  value->tag = tape_tag;
}

static void inline __attribute__ ((always_inline)) set_pair(thing value) {
  value->tag = pair_tag;
}

static void inline __attribute__ ((always_inline)) set_closure(thing value) {
  value->tag = closure_tag;
}

static void inline __attribute__ ((always_inline)) set_checkpoint(thing value) {
  value->tag = checkpoint_tag;
}

static double inline __attribute__ ((always_inline)) as_real(thing value) {
  return value->value.real;
}

static struct dual_number inline __attribute__ ((always_inline))
*as_dual_number(thing value) {
  return &value->value.dual_number;
}

static struct tape inline __attribute__ ((always_inline))
*as_tape(thing value) {
  return &value->value.tape;
}

static struct pair inline __attribute__ ((always_inline))
*as_pair(thing value) {
  return &value->value.pair;
}

static struct closure inline __attribute__ ((always_inline))
*as_closure(thing value) {
  return &value->value.closure;
}

static struct checkpoint inline __attribute__ ((always_inline))
*as_checkpoint(thing value) {
  return &value->value.checkpoint;
}

static void inline __attribute__ ((always_inline)) set_as_real(thing value,
							       double real) {
  value->value.real = real;
}

static int inline __attribute__ ((always_inline)) is_number(thing value) {
  return is_real(value)||is_dual_number(value)||is_tape(value);
}

// paper: n l v
static thing inline __attribute__ ((always_inline))
continuation_apply(thing target,
		   thing count,
		   thing limit,
		   thing argument) {
  if (!is_closure(target)) {
    externalize(target);
    printf("\n");
    panic("Target not a closure");
  }
  return (*as_closure(target)->function)(target, count, limit, argument);
}

// paper: k n l v
static thing inline __attribute__ ((always_inline))
converted_apply(thing target,
		thing continuation,
		thing count,
		thing limit,
		thing argument) {
  if (!is_closure(target)) {
    externalize(target);
    printf("\n");
    panic("Target not a closure");
  }
  return (*as_closure(target)->function)
    (target, continuation, count, limit, argument);
}

static thing make_real(double value) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    double value;
  }));
  set_real(object);
  set_as_real(object, value);
  return object;
}

#endif

static void externalize(thing value) {
  if (is_true(value)) printf("true");
  else if (is_false(value)) printf("false");
  else if (is_null(value)) printf("null");
  else if (is_real(value)) printf("%.18lg", as_real(value));
  else if (is_dual_number(value)) {
    // We don't print forward.
    printf("dual(%u,", as_dual_number(value)->epsilon);
    externalize(as_dual_number(value)->primal);
    printf(",");
    externalize(as_dual_number(value)->tangent);
    printf(")");
  }
  else if (is_tape(value)) {
    // We don't print forward.
    printf("tape(%u,", as_tape(value)->epsilon);
    externalize(as_tape(value)->primal);
    for (unsigned i = 0; i<as_tape(value)->n; i++) {
      printf(",");
      externalize(as_tape(value)->arguments[i].factor);
      printf(",");
      externalize(as_tape(value)->arguments[i].tape);
    }
    printf(",%u,", as_tape(value)->fanout);
    externalize(as_tape(value)->cotangent);
    printf(")");
  }
  else if (is_pair(value)) {
    // We don't print cache and forward.
    printf("pair(");
    externalize(as_pair(value)->car);
    printf(",");
    externalize(as_pair(value)->cdr);
    printf(")");
  }
  else if (is_closure(value)) {
    // We don't print cache and forward.
    printf("closure(%#018lx", (unsigned long)as_closure(value)->function);
    for (unsigned i = 0; i<as_closure(value)->n; i++) {
      printf(",");
      externalize(as_closure(value)->environment[i]);
    }
    printf(")");
  }
  else if (is_checkpoint(value)) {
    // We don't print cache and forward.
    printf("checkpoint(");
    externalize(as_checkpoint(value)->continuation);
    printf(",");
    externalize(as_checkpoint(value)->closure);
    printf(")");
  }
  else internal_error();
}

#ifdef NANBOXING

static void externalize_new(thing value) {
  if (is_true(value)) printf("true");
  else if (is_false(value)) printf("false");
  else if (is_null(value)) printf("null");
  else if (is_real(value)) printf("%.18lg", as_real(value));
  else if (is_dual_number(value)) {
    // We don't print forward.
    printf("dual(%u,", as_new_dual_number(value)->epsilon);
    externalize_new(as_new_dual_number(value)->primal);
    printf(",");
    externalize_new(as_new_dual_number(value)->tangent);
    printf(")");
  }
  else if (is_tape(value)) {
    // We don't print forward.
    printf("tape(%u,", as_new_tape(value)->epsilon);
    externalize_new(as_new_tape(value)->primal);
    for (unsigned i = 0; i<as_new_tape(value)->n; i++) {
      printf(",");
      externalize_new(as_new_tape(value)->arguments[i].factor);
      printf(",");
      externalize_new(as_new_tape(value)->arguments[i].tape);
    }
    printf(",%u,", as_new_tape(value)->fanout);
    externalize_new(as_new_tape(value)->cotangent);
    printf(")");
  }
  else if (is_pair(value)) {
    // We don't print cache and forward.
    printf("pair(");
    externalize_new(as_new_pair(value)->car);
    printf(",");
    externalize_new(as_new_pair(value)->cdr);
    printf(")");
  }
  else if (is_closure(value)) {
    // We don't print cache and forward.
    printf("closure(%#018lx", (unsigned long)as_new_closure(value)->function);
    for (unsigned i = 0; i<as_new_closure(value)->n; i++) {
      printf(",");
      externalize_new(as_new_closure(value)->environment[i]);
    }
    printf(")");
  }
  else if (is_checkpoint(value)) {
    // We don't print cache and forward.
    printf("checkpoint(");
    externalize_new(as_new_checkpoint(value)->continuation);
    printf(",");
    externalize_new(as_new_checkpoint(value)->closure);
    printf(")");
  }
  else internal_error();
}

#endif

#ifdef NANBOXING

static thing cons(thing car, thing cdr) {
  thing object;
  set_pair(&object);
  set_as_pair(&object, (struct pair *)allocate(sizeof(struct pair)));
  as_pair(object)->cache = NULL;
  as_pair(object)->forward = NULL;
  as_pair(object)->car = car;
  as_pair(object)->cdr = cdr;
  return object;
}

static thing make_dual_number(unsigned epsilon,
			      thing primal,
			      thing tangent) {
  thing object;
  set_dual_number(&object);
  set_as_dual_number(&object,
		     (struct dual_number *)
		     allocate(sizeof(struct dual_number)));
  as_dual_number(object)->forward = NULL;
  as_dual_number(object)->epsilon = epsilon;
  as_dual_number(object)->primal = primal;
  as_dual_number(object)->tangent = tangent;
  return object;
}

static thing new_tape1(unsigned epsilon,
		       thing primal,
		       thing factor,
		       thing tape) {
  thing object;
  set_tape(&object);
  set_as_tape(&object, (struct tape *)allocate(sizeof(struct {
    thing forward;
    unsigned epsilon;
    thing primal;
    unsigned n;
    unsigned fanout;
    thing cotangent;
    struct argument arguments[1];
  })));
  as_tape(object)->forward = NULL;
  as_tape(object)->epsilon = epsilon;
  as_tape(object)->primal = primal;
  as_tape(object)->n = 1;
  as_tape(object)->fanout = 0;
  as_tape(object)->cotangent = make_real(0.0);
  as_tape(object)->arguments[0].factor = factor;
  as_tape(object)->arguments[0].tape = tape;
  return object;
}

static thing new_tape2(unsigned epsilon,
		       thing primal,
		       thing factor0,
		       thing factor1,
		       thing tape0,
		       thing tape1) {
  thing object;
  set_tape(&object);
  set_as_tape(&object, (struct tape *)allocate(sizeof(struct {
    thing forward;
    unsigned epsilon;
    thing primal;
    unsigned n;
    unsigned fanout;
    thing cotangent;
    struct argument arguments[2];
  })));
  as_tape(object)->forward = NULL;
  as_tape(object)->epsilon = epsilon;
  as_tape(object)->primal = primal;
  as_tape(object)->n = 2;
  as_tape(object)->fanout = 0;
  as_tape(object)->cotangent = make_real(0.0);
  as_tape(object)->arguments[0].factor = factor0;
  as_tape(object)->arguments[1].factor = factor1;
  as_tape(object)->arguments[0].tape = tape0;
  as_tape(object)->arguments[1].tape = tape1;
  return object;
}

static thing tapify(thing x) {
  thing object;
  set_tape(&object);
  set_as_tape(&object, (struct tape *)allocate(sizeof(struct {
    thing forward;
    unsigned epsilon;
    thing primal;
    unsigned n;
    unsigned fanout;
    thing cotangent;
    struct argument arguments[0];
  })));
  as_tape(object)->forward = NULL;
  as_tape(object)->epsilon = epsilon;
  as_tape(object)->primal = x;
  as_tape(object)->n = 0;
  as_tape(object)->fanout = 0;
  as_tape(object)->cotangent = make_real(0.0);
  return object;
}

#else

static thing cons(thing car, thing cdr) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      thing cache;
      thing car;
      thing cdr;
    };
  }));
  set_pair(object);
  as_pair(object)->cache = NULL;
  as_pair(object)->car = car;
  as_pair(object)->cdr = cdr;
  return object;
}

static thing make_dual_number(unsigned epsilon,
			      thing primal,
			      thing tangent) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      unsigned epsilon;
      thing primal;
      thing tangent;
    };
  }));
  set_dual_number(object);
  as_dual_number(object)->epsilon = epsilon;
  as_dual_number(object)->primal = primal;
  as_dual_number(object)->tangent = tangent;
  return object;
}

static thing new_tape1(unsigned epsilon,
		       thing primal,
		       thing factor,
		       thing tape) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      unsigned epsilon;
      thing primal;
      unsigned n;
      unsigned fanout;
      thing cotangent;
      struct argument arguments[1];
    };
  }));
  set_tape(object);
  as_tape(object)->epsilon = epsilon;
  as_tape(object)->primal = primal;
  as_tape(object)->n = 1;
  as_tape(object)->fanout = 0;
  as_tape(object)->cotangent = make_real(0.0);
  as_tape(object)->arguments[0].factor = factor;
  as_tape(object)->arguments[0].tape = tape;
  return object;
}

static thing new_tape2(unsigned epsilon,
		       thing primal,
		       thing factor0,
		       thing factor1,
		       thing tape0,
		       thing tape1) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      unsigned epsilon;
      thing primal;
      unsigned n;
      unsigned fanout;
      thing cotangent;
      struct argument arguments[2];
    };
  }));
  set_tape(object);
  as_tape(object)->epsilon = epsilon;
  as_tape(object)->primal = primal;
  as_tape(object)->n = 2;
  as_tape(object)->fanout = 0;
  as_tape(object)->cotangent = make_real(0.0);
  as_tape(object)->arguments[0].factor = factor0;
  as_tape(object)->arguments[1].factor = factor1;
  as_tape(object)->arguments[0].tape = tape0;
  as_tape(object)->arguments[1].tape = tape1;
  return object;
}

static thing tapify(thing x) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      unsigned epsilon;
      thing primal;
      unsigned n;
      unsigned fanout;
      thing cotangent;
      struct argument arguments[0];
    };
  }));
  set_tape(object);
  as_tape(object)->epsilon = epsilon;
  as_tape(object)->primal = x;
  as_tape(object)->n = 0;
  as_tape(object)->fanout = 0;
  as_tape(object)->cotangent = make_real(0.0);
  return object;
}

#endif

static thing lift_real_to_real(double (*f)(double),
			       thing (*dfdx)(thing),
			       thing x) {
  if (is_dual_number(x)) {
    return make_dual_number(as_dual_number(x)->epsilon,
			    lift_real_to_real(f,
					      dfdx,
					      as_dual_number(x)->primal),
			    binary_times((*dfdx)(as_dual_number(x)->primal),
					 as_dual_number(x)->tangent));
  }
  else if (is_tape(x)) {
    return new_tape1(as_tape(x)->epsilon,
		     lift_real_to_real(f, dfdx, as_tape(x)->primal),
		     (*dfdx)(as_tape(x)->primal),
		     x);
  }
  else if (is_real(x)) return make_real((*f)(as_real(x)));
  else panic("Argument not a number");
}

static thing lift_real_cross_real_to_real
(double (*f)(double, double),
 thing (*dfdx1)(thing, thing),
 thing (*dfdx2)(thing, thing),
 thing x1,
 thing x2) {
  if (is_dual_number(x1)) {
    if (is_dual_number(x2)) {
      if (as_dual_number(x1)->epsilon<as_dual_number(x2)->epsilon) {
	return make_dual_number
	  (as_dual_number(x2)->epsilon,
	   lift_real_cross_real_to_real(f,
					dfdx1,
					dfdx2,
					x1,
					as_dual_number(x2)->primal),
	   binary_times((*dfdx2)(x1, as_dual_number(x2)->primal),
			as_dual_number(x2)->tangent));
      }
      else if (as_dual_number(x2)->epsilon<as_dual_number(x1)->epsilon) {
	return make_dual_number
	  (as_dual_number(x1)->epsilon,
	   lift_real_cross_real_to_real(f,
					dfdx1,
					dfdx2,
					as_dual_number(x1)->primal,
					x2),
	   binary_times((*dfdx1)(as_dual_number(x1)->primal, x2),
			as_dual_number(x1)->tangent));
      }
      else {
	return make_dual_number
	  (as_dual_number(x1)->epsilon,
	   lift_real_cross_real_to_real(f,
					dfdx1,
					dfdx2,
					as_dual_number(x1)->primal,
					as_dual_number(x2)->primal),
	   binary_plus(binary_times((*dfdx1)(as_dual_number(x1)->primal,
					     as_dual_number(x2)->primal),
				    as_dual_number(x1)->tangent),
		       binary_times((*dfdx2)(as_dual_number(x1)->primal,
					     as_dual_number(x2)->primal),
				    as_dual_number(x2)->tangent)));
      }
    }
    else if (is_tape(x2)) {
      if (as_dual_number(x1)->epsilon<as_tape(x2)->epsilon) {
	return new_tape1(as_tape(x2)->epsilon,
			 lift_real_cross_real_to_real(f,
						      dfdx1,
						      dfdx2,
						      x1,
						      as_tape(x2)->primal),
			 (*dfdx2)(x1, as_tape(x2)->primal),
			 x2);
      }
      else {
	return make_dual_number
	  (as_dual_number(x1)->epsilon,
	   lift_real_cross_real_to_real(f,
					dfdx1,
					dfdx2,
					as_dual_number(x1)->primal,
					x2),
	   binary_times((*dfdx1)(as_dual_number(x1)->primal, x2),
			as_dual_number(x1)->tangent));
      }
    }
    else if (is_real(x2)) {
      return make_dual_number
	(as_dual_number(x1)->epsilon,
	 lift_real_cross_real_to_real(f,
				      dfdx1,
				      dfdx2,
				      as_dual_number(x1)->primal,
				      x2),
	 binary_times((*dfdx1)(as_dual_number(x1)->primal, x2),
		      as_dual_number(x1)->tangent));
    }
    else panic("Second argument not a number");
  }
  else if (is_tape(x1)) {
    if (is_dual_number(x2)) {
      if (as_tape(x1)->epsilon<as_dual_number(x2)->epsilon) {
	return make_dual_number
	  (as_dual_number(x2)->epsilon,
	   lift_real_cross_real_to_real(f,
					dfdx1,
					dfdx2,
					x1,
					as_dual_number(x2)->primal),
	   binary_times((*dfdx2)(x1, as_dual_number(x2)->primal),
			as_dual_number(x2)->tangent));
      }
      else {
	return new_tape1(as_tape(x1)->epsilon,
			 lift_real_cross_real_to_real(f,
						      dfdx1,
						      dfdx2,
						      as_tape(x1)->primal,
						      x2),
			 (*dfdx1)(as_tape(x1)->primal, x2),
			 x1);
      }
    }
    else if (is_tape(x2)) {
      if (as_tape(x1)->epsilon<as_tape(x2)->epsilon) {
	return new_tape1(as_tape(x2)->epsilon,
			 lift_real_cross_real_to_real(f,
						      dfdx1,
						      dfdx2,
						      x1,
						      as_tape(x2)->primal),
			 (*dfdx2)(x1, as_tape(x2)->primal),
			 x2);
      }
      else if (as_tape(x2)->epsilon<as_tape(x1)->epsilon) {
	return new_tape1(as_tape(x1)->epsilon,
			 lift_real_cross_real_to_real(f,
						      dfdx1,
						      dfdx2,
						      as_tape(x1)->primal,
						      x2),
			 (*dfdx1)(as_tape(x1)->primal, x2),
			 x1);
      }
      else {
	return new_tape2(as_tape(x1)->epsilon,
			 lift_real_cross_real_to_real(f,
						      dfdx1,
						      dfdx2,
						      as_tape(x1)->primal,
						      as_tape(x2)->primal),
			 (*dfdx1)(as_tape(x1)->primal, as_tape(x2)->primal),
			 (*dfdx2)(as_tape(x1)->primal, as_tape(x2)->primal),
			 x1,
			 x2);
      }
    }
    else if (is_real(x2)) {
      return new_tape1(as_tape(x1)->epsilon,
		       lift_real_cross_real_to_real(f,
						    dfdx1,
						    dfdx2,
						    as_tape(x1)->primal,
						    x2),
		       (*dfdx1)(as_tape(x1)->primal, x2),
		       x1);
    }
    else panic("Second argument not a number");
  }
  else if (is_real(x1)) {
    if (is_dual_number(x2)) {
      return make_dual_number
	(as_dual_number(x2)->epsilon,
	 lift_real_cross_real_to_real(f,
				      dfdx1,
				      dfdx2,
				      x1,
				      as_dual_number(x2)->primal),
	 binary_times((*dfdx2)(x1, as_dual_number(x2)->primal),
		      as_dual_number(x2)->tangent));
    }
    else if (is_tape(x2)) {
      return new_tape1(as_tape(x2)->epsilon,
		       lift_real_cross_real_to_real(f,
						    dfdx1,
						    dfdx2,
						    x1,
						    as_tape(x2)->primal),
		       (*dfdx2)(x1, as_tape(x2)->primal),
		       x2);
    }
    else if (is_real(x2)) {
      return make_real((*f)(as_real(x1), as_real(x2)));
    }
    else panic("Second argument not a number");
  }
  else panic("First argument not a number");
}

static thing primal_star(thing x) {
  if (is_dual_number(x)) {
    return primal_star(as_dual_number(x)->primal);
  }
  else if (is_tape(x)) {
    return primal_star(as_tape(x)->primal);
  }
  else if (is_real(x)) return x;
  else panic("Argument not a number");

}

static thing lift_real_to_boolean(int (*f)(double), thing x) {
  if (f(as_real(primal_star(x)))) return true_constant;
  else return false_constant;
}

static thing lift_real_cross_real_to_boolean(int (*f)(double, double),
					     thing x1,
					     thing x2) {
  if (f(as_real(primal_star(x1)), as_real(primal_star(x2)))) {
    return true_constant;
  }
  else return false_constant;
}

static double base_plus(double x1, double x2) {
  return x1+x2;
}

static thing dfdx1_plus(thing x1, thing x2) {
  return make_real(1.0);
}

static thing dfdx2_plus(thing x1, thing x2) {
  return make_real(1.0);
}

static thing binary_plus(thing x1, thing x2) {
  return lift_real_cross_real_to_real(&base_plus,
				      &dfdx1_plus,
				      &dfdx2_plus,
				      x1,
				      x2);
}

static double base_minus(double x1, double x2) {
  return x1-x2;
}

static thing dfdx1_minus(thing x1, thing x2) {
  return make_real(1.0);
}

static thing dfdx2_minus(thing x1, thing x2) {
  return make_real(-1.0);
}

static thing binary_minus(thing x1, thing x2) {
  return lift_real_cross_real_to_real(&base_minus,
				      &dfdx1_minus,
				      &dfdx2_minus,
				      x1,
				      x2);
}

static double base_times(double x1, double x2) {
  return x1*x2;
}

static thing dfdx1_times(thing x1, thing x2) {
  return x2;
}

static thing dfdx2_times(thing x1, thing x2) {
  return x1;
}

static thing binary_times(thing x1, thing x2) {
  return lift_real_cross_real_to_real(&base_times,
				      &dfdx1_times,
				      &dfdx2_times,
				      x1,
				      x2);
}

static double base_divide(double x1, double x2) {
  return x1/x2;
}

static thing dfdx1_divide(thing x1, thing x2) {
  return binary_divide(make_real(1.0), x2);
}

static thing dfdx2_divide(thing x1, thing x2) {
  return binary_minus(make_real(0.0), binary_divide(x1, binary_times(x2, x2)));
}

static thing binary_divide(thing x1, thing x2) {
  return lift_real_cross_real_to_real(&base_divide,
				      &dfdx1_divide,
				      &dfdx2_divide,
				      x1,
				      x2);
}

static thing dfdx_sqrt(thing x) {
  return binary_divide(make_real(1.0),
		       binary_times(make_real(2.0), unary_sqrt(x)));
}

static thing unary_sqrt(thing x) {
  return lift_real_to_real(&sqrt, &dfdx_sqrt, x);
}

static thing dfdx_exp(thing x) {
  return unary_exp(x);
}

static thing unary_exp(thing x) {
  return lift_real_to_real(&exp, &dfdx_exp, x);
}

static thing dfdx_log(thing x) {
  return binary_divide(make_real(1.0), x);
}

static thing unary_log(thing x) {
  return lift_real_to_real(&log, &dfdx_log, x);
}

static thing dfdx_sin(thing x) {
  return unary_cos(x);
}

static thing unary_sin(thing x) {
  return lift_real_to_real(&sin, &dfdx_sin, x);
}

static thing dfdx_cos(thing x) {
  return binary_minus(make_real(0.0), unary_sin(x));
}

static thing unary_cos(thing x) {
  return lift_real_to_real(&cos, &dfdx_cos, x);
}

static double base_atan(double x1, double x2) {
  return atan2(x1, x2);
}

static thing dfdx1_atan(thing x1, thing x2) {
  return binary_divide(x2,
		       binary_plus(binary_times(x1, x1), binary_times(x2, x2)));
}

static thing dfdx2_atan(thing x1, thing x2) {
  return binary_minus
    (make_real(0.0),
     binary_divide(x1,
		   binary_plus(binary_times(x1, x1), binary_times(x2, x2))));
}

static thing binary_atan(thing x1, thing x2) {
  return lift_real_cross_real_to_real(&base_atan,
				      &dfdx1_atan,
				      &dfdx2_atan,
				      x1,
				      x2);
}

static int base_eq(double x1, double x2) {
  return x1==x2;
}

static thing binary_eq(thing x1, thing x2) {
  return lift_real_cross_real_to_boolean(&base_eq, x1, x2);
}

static int base_lt(double x1, double x2) {
  return x1<x2;
}

static thing binary_lt(thing x1, thing x2) {
  return lift_real_cross_real_to_boolean(&base_lt, x1, x2);
}

static int base_gt(double x1, double x2) {
  return x1>x2;
}

static thing binary_gt(thing x1, thing x2) {
  return lift_real_cross_real_to_boolean(&base_gt, x1, x2);
}

static int base_le(double x1, double x2) {
  return x1<=x2;
}

static thing binary_le(thing x1, thing x2) {
  return lift_real_cross_real_to_boolean(&base_le, x1, x2);
}

static int base_ge(double x1, double x2) {
  return x1>=x2;
}

static thing binary_ge(thing x1, thing x2) {
  return lift_real_cross_real_to_boolean(&base_ge, x1, x2);
}

static int base_zero(double x) {
  return x==0.0;
}

static thing unary_zero(thing x) {
  return lift_real_to_boolean(&base_zero, x);
}

static int base_positive(double x) {
  return x>0.0;
}

static thing unary_positive(thing x) {
  return lift_real_to_boolean(&base_positive, x);
}

static int base_negative(double x) {
  return x<0.0;
}

static thing unary_negative(thing x) {
  return lift_real_to_boolean(&base_negative, x);
}

static thing unary_null(thing x) {
  if (is_null(x)) return true_constant;
  else return false_constant;
}

static thing unary_boolean(thing x) {
  if (is_true(x)||is_false(x)) return true_constant;
  else return false_constant;
}

static thing unary_real(thing x) {
  if (is_number(x)) return true_constant;
  else return false_constant;
}

static thing unary_pair(thing x) {
  if (is_pair(x)) return true_constant;
  else return false_constant;
}

static thing unary_procedure(thing x) {
  if (is_closure(x)) return true_constant;
  else return false_constant;
}

static thing unary_read_real(thing x) {
  if (!is_null(x)) panic("Argument not null");
  double x0;
  scanf("%lf", &x0);
  return make_real(x0);
}

static thing unary_real_coercion(thing x) {
  return x;
}

static thing unary_write_real(thing x) {
  thing x0 = primal_star(x);
  printf("%.18lg\n", as_real(x0));
  return x;
}

static thing binary_expt(thing x1, thing x2) {
  thing x3 = primal_star(x1);
  thing x4 = primal_star(x2);
  return make_real(pow(as_real(x3), as_real(x4)));
}

static thing unary_floor(thing x) {
  thing x0 = primal_star(x);
  return make_real(floor(as_real(x0)));
}

static thing binary_modulo(thing x1, thing x2) {
  thing x3 = primal_star(x1);
  thing x4 = primal_star(x2);
  //\needswork: This doesn't work for doubles that can't be longs.
  return make_real(((long)as_real(x3))%((long)as_real(x4)));
}

static thing unary_increment(thing x) {
  if (!is_real(x)) internal_error();
  return make_real(as_real(x)+1.0);
}

static thing binary_limit_check(thing x1, thing x2) {
  if (!is_real(x1)) internal_error();
  if (!is_real(x2)) internal_error();
  if (as_real(x1)==as_real(x2)) return true_constant;
  else return false_constant;
}

#ifdef NANBOXING

static thing binary_make_checkpoint(thing x1, thing x2) {
  thing object;
  set_checkpoint(&object);
  set_as_checkpoint(&object,
		    (struct checkpoint *)allocate(sizeof(struct checkpoint)));
  as_checkpoint(object)->cache = NULL;
  as_checkpoint(object)->forward = NULL;
  as_checkpoint(object)->continuation = x1;
  as_checkpoint(object)->closure = x2;
  return object;
}

#else

static thing binary_make_checkpoint(thing x1, thing x2) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      thing cache;
      thing continuation;
      thing closure;
    };
  }));
  set_checkpoint(object);
  as_checkpoint(object)->cache = NULL;
  as_checkpoint(object)->continuation = x1;
  as_checkpoint(object)->closure = x2;
  return object;
}

#endif

static void determine_fanout(thing tape) {
  as_tape(tape)->fanout++;
  if (as_tape(tape)->fanout==1) {
    for (unsigned i = 0; i<as_tape(tape)->n; i++) {
      determine_fanout(as_tape(tape)->arguments[i].tape);
    }
  }
}

static void initialize_cotangent(thing tape) {
  as_tape(tape)->cotangent = make_real(0.0);
  as_tape(tape)->fanout--;
  if (as_tape(tape)->fanout==0) {
    for (unsigned i = 0; i<as_tape(tape)->n; i++) {
      initialize_cotangent(as_tape(tape)->arguments[i].tape);
    }
  }
}

static void reverse_phase(thing cotangent, thing tape) {
  as_tape(tape)->cotangent = binary_plus(as_tape(tape)->cotangent, cotangent);
  as_tape(tape)->fanout--;
  if (as_tape(tape)->fanout==0) {
    thing cotangent = as_tape(tape)->cotangent;
    for (unsigned i = 0; i<as_tape(tape)->n; i++) {
      reverse_phase(binary_times(cotangent, as_tape(tape)->arguments[i].factor),
		    as_tape(tape)->arguments[i].tape);
    }
  }
}

static void clear_cache(thing x) {
  if (is_true(x));
  else if (is_false(x));
  else if (is_null(x));
  else if (is_number(x));
  else if (is_pair(x)) {
    if (as_pair(x)->cache!=NULL) {
      as_pair(x)->cache = NULL;
      clear_cache(as_pair(x)->car);
      clear_cache(as_pair(x)->cdr);
    }
  }
  else if (is_closure(x)) {
    if (as_closure(x)->cache!=NULL) {
      as_closure(x)->cache = NULL;
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	clear_cache(as_closure(x)->environment[i]);
      }
    }
  }
  else if (is_checkpoint(x)) {
    if (as_checkpoint(x)->cache!=NULL) {
      as_checkpoint(x)->cache = NULL;
      clear_cache(as_checkpoint(x)->continuation);
      clear_cache(as_checkpoint(x)->closure);
    }
  }
  else internal_error();
}

#ifdef NANBOXING

static thing walk1(thing (*f)(thing), thing x) {
  //\needswork: This is not safe for space for tapes.
  // here I am: There are actually critical sections for all of these. The
  //            recursive call to walk can invalidate CSE of as_*(object).
  //            The calls to allocate can do so as well.
  thing walk(thing x) {
    if (is_true(x)) return x;
    else if (is_false(x)) return x;
    else if (is_null(x)) return x;
    else if (is_number(x)) return (*f)(x);
    else if (is_pair(x)) {
      if (as_pair(x)->cache!=NULL) return as_pair(x)->cache;
      thing object;
      set_pair(&object);
      set_as_pair(&object, (struct pair *)allocate(sizeof(struct pair)));
      as_pair(x)->cache = object;
      as_pair(object)->cache = NULL;
      as_pair(object)->forward = NULL;
      // The recursive walk can trigger gc.
      as_pair(object)->car = make_real(0.0);
      as_pair(object)->cdr = make_real(0.0);
      as_pair(object)->car = walk(as_pair(x)->car);
      as_pair(object)->cdr = walk(as_pair(x)->cdr);
      return object;
    }
    else if (is_closure(x)) {
      if (as_closure(x)->cache!=NULL) return as_closure(x)->cache;
      gc();
      if (debugging1) printf("begin critical %u\n", critical);
      critical++;
      thing object;
      set_closure(&object);
      set_as_closure(&object, (struct closure *)allocate(sizeof(struct {
	thing cache;
	thing forward;
	thing (*function)();
	unsigned n;
	thing environment[as_closure(x)->n];
      })));
      as_closure(x)->cache = object;
      as_closure(object)->cache = NULL;
      as_closure(object)->forward = NULL;
      as_closure(object)->function = as_closure(x)->function;
      as_closure(object)->n = as_closure(x)->n;
      // The recursive walk can trigger gc.
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	as_closure(object)->environment[i] = make_real(0.0);
      }
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	as_closure(object)->environment[i] =
	  walk(as_closure(x)->environment[i]);
      }
      critical--;
      if (debugging1) printf("end critical %u\n", critical);
      return object;
    }
    else if (is_checkpoint(x)) {
      if (as_checkpoint(x)->cache!=NULL) return as_checkpoint(x)->cache;
      thing object;
      set_checkpoint(&object);
      set_as_checkpoint(&object,
			(struct checkpoint *)
			allocate(sizeof(struct checkpoint)));
      as_checkpoint(x)->cache = object;
      as_checkpoint(object)->cache = NULL;
      as_checkpoint(object)->forward = NULL;
      // The recursive walk can trigger gc.
      as_checkpoint(object)->continuation = make_real(0.0);
      as_checkpoint(object)->closure = make_real(0.0);
      as_checkpoint(object)->continuation =
	walk(as_checkpoint(x)->continuation);
      as_checkpoint(object)->closure = walk(as_checkpoint(x)->closure);
      return object;
    }
    else internal_error();
  }
  thing object = walk(x);
  clear_cache(x);
  return object;
}

static thing walk2(thing (*f)(thing, thing), thing x, thing x_prime) {
  //\needswork: This is not safe for space for tapes.
  // here I am: There are actually critical sections for all of these. The
  //            recursive call to walk can invalidate CSE of as_*(object).
  //            The calls to allocate can do so as well.
  thing walk(thing x, thing x_prime) {
    if (is_true(x)&&is_true(x_prime)) return x;
    else if (is_false(x)&&is_false(x_prime)) return x;
    else if (is_null(x)&&is_null(x_prime)) return x;
    else if (is_number(x)&&is_number(x_prime)) return (*f)(x, x_prime);
    else if (is_pair(x)&&is_pair(x_prime)) {
      if (as_pair(x)->cache!=NULL) {
	if (as_pair(x_prime)->cache!=NULL) {
	  if (as_pair(x)->cache!=as_pair(x_prime)->cache) internal_error();
	  return as_pair(x)->cache;
	}
	else internal_error();
      }
      else if (as_pair(x_prime)->cache!=NULL) internal_error();
      thing object;
      set_pair(&object);
      set_as_pair(&object, (struct pair *)allocate(sizeof(struct pair)));
      as_pair(x)->cache = object;
      as_pair(x_prime)->cache = object;
      as_pair(object)->cache = NULL;
      as_pair(object)->forward = NULL;
      // The recursive walk can trigger gc.
      as_pair(object)->car = make_real(0.0);
      as_pair(object)->cdr = make_real(0.0);
      as_pair(object)->car = walk(as_pair(x)->car, as_pair(x_prime)->car);
      as_pair(object)->cdr = walk(as_pair(x)->cdr, as_pair(x_prime)->cdr);
      return object;
    }
    else if (is_closure(x)&&is_closure(x_prime)&&
	     // We check function here but not in walk2b.
	     as_closure(x)->function==as_closure(x_prime)->function) {
      if (as_closure(x)->cache!=NULL) {
	if (as_closure(x_prime)->cache!=NULL) {
	  if (as_closure(x)->cache!=as_closure(x_prime)->cache) {
	    internal_error();
	  }
	  return as_closure(x)->cache;
	}
	else internal_error();
      }
      else if (as_closure(x_prime)->cache!=NULL) internal_error();
      gc();
      if (debugging1) printf("begin critical %u\n", critical);
      critical++;
      thing object;
      set_closure(&object);
      set_as_closure(&object, (struct closure *)allocate(sizeof(struct {
	thing cache;
	thing forward;
	thing (*function)();
	unsigned n;
	thing environment[as_closure(x)->n];
      })));
      as_closure(x)->cache = object;
      as_closure(x_prime)->cache = object;
      as_closure(object)->cache = NULL;
      as_closure(object)->forward = NULL;
      as_closure(object)->function = as_closure(x)->function;
      as_closure(object)->n = as_closure(x)->n;
      // The recursive walk can trigger gc.
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	as_closure(object)->environment[i] = make_real(0.0);
      }
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	as_closure(object)->environment[i] =
	  walk(as_closure(x)->environment[i],
	       as_closure(x_prime)->environment[i]);
      }
      critical--;
      if (debugging1) printf("end critical %u\n", critical);
      return object;
    }
    else if (is_checkpoint(x)&&is_checkpoint(x_prime)) {
      if (as_checkpoint(x)->cache!=NULL) {
	if (as_checkpoint(x_prime)->cache!=NULL) {
	  if (as_checkpoint(x)->cache!=as_checkpoint(x_prime)->cache) {
	    internal_error();
	  }
	  return as_checkpoint(x)->cache;
	}
	else internal_error();
      }
      else if (as_checkpoint(x_prime)->cache!=NULL) internal_error();
      thing object;
      set_checkpoint(&object);
      set_as_checkpoint(&object,
			(struct checkpoint *)
			allocate(sizeof(struct checkpoint)));
      as_checkpoint(x)->cache = object;
      as_checkpoint(x_prime)->cache = object;
      as_checkpoint(object)->cache = NULL;
      as_checkpoint(object)->forward = NULL;
      // The recursive walk can trigger gc.
      as_checkpoint(object)->continuation = make_real(0.0);
      as_checkpoint(object)->closure = make_real(0.0);
      as_checkpoint(object)->continuation =
	walk(as_checkpoint(x)->continuation,
	     as_checkpoint(x_prime)->continuation);
      as_checkpoint(object)->closure =
	walk(as_checkpoint(x)->closure, as_checkpoint(x_prime)->closure);
      return object;
    }
    else {
      externalize(x);
      printf("\n");
      externalize(x_prime);
      printf("\n");
      panic("Values don't conform");
    }
  }
  thing object = walk(x, x_prime);
  clear_cache(x);
  clear_cache(x_prime);
  return object;
}

static void walk1b(void (*f)(thing), thing x) {
  void walk(thing x) {
    if (is_true(x));
    else if (is_false(x));
    else if (is_null(x));
    else if (is_number(x)) (*f)(x);
    else if (is_pair(x)) {
      if (as_pair(x)->cache!=NULL&&as_pair(x)->cache!=x) internal_error();
      if (as_pair(x)->cache!=NULL) return;
      as_pair(x)->cache = x;
      walk(as_pair(x)->car);
      walk(as_pair(x)->cdr);
    }
    else if (is_closure(x)) {
      if (as_closure(x)->cache!=NULL&&as_closure(x)->cache!=x) internal_error();
      if (as_closure(x)->cache!=NULL) return;
      as_closure(x)->cache = x;
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	walk(as_closure(x)->environment[i]);
      }
    }
    else if (is_checkpoint(x)) {
      if (as_checkpoint(x)->cache!=NULL&&as_checkpoint(x)->cache!=x) {
	internal_error();
      }
      if (as_checkpoint(x)->cache!=NULL) return;
      as_checkpoint(x)->cache = x;
      walk(as_checkpoint(x)->continuation);
      walk(as_checkpoint(x)->closure);
    }
    else internal_error();
  }
  walk(x);
  clear_cache(x);
}

static void walk2b(void (*f)(thing, thing), thing x, thing x_prime) {
  void walk(thing x, thing x_prime) {
    if (is_true(x)&&is_true(x_prime));
    else if (is_false(x)&&is_false(x_prime));
    else if (is_null(x)&&is_null(x_prime));
    else if (is_number(x)&&is_number(x_prime)) (*f)(x, x_prime);
    else if (is_pair(x)&&is_pair(x_prime)) {
      if (as_pair(x)->cache!=NULL&&as_pair(x)->cache!=x) internal_error();
      if (as_pair(x_prime)->cache!=NULL&&as_pair(x_prime)->cache!=x_prime) {
	internal_error();
      }
      if (as_pair(x)->cache!=NULL) {
	if (as_pair(x_prime)->cache!=NULL) return;
	internal_error();
      }
      else if (as_pair(x_prime)->cache!=NULL) internal_error();
      as_pair(x)->cache = x;
      as_pair(x_prime)->cache = x_prime;
      walk(as_pair(x)->car, as_pair(x_prime)->car);
      walk(as_pair(x)->cdr, as_pair(x_prime)->cdr);
    }
    // We don't check function here but do in walk2.
    else if (is_closure(x)&&is_closure(x_prime)) {
      if (as_closure(x)->cache!=NULL&&as_closure(x)->cache!=x) internal_error();
      if (as_closure(x_prime)->cache!=NULL&&
	  as_closure(x_prime)->cache!=x_prime) {
	internal_error();
      }
      if (as_closure(x)->cache!=NULL) {
	if (as_closure(x_prime)->cache!=NULL) return;
	internal_error();
      }
      else if (as_closure(x_prime)->cache!=NULL) internal_error();
      as_closure(x)->cache = x;
      as_closure(x_prime)->cache = x_prime;
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	walk(as_closure(x)->environment[i],
	     as_closure(x_prime)->environment[i]);
      }
    }
    else if (is_checkpoint(x)&&is_checkpoint(x_prime)) {
      if (as_checkpoint(x)->cache!=NULL&&as_checkpoint(x)->cache!=x) {
	internal_error();
      }
      if (as_checkpoint(x_prime)->cache!=NULL&&
	  as_checkpoint(x_prime)->cache!=x_prime) {
	internal_error();
      }
      if (as_checkpoint(x)->cache!=NULL) {
	if (as_checkpoint(x_prime)->cache!=NULL) return;
	internal_error();
      }
      else if (as_checkpoint(x_prime)->cache!=NULL) internal_error();
      as_checkpoint(x)->cache = x;
      as_checkpoint(x_prime)->cache = x_prime;
      walk(as_checkpoint(x)->continuation,
	   as_checkpoint(x_prime)->continuation);
      walk(as_checkpoint(x)->closure, as_checkpoint(x_prime)->closure);
    }
    else {
      externalize(x);
      printf("\n");
      externalize(x_prime);
      printf("\n");
      panic("Values don't conform");
    }
  }
  walk(x, x_prime);
  clear_cache(x);
  clear_cache(x_prime);
}

#else

static thing walk1(thing (*f)(thing), thing x) {
  //\needswork: This is not safe for space for tapes.
  thing walk(thing x) {
    if (is_true(x)) return x;
    else if (is_false(x)) return x;
    else if (is_null(x)) return x;
    else if (is_number(x)) return (*f)(x);
    else if (is_pair(x)) {
      if (as_pair(x)->cache!=NULL) return as_pair(x)->cache;
      thing object = (thing)GC_malloc(sizeof(struct {
	enum tag tag;
	struct {
	  thing cache;
	  thing car;
	  thing cdr;
	};
      }));
      set_pair(object);
      as_pair(x)->cache = object;
      as_pair(object)->cache = NULL;
      as_pair(object)->car = walk(as_pair(x)->car);
      as_pair(object)->cdr = walk(as_pair(x)->cdr);
      return object;
    }
    else if (is_closure(x)) {
      if (as_closure(x)->cache!=NULL) return as_closure(x)->cache;
      thing object = (thing)GC_malloc(sizeof(struct {
	enum tag tag;
	struct {
	  thing cache;
	  thing (*function)();
	  unsigned n;
	  thing environment[as_closure(x)->n];
	};
      }));
      set_closure(object);
      as_closure(x)->cache = object;
      as_closure(object)->cache = NULL;
      as_closure(object)->function = as_closure(x)->function;
      as_closure(object)->n = as_closure(x)->n;
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	as_closure(object)->environment[i] =
	  walk(as_closure(x)->environment[i]);
      }
      return object;
    }
    else if (is_checkpoint(x)) {
      if (as_checkpoint(x)->cache!=NULL) return as_checkpoint(x)->cache;
      thing object = (thing)GC_malloc(sizeof(struct {
	enum tag tag;
	struct {
	  thing cache;
	  thing continuation;
	  thing closure;
	};
      }));
      set_checkpoint(object);
      as_checkpoint(x)->cache = object;
      as_checkpoint(object)->cache = NULL;
      as_checkpoint(object)->continuation =
	walk(as_checkpoint(x)->continuation);
      as_checkpoint(object)->closure = walk(as_checkpoint(x)->closure);
      return object;
    }
    else internal_error();
  }
  thing object = walk(x);
  clear_cache(x);
  return object;
}

static thing walk2(thing (*f)(thing, thing), thing x, thing x_prime) {
  //\needswork: This is not safe for space for tapes.
  thing walk(thing x, thing x_prime) {
    if (is_true(x)&&is_true(x_prime)) return x;
    else if (is_false(x)&&is_false(x_prime)) return x;
    else if (is_null(x)&&is_null(x_prime)) return x;
    else if (is_number(x)&&is_number(x_prime)) return (*f)(x, x_prime);
    else if (is_pair(x)&&is_pair(x_prime)) {
      if (as_pair(x)->cache!=NULL) {
	if (as_pair(x_prime)->cache!=NULL) {
	  if (as_pair(x)->cache!=as_pair(x_prime)->cache) internal_error();
	  return as_pair(x)->cache;
	}
	else internal_error();
      }
      else if (as_pair(x_prime)->cache!=NULL) internal_error();
      thing object = (thing)GC_malloc(sizeof(struct {
	enum tag tag;
	struct {
	  thing cache;
	  thing car;
	  thing cdr;
	};
      }));
      set_pair(object);
      as_pair(x)->cache = object;
      as_pair(x_prime)->cache = object;
      as_pair(object)->cache = NULL;
      as_pair(object)->car = walk(as_pair(x)->car, as_pair(x_prime)->car);
      as_pair(object)->cdr = walk(as_pair(x)->cdr, as_pair(x_prime)->cdr);
      return object;
    }
    else if (is_closure(x)&&is_closure(x_prime)&&
	     // We check function here but not in walk2b.
	     as_closure(x)->function==as_closure(x_prime)->function) {
      if (as_closure(x)->cache!=NULL) {
	if (as_closure(x_prime)->cache!=NULL) {
	  if (as_closure(x)->cache!=as_closure(x_prime)->cache) {
	    internal_error();
	  }
	  return as_closure(x)->cache;
	}
	else internal_error();
      }
      else if (as_closure(x_prime)->cache!=NULL) internal_error();
      thing object = (thing)GC_malloc(sizeof(struct {
	enum tag tag;
	struct {
	  thing cache;
	  thing (*function)();
	  unsigned n;
	  thing environment[as_closure(x)->n];
	};
      }));
      set_closure(object);
      as_closure(x)->cache = object;
      as_closure(x_prime)->cache = object;
      as_closure(object)->cache = NULL;
      as_closure(object)->function = as_closure(x)->function;
      as_closure(object)->n = as_closure(x)->n;
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	as_closure(object)->environment[i] =
	  walk(as_closure(x)->environment[i],
	       as_closure(x_prime)->environment[i]);
      }
      return object;
    }
    else if (is_checkpoint(x)&&is_checkpoint(x_prime)) {
      if (as_checkpoint(x)->cache!=NULL) {
	if (as_checkpoint(x_prime)->cache!=NULL) {
	  if (as_checkpoint(x)->cache!=as_checkpoint(x_prime)->cache) {
	    internal_error();
	  }
	  return as_checkpoint(x)->cache;
	}
	else internal_error();
      }
      else if (as_checkpoint(x_prime)->cache!=NULL) internal_error();
      thing object = (thing)GC_malloc(sizeof(struct {
	enum tag tag;
	struct {
	  thing cache;
	  thing continuation;
	  thing closure;
	};
      }));
      set_checkpoint(object);
      as_checkpoint(x)->cache = object;
      as_checkpoint(x_prime)->cache = object;
      as_checkpoint(object)->cache = NULL;
      as_checkpoint(object)->continuation =
	walk(as_checkpoint(x)->continuation,
	     as_checkpoint(x_prime)->continuation);
      as_checkpoint(object)->closure =
	walk(as_checkpoint(x)->closure, as_checkpoint(x_prime)->closure);
      return object;
    }
    else {
      externalize(x);
      printf("\n");
      externalize(x_prime);
      printf("\n");
      panic("Values don't conform");
    }
  }
  thing object = walk(x, x_prime);
  clear_cache(x);
  clear_cache(x_prime);
  return object;
}

static void walk1b(void (*f)(thing), thing x) {
  void walk(thing x) {
    if (is_true(x));
    else if (is_false(x));
    else if (is_null(x));
    else if (is_number(x)) (*f)(x);
    else if (is_pair(x)) {
      if (as_pair(x)->cache!=NULL&&as_pair(x)->cache!=x) internal_error();
      if (as_pair(x)->cache!=NULL) return;
      as_pair(x)->cache = x;
      walk(as_pair(x)->car);
      walk(as_pair(x)->cdr);
    }
    else if (is_closure(x)) {
      if (as_closure(x)->cache!=NULL&&as_closure(x)->cache!=x) internal_error();
      if (as_closure(x)->cache!=NULL) return;
      as_closure(x)->cache = x;
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	walk(as_closure(x)->environment[i]);
      }
    }
    else if (is_checkpoint(x)) {
      if (as_checkpoint(x)->cache!=NULL&&as_checkpoint(x)->cache!=x) {
	internal_error();
      }
      if (as_checkpoint(x)->cache!=NULL) return;
      as_checkpoint(x)->cache = x;
      walk(as_checkpoint(x)->continuation);
      walk(as_checkpoint(x)->closure);
    }
    else internal_error();
  }
  walk(x);
  clear_cache(x);
}

static void walk2b(void (*f)(thing, thing), thing x, thing x_prime) {
  void walk(thing x, thing x_prime) {
    if (is_true(x)&&is_true(x_prime));
    else if (is_false(x)&&is_false(x_prime));
    else if (is_null(x)&&is_null(x_prime));
    else if (is_number(x)&&is_number(x_prime)) (*f)(x, x_prime);
    else if (is_pair(x)&&is_pair(x_prime)) {
      if (as_pair(x)->cache!=NULL&&as_pair(x)->cache!=x) internal_error();
      if (as_pair(x_prime)->cache!=NULL&&as_pair(x_prime)->cache!=x_prime) {
	internal_error();
      }
      if (as_pair(x)->cache!=NULL) {
	if (as_pair(x_prime)->cache!=NULL) return;
	internal_error();
      }
      else if (as_pair(x_prime)->cache!=NULL) internal_error();
      as_pair(x)->cache = x;
      as_pair(x_prime)->cache = x_prime;
      walk(as_pair(x)->car, as_pair(x_prime)->car);
      walk(as_pair(x)->cdr, as_pair(x_prime)->cdr);
    }
    // We don't check function here but do in walk2.
    else if (is_closure(x)&&is_closure(x_prime)) {
      if (as_closure(x)->cache!=NULL&&as_closure(x)->cache!=x) internal_error();
      if (as_closure(x_prime)->cache!=NULL&&
	  as_closure(x_prime)->cache!=x_prime) {
	internal_error();
      }
      if (as_closure(x)->cache!=NULL) {
	if (as_closure(x_prime)->cache!=NULL) return;
	internal_error();
      }
      else if (as_closure(x_prime)->cache!=NULL) internal_error();
      as_closure(x)->cache = x;
      as_closure(x_prime)->cache = x_prime;
      for (unsigned i = 0; i<as_closure(x)->n; i++) {
	walk(as_closure(x)->environment[i],
	     as_closure(x_prime)->environment[i]);
      }
    }
    else if (is_checkpoint(x)&&is_checkpoint(x_prime)) {
      if (as_checkpoint(x)->cache!=NULL&&as_checkpoint(x)->cache!=x) {
	internal_error();
      }
      if (as_checkpoint(x_prime)->cache!=NULL&&
	  as_checkpoint(x_prime)->cache!=x_prime) {
	internal_error();
      }
      if (as_checkpoint(x)->cache!=NULL) {
	if (as_checkpoint(x_prime)->cache!=NULL) return;
	internal_error();
      }
      else if (as_checkpoint(x_prime)->cache!=NULL) internal_error();
      as_checkpoint(x)->cache = x;
      as_checkpoint(x_prime)->cache = x_prime;
      walk(as_checkpoint(x)->continuation,
	   as_checkpoint(x_prime)->continuation);
      walk(as_checkpoint(x)->closure, as_checkpoint(x_prime)->closure);
    }
    else {
      externalize(x);
      printf("\n");
      externalize(x_prime);
      printf("\n");
      panic("Values don't conform");
    }
  }
  walk(x, x_prime);
  clear_cache(x);
  clear_cache(x_prime);
}

#endif

static thing forward_mode
(thing (*map_independent)
 (thing (*)(thing, thing), thing, thing),
 thing (*map_dependent)(thing (*)(thing), thing),
 thing (*f)(thing),
 thing x,
 thing x_tangent) {
  thing lambda0(thing x, thing x_tangent) {
    return make_dual_number(epsilon, x, x_tangent);
  }
  thing lambda1(thing y_forward) {
    if (is_dual_number(y_forward)&&
	!(as_dual_number(y_forward)->epsilon<epsilon)) {
      return as_dual_number(y_forward)->primal;
    }
    else return y_forward;
  }
  thing lambda2(thing y_forward) {
    if (is_dual_number(y_forward)&&
	!(as_dual_number(y_forward)->epsilon<epsilon)) {
      return as_dual_number(y_forward)->tangent;
    }
    else return make_real(0.0);
  }
  epsilon++;
  thing y_forward = (*f)((*map_independent)(&lambda0, x, x_tangent));
  thing y = (*map_dependent)(&lambda1, y_forward);
  thing y_tangent = (*map_dependent)(&lambda2, y_forward);
  epsilon--;
  return cons(y, y_tangent);
}

static thing reverse_mode
(thing (*map_independent)(thing (*)(thing), thing),
 thing (*map_dependent)(thing (*)(thing), thing),
 void (*for_each_dependent1)(void (*)(thing), thing),
 void (*for_each_dependent2)(void (*)(thing, thing), thing, thing),
 thing (*f)(thing),
 thing x,
 thing y_cotangent) {
  void lambda0(thing y_reverse) {
    if (is_tape(y_reverse)&&!(as_tape(y_reverse)->epsilon<epsilon)) {
      determine_fanout(y_reverse);
    }
  }
  void lambda1(thing y_reverse) {
    if (is_tape(y_reverse)&&!(as_tape(y_reverse)->epsilon<epsilon)) {
      initialize_cotangent(y_reverse);
    }
  }
  void lambda2(thing y_reverse, thing y_cotangent) {
    if (is_tape(y_reverse)&&!(as_tape(y_reverse)->epsilon<epsilon)) {
      reverse_phase(y_cotangent, y_reverse);
    }
  }
  thing lambda3(thing x_reverse) {
    if (!is_tape(x_reverse)) internal_error();
    return as_tape(x_reverse)->cotangent;
  }
  thing lambda4(thing y_reverse) {
    if (is_tape(y_reverse)&&!(as_tape(y_reverse)->epsilon<epsilon)) {
      return as_tape(y_reverse)->primal;
    }
    else return y_reverse;
  }
  epsilon++;
  thing x_reverse = (*map_independent)(&tapify, x);
  thing y_reverse = (*f)(x_reverse);
  for_each_dependent1(&lambda0, y_reverse);
  for_each_dependent1(&lambda1, y_reverse);
  for_each_dependent1(&lambda0, y_reverse);
  for_each_dependent2(&lambda2, y_reverse, y_cotangent);
  thing x_cotangent = (*map_independent)(&lambda3, x_reverse);
  thing y = (*map_dependent)(&lambda4, y_reverse);
  epsilon--;
  return cons(y, x_cotangent);
}

static thing lambda_expression_that_returns_x
(thing target, thing count, thing limit, thing argument) {
  return argument;
}

static thing lambda_expression_that_returns_n
(thing target, thing count, thing limit, thing argument) {
  return count;
}

static thing lambda_expression_that_resumes
(thing target, thing continuation, thing count, thing limit, thing argument) {
  if (!is_checkpoint(argument)) internal_error();
  return converted_apply(as_checkpoint(argument)->closure,
			 as_checkpoint(argument)->continuation,
			 make_real(0.0),
			 limit,
			 null_constant);
}

#ifdef NANBOXING

static thing lambda_expression_for_checkpoint
(thing target, thing continuation, thing count, thing limit, thing argument) {
  thing value1 = as_closure(target)->environment[0];
  thing value2 = argument;
  thing value3 = primal_star(as_closure(target)->environment[1]);
  if (!is_real(value3)) internal_error();
  if (!is_real(limit)) internal_error();
  if (is_closure(continuation)&&
      as_closure(continuation)==as_closure(continuation_that_returns_n)) {
    internal_error();
  }
  if (is_real(limit)&&as_real(limit)==HUGE_VAL) {
    // We used to assert-that-is-checkpoint on the result. But this
    // assertion foils the tail call. And it also breaks treeverse.
    return converted_apply
      (value1, continuation, make_real(0.0), value3, value2);
  }
  else {
    thing checkpoint =
      converted_apply(value1, continuation, make_real(0.0), limit, value2);
    if (!is_checkpoint(checkpoint)) internal_error();
    thing object;
    set_checkpoint(&object);
    set_as_checkpoint(&object,
		      (struct checkpoint *)
		      allocate(sizeof(struct checkpoint)));
    as_checkpoint(object)->cache = NULL;
    as_checkpoint(object)->forward = NULL;
    as_checkpoint(object)->continuation =
      as_checkpoint(checkpoint)->continuation;
    as_checkpoint(object)->closure =
      make_closure_for_checkpoint(as_checkpoint(checkpoint)->closure,
				  ((unsigned long)as_real(value3))-
				  ((unsigned long)as_real(limit)));
    return object;
  }
}

// paper: f l
static thing make_closure_for_checkpoint(thing value1, unsigned limit) {
  thing object;
  set_closure(&object);
  set_as_closure(&object, (struct closure *)allocate(sizeof(struct {
    thing cache;
    thing forward;
    thing (*function)();
    unsigned n;
    thing environment[2];
  })));
  as_closure(object)->cache = NULL;
  as_closure(object)->forward = NULL;
  as_closure(object)->function = &lambda_expression_for_checkpoint;
  as_closure(object)->n = 2;
  as_closure(object)->environment[0] = value1;
  as_closure(object)->environment[1] = make_real(limit);
  return object;
}

#else

static thing lambda_expression_for_checkpoint
(thing target, thing continuation, thing count, thing limit, thing argument) {
  thing value1 = as_closure(target)->environment[0];
  thing value2 = argument;
  thing value3 = primal_star(as_closure(target)->environment[1]);
  if (!is_real(value3)) internal_error();
  if (!is_real(limit)) internal_error();
  if (is_closure(continuation)&&
      as_closure(continuation)==as_closure(continuation_that_returns_n)) {
    internal_error();
  }
  if (is_real(limit)&&as_real(limit)==HUGE_VAL) {
    // We used to assert-that-is-checkpoint on the result. But this
    // assertion foils the tail call. And it also breaks treeverse.
    return converted_apply
      (value1, continuation, make_real(0.0), value3, value2);
  }
  else {
    thing checkpoint =
      converted_apply(value1, continuation, make_real(0.0), limit, value2);
    if (!is_checkpoint(checkpoint)) internal_error();
    thing object = (thing)GC_malloc(sizeof(struct {
      enum tag tag;
      struct {
	thing cache;
	thing continuation;
	thing closure;
      };
    }));
    set_checkpoint(object);
    as_checkpoint(object)->cache = NULL;
    as_checkpoint(object)->continuation =
      as_checkpoint(checkpoint)->continuation;
    as_checkpoint(object)->closure =
      make_closure_for_checkpoint(as_checkpoint(checkpoint)->closure,
				  ((unsigned long)as_real(value3))-
				  ((unsigned long)as_real(limit)));
    return object;
  }
}

// paper: f l
static thing make_closure_for_checkpoint(thing value1, unsigned long limit) {
  thing object = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      thing cache;
      thing (*function)();
      unsigned n;
      thing environment[2];
    };
  }));
  set_closure(object);
  as_closure(object)->cache = NULL;
  as_closure(object)->function = &lambda_expression_for_checkpoint;
  as_closure(object)->n = 2;
  as_closure(object)->environment[0] = value1;
  as_closure(object)->environment[1] = make_real(limit);
  return object;
}

#endif

// paper: v
static thing ternary_jstar(thing x1, thing x2, thing x3) {
  thing lambda0(thing x) {
    return converted_apply(x1,
			   continuation_that_returns_x,
			   make_real(0.0),
			   make_real(HUGE_VAL),
			   x);
  }
  return forward_mode(&walk2, &walk1, &lambda0, x2, x3);
}

// paper: v
static thing ternary_starj(thing x1, thing x2, thing x3) {
  thing lambda0(thing x) {
    return converted_apply(x1,
			   continuation_that_returns_x,
			   make_real(0.0),
			   make_real(HUGE_VAL),
			   x);
  }
  return reverse_mode(&walk1, &walk1, &walk1b, &walk2b, &lambda0, x2, x3);
}

// paper: f x
static unsigned long primops(thing value1, thing value2) {
  thing result = converted_apply(value1,
				 continuation_that_returns_n,
				 make_real(0.0),
				 make_real(HUGE_VAL),
				 value2);
  if (is_checkpoint(result)) internal_error(); //debugging
  else if (is_real(result)) return (unsigned long)as_real(result);
  else internal_error();	//debugging
}

// paper: f x l
static thing checkpoint(thing value1, thing value2, thing value3) {
  thing result = converted_apply(value1,
				 continuation_that_returns_x,
				 make_real(0.0),
				 value3,
				 value2);
  if (!is_checkpoint(result)) internal_error();
  return result;
}

// paper: f x y_grave
static thing ternary_checkpoint_starj(thing value1, thing value2, thing value3)
{
  thing loop(thing value1,
	     thing value2,
	     thing value3,
	     unsigned long l) {
    // 0. (y,x`)=*j(f,x,y`)
    if (l<=base_case_duration) return ternary_starj(value1, value2, value3);
    else {
      // 2. c=checkpoint(f,x,n)
      thing u = checkpoint(value1, value2, make_real(l/2));
      // 3. (y,u`)=checkpoint-*j(\u.resume(u),u,y`)
      thing y_u_cotangent = loop(closure_that_resumes, u, value3, l-l/2);
      if (!is_pair(y_u_cotangent)) internal_error();
      // 4. (u,x`)=checkpoint-*j(\x.checkpoint(f,x,n),x,u`)
      thing u_x_cotangent =
	loop(make_closure_for_checkpoint(value1, l/2),
	     value2,
	     as_pair(y_u_cotangent)->cdr,
	     l/2);
      if (!is_pair(u_x_cotangent)) internal_error();
      return cons(as_pair(y_u_cotangent)->car,
		  as_pair(u_x_cotangent)->cdr);
    }
  }
  // 1. l=primops(f,x)
  return loop(value1, value2, value3, primops(value1, value2));
}

static thing ternary_binary_checkpoint_starj(thing f, thing x, thing y_grave) {
  thing binary(thing f, thing x, thing y_grave, unsigned long delta,
	       unsigned long tau, unsigned long phi) {
    if (phi<=base_case_duration||delta==0||tau==0) {
      return ternary_starj(f, x, y_grave);
    }
    else {
      unsigned long kappa = (*mid)(delta, tau, 0, phi);
      thing u = checkpoint(f, x, make_real(kappa));
      thing y_u_cotangent = binary(closure_that_resumes, u, y_grave, delta-1,
				   tau, phi-kappa);
      thing u_x_cotangent = binary(make_closure_for_checkpoint(f, kappa), x,
				   as_pair(y_u_cotangent)->cdr, delta, tau-1,
				   kappa);
      return cons(as_pair(y_u_cotangent)->car,
		  as_pair(u_x_cotangent)->cdr);
    }
  }
  return binary(f, x, y_grave, d, t, primops(f, x));
}

static thing treeverse(thing f, thing x, thing y_grave, unsigned long delta,
		       unsigned long tau, unsigned long beta,
		       unsigned long sigma, unsigned long phi) {
  if (sigma>beta) {
    thing u = checkpoint(f, x, make_real(sigma-beta));
    return first(closure_that_resumes, u, y_grave, delta-1, tau, beta, sigma,
		 phi);
  }
  else return first(f, x, y_grave, delta, tau, beta, sigma, phi);
}

static thing first(thing f, thing x, thing y_grave, unsigned long delta,
		   unsigned long tau, unsigned long beta, unsigned long sigma,
		   unsigned long phi) {
  if (phi-sigma>base_case_duration&&delta!=0&&tau!=0) {
    unsigned long kappa = (*mid)(delta, tau, sigma, phi);
    thing y_u_cotangent = treeverse(f, x, y_grave, delta, tau, sigma, kappa,
				    phi);
    return rest(f, x, as_pair(y_u_cotangent)->cdr,
		as_pair(y_u_cotangent)->car, delta, tau-1, beta, sigma,
		kappa);
  }
  else return ternary_starj(make_closure_for_checkpoint(f, phi-sigma),
			    x, y_grave);
}

static thing rest(thing f, thing x, thing y_grave, thing y, unsigned long delta,
		  unsigned long tau, unsigned long beta, unsigned long sigma,
		  unsigned long phi) {
  if (phi-sigma>base_case_duration&&delta!=0&&tau!=0) {
    unsigned long kappa = (*mid)(delta, tau, sigma, phi);
    thing y_u_cotangent = treeverse(f, x, y_grave, delta, tau, sigma, kappa,
				    phi);
    return rest(f, x, as_pair(y_u_cotangent)->cdr, y, delta, tau-1, beta,
		sigma, kappa);
  }
  else {
    thing y_u_cotangent =
      ternary_starj(make_closure_for_checkpoint(f, phi-sigma), x, y_grave);
    return cons(y, as_pair(y_u_cotangent)->cdr);
  }
}

static thing ternary_treeverse_checkpoint_starj(thing f, thing x, thing y_grave)
{
  return treeverse(f, x, y_grave, d, t, 0, 0, primops(f, x));
}

unsigned long bisection_mid(unsigned long delta, unsigned long tau,
			    unsigned long sigma, unsigned long phi) {
  return (unsigned long)ceil(((double)(sigma+phi))/2);
}

unsigned long binomial_mid(unsigned long delta, unsigned long tau,
			   unsigned long sigma, unsigned long phi) {
    return (unsigned long)ceil(((double)(delta*sigma+tau*phi))/(delta+tau));
}

unsigned long choose(unsigned long n, unsigned long r) {
  unsigned long loop (unsigned long i, unsigned long c) {
    if (i==n-r) {
      unsigned long loop(unsigned long i, unsigned long c) {
	if (i==0) return c;
	else return loop(i-1, c/i);
      }
      return loop(r, c);
    }
    else return loop(i-1, i*c);
  }
  return loop(n, 1);
}

unsigned long eta(unsigned long d, unsigned long t) {
  return choose(d+t, t);
}

unsigned long fixed_d(unsigned long d, unsigned long base_case_duration,
		      unsigned long l) {
  unsigned long loop(unsigned long t) {
    if (eta(d, t)>l/base_case_duration) return t-1;
    else return loop(t+1);
  }
  return loop(0);
}

unsigned long fixed_t(unsigned long d, unsigned long base_case_duration,
		      unsigned long l) {
  unsigned long loop(unsigned long d) {
    if (eta(d, t)>l/base_case_duration) return d-1;
    else return loop(d+1);
  }
  return loop(0);
}

unsigned long fixed_base_case_duration(unsigned long base_case_duration,
				       unsigned long l) {
  unsigned long loop(unsigned long d_t) {
    if (eta(d_t, d_t)>l/base_case_duration) return d_t-1;
    else return loop(d_t+1);
  }
  return loop(0);
}

#ifdef NANBOXING

static void initialize_constants(void) {
  mid = &bisection_mid;
  set_true(&true_constant);
  set_false(&false_constant);
  set_null(&null_constant);
  set_closure(&continuation_that_returns_x);
  set_as_closure(&continuation_that_returns_x,
		 (struct closure *)allocate(sizeof(struct {
		   thing cache;
		   thing forward;
		   thing (*function)();
		   unsigned n;
		   thing environment[0];
		 })));
  as_closure(continuation_that_returns_x)->cache = NULL;
  as_closure(continuation_that_returns_x)->forward = NULL;
  as_closure(continuation_that_returns_x)->function =
    &lambda_expression_that_returns_x;
  as_closure(continuation_that_returns_x)->n = 0;
  set_closure(&continuation_that_returns_n);
  set_as_closure(&continuation_that_returns_n,
		 (struct closure *)allocate(sizeof(struct {
		   thing cache;
		   thing forward;
		   thing (*function)();
		   unsigned n;
		   thing environment[0];
		 })));
  as_closure(continuation_that_returns_n)->cache = NULL;
  as_closure(continuation_that_returns_n)->forward = NULL;
  as_closure(continuation_that_returns_n)->function =
    &lambda_expression_that_returns_n;
  as_closure(continuation_that_returns_n)->n = 0;
  set_closure(&closure_that_resumes);
  set_as_closure(&closure_that_resumes,
		 (struct closure *)allocate(sizeof(struct {
		   thing cache;
		   thing forward;
		   thing (*function)();
		   unsigned n;
		   thing environment[0];
		 })));
  as_closure(closure_that_resumes)->cache = NULL;
  as_closure(closure_that_resumes)->forward = NULL;
  as_closure(closure_that_resumes)->function = &lambda_expression_that_resumes;
  as_closure(closure_that_resumes)->n = 0;
}

#else

static void initialize_constants(void) {
  mid = &bisection_mid;
  true_constant = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
  }));
  set_true(true_constant);
  false_constant = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
  }));
  set_false(false_constant);
  null_constant = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
  }));
  set_null(null_constant);
  continuation_that_returns_x = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      thing cache;
      thing (*function)();
      unsigned n;
      thing environment[0];
    };
  }));
  set_closure(continuation_that_returns_x);
  as_closure(continuation_that_returns_x)->cache = NULL;
  as_closure(continuation_that_returns_x)->function =
    &lambda_expression_that_returns_x;
  as_closure(continuation_that_returns_x)->n = 0;
  continuation_that_returns_n = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      thing cache;
      thing (*function)();
      unsigned n;
      thing environment[0];
    };
  }));
  set_closure(continuation_that_returns_n);
  as_closure(continuation_that_returns_n)->cache = NULL;
  as_closure(continuation_that_returns_n)->function =
    &lambda_expression_that_returns_n;
  as_closure(continuation_that_returns_n)->n = 0;
  closure_that_resumes = (thing)GC_malloc(sizeof(struct {
    enum tag tag;
    struct {
      thing cache;
      thing (*function)();
      unsigned n;
      thing environment[0];
    };
  }));
  set_closure(closure_that_resumes);
  as_closure(closure_that_resumes)->cache = NULL;
  as_closure(closure_that_resumes)->function = &lambda_expression_that_resumes;
  as_closure(closure_that_resumes)->n = 0;
}

#endif

#ifdef NANBOXING

static thing *stack_pointer(thing x) {
  return &x;
}

static thing allocate(size_t n) {
  if (n==0) return NULL;
  if (n%8!=0) internal_error();
  if (((thing)(((char *)from_free_pointer)+n))>from_limit) {
    gc();
    if (((thing)(((char *)from_free_pointer)+n))>from_limit) {
      panic("Out of memory");
    }
  }
  thing object = from_free_pointer;
  from_free_pointer = (thing)(((char *)from_free_pointer)+n);
  return object;
}

static thing gc_allocate(size_t n) {
  if (n==0) return NULL;
  if (n%8!=0) internal_error();
  if (((thing)(((char *)to_free_pointer)+n))>to_limit) internal_error();
  thing object = to_free_pointer;
  to_free_pointer = (thing)(((char *)to_free_pointer)+n);
  return object;
}

static thing copy(thing old) {
  if (is_true(old)) return old;
  else if (is_false(old)) return old;
  else if (is_null(old)) return old;
  else if (is_real(old)) return old;
  else if (is_dual_number(old)) {
    if (as_dual_number(old)->forward!=NULL) {
      if (debugging3) {
	printf("forward hit dual number %#018lx\n", (unsigned long)old);
      }
      return as_dual_number(old)->forward;
    }
    thing new;
    set_dual_number(&new);
    set_as_dual_number(&new,
		       (struct dual_number *)
		       gc_allocate(sizeof(struct dual_number)));
    if (debugging4) {
      printf("forwarding dual number %#018lx to %#018lx\n",
	     (unsigned long)old, (unsigned long)new);
    }
    as_dual_number(old)->forward = new;
    as_new_dual_number(new)->forward = NULL;
    as_new_dual_number(new)->epsilon = as_dual_number(old)->epsilon;
    as_new_dual_number(new)->primal = copy(as_dual_number(old)->primal);
    as_new_dual_number(new)->tangent = copy(as_dual_number(old)->tangent);
    return new;
  }
  else if (is_tape(old)) {
    if (as_tape(old)->forward!=NULL) {
      if (debugging3) printf("forward tape hit %#018lx\n", (unsigned long)old);
      return as_tape(old)->forward;
    }
    thing new;
    set_tape(&new);
    set_as_tape(&new, (struct tape *)gc_allocate(sizeof(struct {
      thing forward;
      unsigned epsilon;
      thing primal;
      unsigned n;
      unsigned fanout;
      thing cotangent;
      struct argument arguments[as_tape(old)->n];
    })));
    if (debugging4) {
      printf("forwarding tape %#018lx to %#018lx\n",
	     (unsigned long)old, (unsigned long)new);
    }
    as_tape(old)->forward = new;
    as_new_tape(new)->forward = NULL;
    as_new_tape(new)->epsilon = as_tape(old)->epsilon;
    as_new_tape(new)->primal = copy(as_tape(old)->primal);
    as_new_tape(new)->n = as_tape(old)->n;
    for (unsigned i = 0; i<as_tape(old)->n; i++) {
      as_new_tape(new)->arguments[i].factor =
	copy(as_tape(old)->arguments[i].factor);
    }
    for (unsigned i = 0; i<as_tape(old)->n; i++) {
      as_new_tape(new)->arguments[i].tape =
	copy(as_tape(old)->arguments[i].tape);
    }
    as_new_tape(new)->fanout = as_tape(old)->fanout;
    as_new_tape(new)->cotangent = copy(as_tape(old)->cotangent);
    return new;
  }
  else if (is_pair(old)) {
    if (as_pair(old)->forward!=NULL) {
      if (debugging3) printf("forward hit pair %#018lx\n", (unsigned long)old);
      return as_pair(old)->forward;
    }
    thing new;
    set_pair(&new);
    set_as_new_pair(&new, (struct pair *)gc_allocate(sizeof(struct pair)));
    if (debugging4) {
      printf("forwarding pair %#018lx to %#018lx\n",
	     (unsigned long)old, (unsigned long)new);
    }
    as_pair(old)->forward = new;
    as_new_pair(new)->forward = NULL;
    as_new_pair(new)->cache = copy(as_pair(old)->cache);
    as_new_pair(new)->car = copy(as_pair(old)->car);
    as_new_pair(new)->cdr = copy(as_pair(old)->cdr);
    return new;
  }
  else if (is_closure(old)) {
    if (as_closure(old)->forward!=NULL) {
      if (debugging3) {
	printf("forward hit closure %#018lx\n", (unsigned long)old);
      }
      return as_closure(old)->forward;
    }
    thing new;
    set_closure(&new);
    set_as_new_closure(&new, (struct closure *)gc_allocate(sizeof(struct {
      thing cache;
      thing forward;
      thing (*function)();
      unsigned n;
      thing environment[as_closure(old)->n];
    })));
    if (debugging4) {
      printf("forwarding closure %#018lx to %#018lx\n",
	     (unsigned long)old, (unsigned long)new);
    }
    as_closure(old)->forward = new;
    as_new_closure(new)->forward = NULL;
    as_new_closure(new)->cache = copy(as_closure(old)->cache);
    as_new_closure(new)->function = as_closure(old)->function;
    as_new_closure(new)->n = as_closure(old)->n;
    for (unsigned i = 0; i<as_closure(old)->n; i++) {
      as_new_closure(new)->environment[i] =
	copy(as_closure(old)->environment[i]);
    }
    return new;
  }
  else if (is_checkpoint(old)) {
    if (as_checkpoint(old)->forward!=NULL) {
      if (debugging3) {
	printf("forward hit checkpoint %#018lx\n", (unsigned long)old);
      }
      return as_checkpoint(old)->forward;
    }
    thing new;
    set_checkpoint(&new);
    set_as_checkpoint(&new,
		      (struct checkpoint *)
		      gc_allocate(sizeof(struct checkpoint)));
    if (debugging4) {
      printf("forwarding checkpoint %#018lx to %#018lx\n",
	     (unsigned long)old, (unsigned long)new);
    }
    as_checkpoint(old)->forward = new;
    as_new_checkpoint(new)->forward = NULL;
    as_new_checkpoint(new)->cache = copy(as_checkpoint(old)->cache);
    as_new_checkpoint(new)->continuation =
      copy(as_checkpoint(old)->continuation);
    as_new_checkpoint(new)->closure = copy(as_checkpoint(old)->closure);
    return new;
  }
  else internal_error();
}

static void gc(void) {
  thing *stack_top = stack_pointer(make_real(0.0));
  thing registers[13];
  get_registers(&registers[0]);
  if (debugging5) printf("begin GC\n");
  continuation_that_returns_x = copy(continuation_that_returns_x);
  continuation_that_returns_n = copy(continuation_that_returns_n);
  closure_that_resumes = copy(closure_that_resumes);
  for (unsigned i = 0; i<13; i++) {
    thing s = registers[i];
    if (is_dual_number(s)||is_tape(s)||is_pair(s)||is_closure(s)||
	is_checkpoint(s)) {
      if (in_from_space(s)) {
	if (debugging8) {
	  printf("register %u hit %#018lx\n", i, (unsigned long)s);
	}
	if (debugging9) {
	  printf("before ");
	  externalize(s);
	  printf("\n");
	}
	registers[i] = copy(s);
	if (debugging9) {
	  printf("after ");
	  externalize_new(registers[i]);
	  printf("\n");
	}
      }
    }
  }
  for (thing *s = stack_bottom; s>stack_top; s--) {
    if (debugging6) {
      printf("stack location %#018lx %#018lx\n",
	     (unsigned long)s, (unsigned long)*s);
    }
    if (is_dual_number(*s)||is_tape(*s)||is_pair(*s)||is_closure(*s)||
	is_checkpoint(*s)) {
      if (debugging7) printf("stack object %#018lx\n", (unsigned long)*s);
      if (in_from_space(*s)) {
	if (debugging8) printf("stack hit %#018lx\n", (unsigned long)*s);
	if (debugging9) {
	  printf("before ");
	  externalize(*s);
	  printf("\n");
	}
	*s = copy(*s);
	if (debugging9) {
	  printf("after ");
	  externalize_new(*s);
	  printf("\n");
	}
      }
    }
  }
  from_free_pointer = to_free_pointer;
  zero_to_one = !zero_to_one;
  if (zero_to_one) {
    from_begin = (thing)&heap0[0];
    from_limit = (thing)&heap0[HEAP];
    to_begin = (thing)&heap1[0];
    to_free_pointer = (thing)&heap1[0];
    to_limit = (thing)&heap1[HEAP];
  }
  else {
    from_begin = (thing)&heap1[0];
    from_limit = (thing)&heap1[HEAP];
    to_begin = (thing)&heap0[0];
    to_free_pointer = (thing)&heap0[0];
    to_limit = (thing)&heap0[HEAP];
  }
  if (debugging5) printf("end GC\n");
  put_registers(&registers[0]);
}

#endif
