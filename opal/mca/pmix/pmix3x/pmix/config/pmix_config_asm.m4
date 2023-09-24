dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2018 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2008-2018 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2017      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl Copyright (c) 2020      Google, LLC. All rights reserved.
dnl Copyright (c) 2020      Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl This is a C test to see if 128-bit __atomic_compare_exchange_n()
dnl actually works (e.g., it compiles and links successfully on
dnl ARM64+clang, but returns incorrect answers as of August 2018).
AC_DEFUN([PMIX_ATOMIC_COMPARE_EXCHANGE_N_TEST_SOURCE],[[
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
typedef union {
    uint64_t fake@<:@2@:>@;
    __int128 real;
} pmix128;
static void test1(void)
{
    // As of Aug 2018, we could not figure out a way to assign 128-bit
    // constants -- the compilers would not accept it.  So use a fake
    // union to assign 2 uin64_t's to make a single __int128.
    pmix128 ptr      = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    pmix128 expected = { .fake = { 0x11EEDDCCBBAA0099, 0x88776655443322FF }};
    pmix128 desired  = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = __atomic_compare_exchange_n(&ptr.real, &expected.real,
                                         desired.real, true,
                                         __ATOMIC_RELAXED, __ATOMIC_RELAXED);
    if ( !(r == false && ptr.real == expected.real)) {
        exit(1);
    }
}
static void test2(void)
{
    pmix128 ptr =      { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    pmix128 expected = ptr;
    pmix128 desired =  { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = __atomic_compare_exchange_n(&ptr.real, &expected.real,
                                         desired.real, true,
                                         __ATOMIC_RELAXED, __ATOMIC_RELAXED);
    if (!(r == true && ptr.real == desired.real)) {
        exit(2);
    }
}
int main(int argc, char** argv)
{
    test1();
    test2();
    return 0;
}
]])

dnl ------------------------------------------------------------------

dnl This is a C test to see if 128-bit __sync_bool_compare_and_swap()
dnl actually works (e.g., it compiles and links successfully on
dnl ARM64+clang, but returns incorrect answers as of August 2018).
AC_DEFUN([PMIX_SYNC_BOOL_COMPARE_AND_SWAP_TEST_SOURCE],[[
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
typedef union {
    uint64_t fake@<:@2@:>@;
    __int128 real;
} pmix128;
static void test1(void)
{
    // As of Aug 2018, we could not figure out a way to assign 128-bit
    // constants -- the compilers would not accept it.  So use a fake
    // union to assign 2 uin64_t's to make a single __int128.
    pmix128 ptr    = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    pmix128 oldval = { .fake = { 0x11EEDDCCBBAA0099, 0x88776655443322FF }};
    pmix128 newval = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = __sync_bool_compare_and_swap(&ptr.real, oldval.real, newval.real);
    if (!(r == false && ptr.real != newval.real)) {
        exit(1);
    }
}
static void test2(void)
{
    pmix128 ptr    = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    pmix128 oldval = ptr;
    pmix128 newval = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = __sync_bool_compare_and_swap(&ptr.real, oldval.real, newval.real);
    if (!(r == true && ptr.real == newval.real)) {
        exit(2);
    }
}
int main(int argc, char** argv)
{
    test1();
    test2();
    return 0;
}
]])

dnl This is a C test to see if 128-bit __atomic_compare_exchange_n()
dnl actually works (e.g., it compiles and links successfully on
dnl ARM64+clang, but returns incorrect answers as of August 2018).
AC_DEFUN([PMIX_ATOMIC_COMPARE_EXCHANGE_STRONG_TEST_SOURCE],[[
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdatomic.h>
typedef union {
    uint64_t fake@<:@2@:>@;
    _Atomic __int128 real;
    __int128 real2;
} pmix128;
static void test1(void)
{
    // As of Aug 2018, we could not figure out a way to assign 128-bit
    // constants -- the compilers would not accept it.  So use a fake
    // union to assign 2 uin64_t's to make a single __int128.
    pmix128 ptr      = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    pmix128 expected = { .fake = { 0x11EEDDCCBBAA0099, 0x88776655443322FF }};
    pmix128 desired  = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = atomic_compare_exchange_strong (&ptr.real, &expected.real2,
                                             desired.real);
    if ( !(r == false && ptr.real == expected.real)) {
        exit(1);
    }
}
static void test2(void)
{
    pmix128 ptr =      { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    pmix128 expected = ptr;
    pmix128 desired =  { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = atomic_compare_exchange_strong (&ptr.real, &expected.real2,
                                             desired.real);
    if (!(r == true && ptr.real == desired.real)) {
        exit(2);
    }
}
int main(int argc, char** argv)
{
    test1();
    test2();
    return 0;
}
]])

dnl ------------------------------------------------------------------

dnl
dnl Check to see if a specific function is linkable.
dnl
dnl Check with:
dnl 1. No compiler/linker flags.
dnl 2. CFLAGS += -mcx16
dnl 3. LIBS += -latomic
dnl 4. Finally, if it links ok with any of #1, #2, or #3, actually try
dnl to run the test code (if we're not cross-compiling) and verify
dnl that it actually gives us the correct result.
dnl
dnl Note that we unfortunately can't use AC SEARCH_LIBS because its
dnl check incorrectly fails (because these functions are special compiler
dnl intrinsics -- SEARCH_LIBS tries with "check FUNC()", which the
dnl compiler complains doesn't match the internal prototype).  So we have
dnl to use our own LINK_IFELSE tests.  Indeed, since these functions are
dnl so special, we actually need a valid source code that calls the
dnl functions with correct arguments, etc.  It's not enough, for example,
dnl to do the usual "try to set a function pointer to the symbol" trick to
dnl determine if these functions are available, because the compiler may
dnl not implement these as actual symbols.  So just try to link a real
dnl test code.
dnl
dnl $1: function name to print
dnl $2: program to test
dnl $3: action if any of 1, 2, or 3 succeeds
dnl #4: action if all of 1, 2, and 3 fail
dnl
AC_DEFUN([PMIX_ASM_CHECK_ATOMIC_FUNC],[
    PMIX_VAR_SCOPE_PUSH([pmix_asm_check_func_happy pmix_asm_check_func_CFLAGS_save pmix_asm_check_func_LIBS_save])
    pmix_asm_check_func_CFLAGS_save=$CFLAGS
    pmix_asm_check_func_LIBS_save=$LIBS
    dnl Check with no compiler/linker flags
    AC_MSG_CHECKING([for $1])
    AC_LINK_IFELSE([$2],
        [pmix_asm_check_func_happy=1
         AC_MSG_RESULT([yes])],
        [pmix_asm_check_func_happy=0
         AC_MSG_RESULT([no])])
    dnl If that didn't work, try again with CFLAGS+=mcx16
    AS_IF([test $pmix_asm_check_func_happy -eq 0],
        [AC_MSG_CHECKING([for $1 with -mcx16])
         CFLAGS="$CFLAGS -mcx16"
         AC_LINK_IFELSE([$2],
             [pmix_asm_check_func_happy=1
              AC_MSG_RESULT([yes])],
             [pmix_asm_check_func_happy=0
              CFLAGS=$pmix_asm_check_func_CFLAGS_save
              AC_MSG_RESULT([no])])
         ])
    dnl If that didn't work, try again with LIBS+=-latomic
    AS_IF([test $pmix_asm_check_func_happy -eq 0],
        [AC_MSG_CHECKING([for $1 with -latomic])
         LIBS="$LIBS -latomic"
         AC_LINK_IFELSE([$2],
             [pmix_asm_check_func_happy=1
              AC_MSG_RESULT([yes])],
             [pmix_asm_check_func_happy=0
              LIBS=$pmix_asm_check_func_LIBS_save
              AC_MSG_RESULT([no])])
         ])
    dnl If we have it, try it and make sure it gives a correct result.
    dnl As of Aug 2018, we know that it links but does *not* work on clang
    dnl 6 on ARM64.
    AS_IF([test $pmix_asm_check_func_happy -eq 1],
        [AC_MSG_CHECKING([if $1() gives correct results])
         AC_RUN_IFELSE([$2],
              [AC_MSG_RESULT([yes])],
              [pmix_asm_check_func_happy=0
               AC_MSG_RESULT([no])],
              [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
         ])
    dnl If we were unsuccessful, restore CFLAGS/LIBS
    AS_IF([test $pmix_asm_check_func_happy -eq 0],
        [CFLAGS=$pmix_asm_check_func_CFLAGS_save
         LIBS=$pmix_asm_check_func_LIBS_save])
    dnl Run the user actions
    AS_IF([test $pmix_asm_check_func_happy -eq 1], [$3], [$4])
    PMIX_VAR_SCOPE_POP
])

dnl ------------------------------------------------------------------

AC_DEFUN([PMIX_CHECK_SYNC_BUILTIN_CSWAP_INT128], [
  PMIX_VAR_SCOPE_PUSH([sync_bool_compare_and_swap_128_result])
  # Do we have __sync_bool_compare_and_swap?
  # Use a special macro because we need to check with a few different
  # CFLAGS/LIBS.
  PMIX_ASM_CHECK_ATOMIC_FUNC([__sync_bool_compare_and_swap],
      [AC_LANG_SOURCE(PMIX_SYNC_BOOL_COMPARE_AND_SWAP_TEST_SOURCE)],
      [sync_bool_compare_and_swap_128_result=1],
      [sync_bool_compare_and_swap_128_result=0])
  AC_DEFINE_UNQUOTED([PMIX_HAVE_SYNC_BUILTIN_CSWAP_INT128],
        [$sync_bool_compare_and_swap_128_result],
        [Whether the __sync builtin atomic compare and swap supports 128-bit values])
  PMIX_VAR_SCOPE_POP
])

AC_DEFUN([PMIX_CHECK_GCC_BUILTIN_CSWAP_INT128], [
  PMIX_VAR_SCOPE_PUSH([atomic_compare_exchange_n_128_result atomic_compare_exchange_n_128_CFLAGS_save atomic_compare_exchange_n_128_LIBS_save])
  atomic_compare_exchange_n_128_CFLAGS_save=$CFLAGS
  atomic_compare_exchange_n_128_LIBS_save=$LIBS
  # Do we have __sync_bool_compare_and_swap?
  # Use a special macro because we need to check with a few different
  # CFLAGS/LIBS.
  PMIX_ASM_CHECK_ATOMIC_FUNC([__atomic_compare_exchange_n],
      [AC_LANG_SOURCE(PMIX_ATOMIC_COMPARE_EXCHANGE_N_TEST_SOURCE)],
      [atomic_compare_exchange_n_128_result=1],
      [atomic_compare_exchange_n_128_result=0])
  # If we have it and it works, check to make sure it is always lock
  # free.
  AS_IF([test $atomic_compare_exchange_n_128_result -eq 1],
        [AC_MSG_CHECKING([if __int128 atomic compare-and-swap is always lock-free])
         AC_RUN_IFELSE([AC_LANG_PROGRAM([], [if (!__atomic_always_lock_free(16, 0)) { return 1; }])],
              [AC_MSG_RESULT([yes])],
              [atomic_compare_exchange_n_128_result=0
               # If this test fails, need to reset CFLAGS/LIBS (the
               # above tests atomically set CFLAGS/LIBS or not; this
               # test is running after the fact, so we have to undo
               # the side-effects of setting CFLAGS/LIBS if the above
               # tests passed).
               CFLAGS=$atomic_compare_exchange_n_128_CFLAGS_save
               LIBS=$atomic_compare_exchange_n_128_LIBS_save
               AC_MSG_RESULT([no])],
              [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
        ])
  AC_DEFINE_UNQUOTED([PMIX_HAVE_GCC_BUILTIN_CSWAP_INT128],
        [$atomic_compare_exchange_n_128_result],
        [Whether the __atomic builtin atomic compare swap is both supported and lock-free on 128-bit values])
  dnl If we could not find decent support for 128-bits __atomic let's
  dnl try the GCC _sync
  AS_IF([test $atomic_compare_exchange_n_128_result -eq 0],
      [PMIX_CHECK_SYNC_BUILTIN_CSWAP_INT128])
  PMIX_VAR_SCOPE_POP
])

AC_DEFUN([PMIX_CHECK_GCC_ATOMIC_BUILTINS], [
  if test -z "$pmix_cv_have___atomic" ; then
    AC_MSG_CHECKING([for 32-bit GCC built-in atomics])
    AC_TRY_LINK([
#include <stdint.h>
uint32_t tmp, old = 0;
uint64_t tmp64, old64 = 0;], [
__atomic_thread_fence(__ATOMIC_SEQ_CST);
__atomic_compare_exchange_n(&tmp, &old, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp, 1, __ATOMIC_RELAXED);
__atomic_compare_exchange_n(&tmp64, &old64, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp64, 1, __ATOMIC_RELAXED);],
        [pmix_cv_have___atomic=yes],
        [pmix_cv_have___atomic=no])
    AC_MSG_RESULT([$pmix_cv_have___atomic])
    if test $pmix_cv_have___atomic = "yes" ; then
    AC_MSG_CHECKING([for 64-bit GCC built-in atomics])
    AC_TRY_LINK([
#include <stdint.h>
uint64_t tmp64, old64 = 0;], [
__atomic_compare_exchange_n(&tmp64, &old64, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp64, 1, __ATOMIC_RELAXED);],
            [pmix_cv_have___atomic_64=yes],
            [pmix_cv_have___atomic_64=no])
    AC_MSG_RESULT([$pmix_cv_have___atomic_64])
    if test $pmix_cv_have___atomic_64 = "yes" ; then
        AC_MSG_CHECKING([if 64-bit GCC built-in atomics are lock-free])
        AC_RUN_IFELSE([AC_LANG_PROGRAM([], [if (!__atomic_is_lock_free (8, 0)) { return 1; }])],
              [AC_MSG_RESULT([yes])],
              [AC_MSG_RESULT([no])
               pmix_cv_have___atomic_64=no],
              [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
    fi
    else
    pmix_cv_have___atomic_64=no
    fi
    # Check for 128-bit support
    PMIX_CHECK_GCC_BUILTIN_CSWAP_INT128
  fi
])

AC_DEFUN([PMIX_CHECK_C11_CSWAP_INT128], [
  PMIX_VAR_SCOPE_PUSH([atomic_compare_exchange_result atomic_compare_exchange_CFLAGS_save atomic_compare_exchange_LIBS_save])
  atomic_compare_exchange_CFLAGS_save=$CFLAGS
  atomic_compare_exchange_LIBS_save=$LIBS
  # Do we have C11 atomics on 128-bit integers?
  # Use a special macro because we need to check with a few different
  # CFLAGS/LIBS.
  PMIX_ASM_CHECK_ATOMIC_FUNC([atomic_compare_exchange_strong_16],
      [AC_LANG_SOURCE(PMIX_ATOMIC_COMPARE_EXCHANGE_STRONG_TEST_SOURCE)],
      [atomic_compare_exchange_result=1],
      [atomic_compare_exchange_result=0])
  # If we have it and it works, check to make sure it is always lock
  # free.
  AS_IF([test $atomic_compare_exchange_result -eq 1],
        [AC_MSG_CHECKING([if C11 __int128 atomic compare-and-swap is always lock-free])
         AC_RUN_IFELSE([AC_LANG_PROGRAM([#include <stdatomic.h>], [_Atomic __int128_t x; if (!atomic_is_lock_free(&x)) { return 1; }])],
              [AC_MSG_RESULT([yes])],
              [atomic_compare_exchange_result=0
               # If this test fails, need to reset CFLAGS/LIBS (the
               # above tests atomically set CFLAGS/LIBS or not; this
               # test is running after the fact, so we have to undo
               # the side-effects of setting CFLAGS/LIBS if the above
               # tests passed).
               CFLAGS=$atomic_compare_exchange_CFLAGS_save
               LIBS=$atomic_compare_exchange_LIBS_save
               AC_MSG_RESULT([no])],
              [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
        ])
  AC_DEFINE_UNQUOTED([PMIX_HAVE_C11_CSWAP_INT128],
        [$atomic_compare_exchange_result],
        [Whether C11 atomic compare swap is both supported and lock-free on 128-bit values])
  dnl If we could not find decent support for 128-bits atomic let's
  dnl try the GCC _sync
  AS_IF([test $atomic_compare_exchange_result -eq 0],
      [PMIX_CHECK_SYNC_BUILTIN_CSWAP_INT128])
  PMIX_VAR_SCOPE_POP
])


dnl #################################################################
dnl
dnl PMIX_CHECK_CMPXCHG16B
dnl
dnl #################################################################
AC_DEFUN([PMIX_CMPXCHG16B_TEST_SOURCE],[[
#include <stdint.h>
#include <assert.h>
union pmix_counted_pointer_t {
    struct {
        uint64_t counter;
        uint64_t item;
    } data;
#if defined(HAVE___INT128) && HAVE___INT128
    __int128 value;
#elif defined(HAVE_INT128_T) && HAVE_INT128_T
    int128_t value;
#endif
};
typedef union pmix_counted_pointer_t pmix_counted_pointer_t;
int main(int argc, char* argv) {
    volatile pmix_counted_pointer_t a;
    pmix_counted_pointer_t b;
    a.data.counter = 0;
    a.data.item = 0x1234567890ABCDEF;
    b.data.counter = a.data.counter;
    b.data.item = a.data.item;
    /* bozo checks */
    assert(16 == sizeof(pmix_counted_pointer_t));
    assert(a.data.counter == b.data.counter);
    assert(a.data.item == b.data.item);
    /*
     * the following test fails on buggy compilers
     * so far, with icc -o conftest conftest.c
     *  - intel icc 14.0.0.080 (aka 2013sp1)
     *  - intel icc 14.0.1.106 (aka 2013sp1u1)
     * older and more recents compilers work fine
     * buggy compilers work also fine but only with -O0
     */
#if (defined(HAVE___INT128) && HAVE___INT128) || (defined(HAVE_INT128_T) && HAVE_INT128_T)
    return (a.value != b.value);
#else
    return 0;
#endif
}
]])

AC_DEFUN([PMIX_CHECK_CMPXCHG16B],[
    PMIX_VAR_SCOPE_PUSH([cmpxchg16b_result])
    PMIX_ASM_CHECK_ATOMIC_FUNC([cmpxchg16b],
                               [AC_LANG_PROGRAM([[unsigned char tmp[16];]],
                                                [[__asm__ __volatile__ ("lock cmpxchg16b (%%rsi)" : : "S" (tmp) : "memory", "cc");]])],
                               [cmpxchg16b_result=1],
                               [cmpxchg16b_result=0])
    # If we have it, make sure it works.
    AS_IF([test $cmpxchg16b_result -eq 1],
          [AC_MSG_CHECKING([if cmpxchg16b_result works])
           AC_RUN_IFELSE([AC_LANG_SOURCE(PMIX_CMPXCHG16B_TEST_SOURCE)],
                         [AC_MSG_RESULT([yes])],
                         [cmpxchg16b_result=0
                          AC_MSG_RESULT([no])],
                         [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
          ])
    AC_DEFINE_UNQUOTED([PMIX_HAVE_CMPXCHG16B], [$cmpxchg16b_result],
        [Whether the processor supports the cmpxchg16b instruction])
    PMIX_VAR_SCOPE_POP
])dnl

dnl #################################################################
dnl
dnl PMIX_CHECK_INLINE_GCC
dnl
dnl Check if the compiler is capable of doing GCC-style inline
dnl assembly.  Some compilers emit a warning and ignore the inline
dnl assembly (xlc on OS X) and compile without error.  Therefore,
dnl the test attempts to run the emitted code to check that the
dnl assembly is actually run.  To run this test, one argument to
dnl the macro must be an assembly instruction in gcc format to move
dnl the value 0 into the register containing the variable ret.
dnl For PowerPC, this would be:
dnl
dnl   "li %0,0" : "=&r"(ret)
dnl
dnl For testing ia32 assembly, the assembly instruction xaddl is
dnl tested.  The xaddl instruction is used by some of the atomic
dnl implementations so it makes sense to test for it.  In addition,
dnl some compilers (i.e. earlier versions of Sun Studio 12) do not
dnl necessarily handle xaddl properly, so that needs to be detected
dnl during configure time.
dnl
dnl DEFINE PMIX_GCC_INLINE_ASSEMBLY to 0 or 1 depending on GCC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([PMIX_CHECK_INLINE_C_GCC],[
    assembly="$1"
    asm_result="unknown"
    AC_MSG_CHECKING([if $CC supports GCC inline assembly])
    if test ! "$assembly" = "" ; then
        AC_RUN_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT],[[
int ret = 1;
int negone = -1;
__asm__ __volatile__ ($assembly);
return ret;
                                                             ]])],
                      [asm_result="yes"], [asm_result="no"],
                      [asm_result="unknown"])
    else
        assembly="test skipped - assuming no"
    fi
    # if we're cross compiling, just try to compile and figure good enough
    if test "$asm_result" = "unknown" ; then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT],[[
int ret = 1;
int negone = -1;
__asm__ __volatile__ ($assembly);
return ret;
                                                              ]])],
                       [asm_result="yes"], [asm_result="no"])
    fi
    AC_MSG_RESULT([$asm_result])
    if test "$asm_result" = "yes" ; then
        PMIX_C_GCC_INLINE_ASSEMBLY=1
        pmix_cv_asm_inline_supported="yes"
    else
        PMIX_C_GCC_INLINE_ASSEMBLY=0
    fi
    AC_DEFINE_UNQUOTED([PMIX_C_GCC_INLINE_ASSEMBLY],
                       [$PMIX_C_GCC_INLINE_ASSEMBLY],
                       [Whether C compiler supports GCC style inline assembly])
    unset PMIX_C_GCC_INLINE_ASSEMBLY assembly asm_result
])dnl

dnl #################################################################
dnl
dnl PMIX_CONFIG_ASM
dnl
dnl DEFINE PMIX_ASSEMBLY_ARCH to something in sys/architecture.h
dnl DEFINE PMIX_ASSEMBLY_FORMAT to string containing correct
dnl                             format for assembly (not user friendly)
dnl SUBST PMIX_ASSEMBLY_FORMAT to string containing correct
dnl                             format for assembly (not user friendly)
dnl
dnl #################################################################
AC_DEFUN([PMIX_CONFIG_ASM],[
    AC_REQUIRE([PMIX_SETUP_CC])
    AC_REQUIRE([AM_PROG_AS])
    AC_ARG_ENABLE([c11-atomics],[AC_HELP_STRING([--enable-c11-atomics],
                  [Enable use of C11 atomics if available (default: enabled)])])
    AC_ARG_ENABLE([builtin-atomics],
      [AC_HELP_STRING([--enable-builtin-atomics],
         [Enable use of GCC built-in atomics (default: autodetect)])])
    PMIX_CHECK_C11_CSWAP_INT128
    pmix_cv_asm_builtin="BUILTIN_NO"
    PMIX_CHECK_GCC_ATOMIC_BUILTINS
    if test "x$enable_c11_atomics" != "xno" && test "$pmix_cv_c11_supported" = "yes" ; then
        pmix_cv_asm_builtin="BUILTIN_C11"
        PMIX_CHECK_C11_CSWAP_INT128
    elif test "x$enable_c11_atomics" = "xyes"; then
        AC_MSG_WARN([C11 atomics were requested but are not supported])
        AC_MSG_ERROR([Cannot continue])
    elif test "$enable_builtin_atomics" = "yes" ; then
    if test $pmix_cv_have___atomic = "yes" ; then
       pmix_cv_asm_builtin="BUILTIN_GCC"
    else
        AC_MSG_WARN([GCC built-in atomics requested but not found.])
        AC_MSG_ERROR([Cannot continue])
    fi
    fi
        # find our architecture for purposes of assembly stuff
        pmix_cv_asm_arch="UNSUPPORTED"
        PMIX_GCC_INLINE_ASSIGN=""
    if test "$pmix_cv_have___atomic_64" ; then
            PMIX_ASM_SUPPORT_64BIT=1
    else
        PMIX_ASM_SUPPORT_64BIT=0
    fi
        case "${host}" in
        x86_64-*x32)
            pmix_cv_asm_arch="X86_64"
            PMIX_ASM_SUPPORT_64BIT=1
            PMIX_GCC_INLINE_ASSIGN='"xaddl %1,%0" : "=m"(ret), "+r"(negone) : "m"(ret)'
            ;;
        i?86-*|x86_64*|amd64*)
            if test "$ac_cv_sizeof_long" = "4" ; then
                pmix_cv_asm_arch="IA32"
            else
                pmix_cv_asm_arch="X86_64"
            fi
            PMIX_ASM_SUPPORT_64BIT=1
            PMIX_GCC_INLINE_ASSIGN='"xaddl %1,%0" : "=m"(ret), "+r"(negone) : "m"(ret)'
            PMIX_CHECK_CMPXCHG16B
            ;;
        aarch64*)
            pmix_cv_asm_arch="ARM64"
            PMIX_ASM_SUPPORT_64BIT=1
            PMIX_ASM_ARM_VERSION=8
            PMIX_GCC_INLINE_ASSIGN='"mov %0, #0" : "=&r"(ret)'
            ;;
        armv7*|arm-*-linux-gnueabihf)
            pmix_cv_asm_arch="ARM"
            PMIX_ASM_SUPPORT_64BIT=1
            PMIX_ASM_ARM_VERSION=7
            PMIX_GCC_INLINE_ASSIGN='"mov %0, #0" : "=&r"(ret)'
            ;;
        armv6*)
            pmix_cv_asm_arch="ARM"
            PMIX_ASM_SUPPORT_64BIT=0
            PMIX_ASM_ARM_VERSION=6
            CCASFLAGS="$CCASFLAGS -march=armv7-a"
            PMIX_GCC_INLINE_ASSIGN='"mov %0, #0" : "=&r"(ret)'
            ;;
        powerpc-*|powerpc64-*|powerpcle-*|powerpc64le-*|rs6000-*|ppc-*)
            if test "$ac_cv_sizeof_long" = "4" ; then
                pmix_cv_asm_arch="POWERPC32"
            elif test "$ac_cv_sizeof_long" = "8" ; then
                PMIX_ASM_SUPPORT_64BIT=1
                pmix_cv_asm_arch="POWERPC64"
            else
                AC_MSG_ERROR([Could not determine PowerPC word size: $ac_cv_sizeof_long])
            fi
            PMIX_GCC_INLINE_ASSIGN='"1: li %0,0" : "=&r"(ret)'
            ;;
        *)
        if test $pmix_cv_have___atomic = "yes" ; then
        pmix_cv_asm_builtin="BUILTIN_GCC"
        else
        AC_MSG_ERROR([No atomic primitives available for $host])
        fi
        ;;
        esac
    if ! test -z "$PMIX_ASM_ARM_VERSION" ; then
        AC_DEFINE_UNQUOTED([PMIX_ASM_ARM_VERSION], [$PMIX_ASM_ARM_VERSION],
                               [What ARM assembly version to use])
    fi
    if test "$pmix_cv_asm_builtin" = "BUILTIN_GCC" ; then
         AC_DEFINE([PMIX_C_GCC_INLINE_ASSEMBLY], [1],
           [Whether C compiler supports GCC style inline assembly])
    else
         AC_DEFINE_UNQUOTED([PMIX_ASM_SUPPORT_64BIT],
                [$PMIX_ASM_SUPPORT_64BIT],
                [Whether we can do 64bit assembly operations or not.  Should not be used outside of the assembly header files])
         AC_SUBST([PMIX_ASM_SUPPORT_64BIT])
         pmix_cv_asm_inline_supported="no"
         # now that we know our architecture, try to inline assemble
         PMIX_CHECK_INLINE_C_GCC([$PMIX_GCC_INLINE_ASSIGN])
      fi # if pmix_cv_asm_builtin = BUILTIN_GCC
    result="PMIX_$pmix_cv_asm_arch"
    PMIX_ASSEMBLY_ARCH="$pmix_cv_asm_arch"
    AC_MSG_CHECKING([for assembly architecture])
    AC_MSG_RESULT([$pmix_cv_asm_arch])
    AC_DEFINE_UNQUOTED([PMIX_ASSEMBLY_ARCH], [$result],
        [Architecture type of assembly to use for atomic operations and CMA])
    AC_SUBST([PMIX_ASSEMBLY_ARCH])
    # Check for RDTSCP support
    result=0
    AS_IF([test "$pmix_cv_asm_arch" = "PMIX_X86_64" || test "$pmix_cv_asm_arch" = "PMIX_IA32"],
          [AC_MSG_CHECKING([for RDTSCP assembly support])
           AC_LANG_PUSH([C])
           AC_TRY_RUN([[
int main(int argc, char* argv[])
{
  unsigned int rax, rdx;
  __asm__ __volatile__ ("rdtscp\n": "=a" (rax), "=d" (rdx):: "%rax", "%rdx");
  return 0;
}
           ]],
           [result=1
            AC_MSG_RESULT([yes])],
           [AC_MSG_RESULT([no])],
           [#cross compile not supported
            AC_MSG_RESULT(["no (cross compiling)"])])
           AC_LANG_POP([C])])
    AC_DEFINE_UNQUOTED([PMIX_ASSEMBLY_SUPPORTS_RDTSCP], [$result],
                       [Whether we have support for RDTSCP instruction])
    result="PMIX_$pmix_cv_asm_builtin"
    PMIX_ASSEMBLY_BUILTIN="$pmix_cv_asm_builtin"
    AC_MSG_CHECKING([for builtin atomics])
    AC_MSG_RESULT([$pmix_cv_asm_builtin])
    AC_DEFINE_UNQUOTED([PMIX_ASSEMBLY_BUILTIN], [$result],
        [Whether to use builtin atomics])
    AC_SUBST([PMIX_ASSEMBLY_BUILTIN])
    PMIX_ASM_FIND_FILE
    unset result asm_format
])dnl


dnl #################################################################
dnl
dnl PMIX_ASM_FIND_FILE
dnl
dnl
dnl do all the evil mojo to provide a working assembly file
dnl
dnl #################################################################
AC_DEFUN([PMIX_ASM_FIND_FILE], [
    AC_REQUIRE([AC_PROG_GREP])
    AC_REQUIRE([AC_PROG_FGREP])
if test "$pmix_cv_asm_arch" != "WINDOWS" && test "$pmix_cv_asm_builtin" != "BUILTIN_GCC" && test "$pmix_cv_asm_builtin" != "BUILTIN_OSX"  && test "$pmix_cv_asm_inline_arch" = "no" ; then
    AC_MSG_ERROR([no atomic support available. exiting])
else
    # On windows with VC++, atomics are done with compiler primitives
    pmix_cv_asm_file=""
fi
])dnl
