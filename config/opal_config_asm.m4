dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2020 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2008-2021 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2017      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl Copyright (c) 2020      Google, LLC. All rights reserved.
dnl Copyright (c) 2020      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2021      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl This is a C test to see if 128-bit __atomic_compare_exchange_n()
dnl actually works (e.g., it compiles and links successfully on
dnl ARM64+clang, but returns incorrect answers as of August 2018).
AC_DEFUN([OPAL_ATOMIC_COMPARE_EXCHANGE_N_TEST_SOURCE],[[
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

typedef union {
    uint64_t fake@<:@2@:>@;
    __int128 real;
} ompi128;

static void test1(void)
{
    // As of Aug 2018, we could not figure out a way to assign 128-bit
    // constants -- the compilers would not accept it.  So use a fake
    // union to assign 2 uin64_t's to make a single __int128.
    ompi128 ptr      = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    ompi128 expected = { .fake = { 0x11EEDDCCBBAA0099, 0x88776655443322FF }};
    ompi128 desired  = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = __atomic_compare_exchange_n(&ptr.real, &expected.real,
                                         desired.real, true,
                                         __ATOMIC_RELAXED, __ATOMIC_RELAXED);
    if ( !(r == false && ptr.real == expected.real)) {
        exit(1);
    }
}

static void test2(void)
{
    ompi128 ptr =      { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    ompi128 expected = ptr;
    ompi128 desired =  { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
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
AC_DEFUN([OPAL_SYNC_BOOL_COMPARE_AND_SWAP_TEST_SOURCE],[[
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

typedef union {
    uint64_t fake@<:@2@:>@;
    __int128 real;
} ompi128;

static void test1(void)
{
    // As of Aug 2018, we could not figure out a way to assign 128-bit
    // constants -- the compilers would not accept it.  So use a fake
    // union to assign 2 uin64_t's to make a single __int128.
    ompi128 ptr    = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    ompi128 oldval = { .fake = { 0x11EEDDCCBBAA0099, 0x88776655443322FF }};
    ompi128 newval = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = __sync_bool_compare_and_swap(&ptr.real, oldval.real, newval.real);
    if (!(r == false && ptr.real != newval.real)) {
        exit(1);
    }
}

static void test2(void)
{
    ompi128 ptr    = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    ompi128 oldval = ptr;
    ompi128 newval = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
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
AC_DEFUN([OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_TEST_SOURCE],[[
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdatomic.h>

typedef union {
    uint64_t fake@<:@2@:>@;
    _Atomic __int128 real;
    __int128 real2;
} ompi128;

static void test1(void)
{
    // As of Aug 2018, we could not figure out a way to assign 128-bit
    // constants -- the compilers would not accept it.  So use a fake
    // union to assign 2 uin64_t's to make a single __int128.
    ompi128 ptr      = { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    ompi128 expected = { .fake = { 0x11EEDDCCBBAA0099, 0x88776655443322FF }};
    ompi128 desired  = { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
    bool r = atomic_compare_exchange_strong (&ptr.real, &expected.real2,
                                             desired.real);
    if ( !(r == false && ptr.real == expected.real)) {
        exit(1);
    }
}

static void test2(void)
{
    ompi128 ptr =      { .fake = { 0xFFEEDDCCBBAA0099, 0x8877665544332211 }};
    ompi128 expected = ptr;
    ompi128 desired =  { .fake = { 0x1122DDCCBBAA0099, 0x887766554433EEFF }};
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
AC_DEFUN([OPAL_ASM_CHECK_ATOMIC_FUNC],[
    OPAL_VAR_SCOPE_PUSH([opal_asm_check_func_happy opal_asm_check_func_CFLAGS_save opal_asm_check_func_LIBS_save])

    opal_asm_check_func_CFLAGS_save=$CFLAGS
    opal_asm_check_func_LIBS_save=$LIBS

    dnl Check with no compiler/linker flags
    AC_MSG_CHECKING([for $1])
    AC_LINK_IFELSE([$2],
        [opal_asm_check_func_happy=1
         AC_MSG_RESULT([yes])],
        [opal_asm_check_func_happy=0
         AC_MSG_RESULT([no])])

    dnl If that didn't work, try again with CFLAGS+=mcx16
    AS_IF([test $opal_asm_check_func_happy -eq 0],
        [AC_MSG_CHECKING([for $1 with -mcx16])
         CFLAGS="$CFLAGS -mcx16"
         AC_LINK_IFELSE([$2],
             [opal_asm_check_func_happy=1
              AC_MSG_RESULT([yes])],
             [opal_asm_check_func_happy=0
              CFLAGS=$opal_asm_check_func_CFLAGS_save
              AC_MSG_RESULT([no])])
         ])

    dnl If that didn't work, try again with LIBS+=-latomic
    AS_IF([test $opal_asm_check_func_happy -eq 0],
        [AC_MSG_CHECKING([for $1 with -latomic])
         LIBS="$LIBS -latomic"
         AC_LINK_IFELSE([$2],
             [opal_asm_check_func_happy=1
              AC_MSG_RESULT([yes])],
             [opal_asm_check_func_happy=0
              LIBS=$opal_asm_check_func_LIBS_save
              AC_MSG_RESULT([no])])
         ])

    dnl If we have it, try it and make sure it gives a correct result.
    dnl As of Aug 2018, we know that it links but does *not* work on clang
    dnl 6 on ARM64.
    AS_IF([test $opal_asm_check_func_happy -eq 1],
        [AC_MSG_CHECKING([if $1() gives correct results])
         AC_RUN_IFELSE([$2],
              [AC_MSG_RESULT([yes])],
              [opal_asm_check_func_happy=0
               AC_MSG_RESULT([no])],
              [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
         ])

    dnl If we were unsuccessful, restore CFLAGS/LIBS
    AS_IF([test $opal_asm_check_func_happy -eq 0],
        [CFLAGS=$opal_asm_check_func_CFLAGS_save
         LIBS=$opal_asm_check_func_LIBS_save])

    dnl Run the user actions
    AS_IF([test $opal_asm_check_func_happy -eq 1], [$3], [$4])

    OPAL_VAR_SCOPE_POP
])

dnl ------------------------------------------------------------------

AC_DEFUN([OPAL_CHECK_SYNC_BUILTIN_CSWAP_INT128], [
  OPAL_VAR_SCOPE_PUSH([sync_bool_compare_and_swap_128_result])

  # Do we have __sync_bool_compare_and_swap?
  # Use a special macro because we need to check with a few different
  # CFLAGS/LIBS.
  OPAL_ASM_CHECK_ATOMIC_FUNC([__sync_bool_compare_and_swap],
      [AC_LANG_SOURCE(OPAL_SYNC_BOOL_COMPARE_AND_SWAP_TEST_SOURCE)],
      [sync_bool_compare_and_swap_128_result=1],
      [sync_bool_compare_and_swap_128_result=0])

  AC_DEFINE_UNQUOTED([OPAL_HAVE_SYNC_BUILTIN_CSWAP_INT128],
        [$sync_bool_compare_and_swap_128_result],
        [Whether the __sync builtin atomic compare and swap supports 128-bit values])

  OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OPAL_CHECK_GCC_BUILTIN_CSWAP_INT128], [
  OPAL_VAR_SCOPE_PUSH([atomic_compare_exchange_n_128_result atomic_compare_exchange_n_128_CFLAGS_save atomic_compare_exchange_n_128_LIBS_save])

  atomic_compare_exchange_n_128_CFLAGS_save=$CFLAGS
  atomic_compare_exchange_n_128_LIBS_save=$LIBS

  # Do we have __sync_bool_compare_and_swap?
  # Use a special macro because we need to check with a few different
  # CFLAGS/LIBS.
  OPAL_ASM_CHECK_ATOMIC_FUNC([__atomic_compare_exchange_n],
      [AC_LANG_SOURCE(OPAL_ATOMIC_COMPARE_EXCHANGE_N_TEST_SOURCE)],
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

  AC_DEFINE_UNQUOTED([OPAL_HAVE_GCC_BUILTIN_CSWAP_INT128],
        [$atomic_compare_exchange_n_128_result],
        [Whether the __atomic builtin atomic compare swap is both supported and lock-free on 128-bit values])

  dnl If we could not find decent support for 128-bits __atomic let's
  dnl try the GCC _sync
  AS_IF([test $atomic_compare_exchange_n_128_result -eq 0],
      [OPAL_CHECK_SYNC_BUILTIN_CSWAP_INT128])

  OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OPAL_CHECK_GCC_ATOMIC_BUILTINS], [
  if test -z "$opal_cv_have___atomic" ; then
    AC_MSG_CHECKING([for 32-bit GCC built-in atomics])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <stdint.h>
uint32_t tmp, old = 0;
uint64_t tmp64, old64 = 0;], [
__atomic_thread_fence(__ATOMIC_SEQ_CST);
__atomic_compare_exchange_n(&tmp, &old, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp, 1, __ATOMIC_RELAXED);
__atomic_compare_exchange_n(&tmp64, &old64, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp64, 1, __ATOMIC_RELAXED);])],
                [opal_cv_have___atomic=yes],
                [opal_cv_have___atomic=no])

    AC_MSG_RESULT([$opal_cv_have___atomic])

    if test $opal_cv_have___atomic = "yes" ; then
        AC_MSG_CHECKING([for 64-bit GCC built-in atomics])

        AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <stdint.h>
uint64_t tmp64, old64 = 0;], [
__atomic_compare_exchange_n(&tmp64, &old64, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp64, 1, __ATOMIC_RELAXED);])],
                    [opal_cv_have___atomic_64=yes],
                    [opal_cv_have___atomic_64=no])

        AC_MSG_RESULT([$opal_cv_have___atomic_64])

        if test $opal_cv_have___atomic_64 = "yes" ; then
            AC_MSG_CHECKING([if 64-bit GCC built-in atomics are lock-free])
            AC_RUN_IFELSE([AC_LANG_PROGRAM([], [if (!__atomic_is_lock_free (8, 0)) { return 1; }])],
                          [AC_MSG_RESULT([yes])],
                          [AC_MSG_RESULT([no])
                           opal_cv_have___atomic_64=no],
                          [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
        fi
    else
        opal_cv_have___atomic_64=no
    fi

    # Check for 128-bit support
    OPAL_CHECK_GCC_BUILTIN_CSWAP_INT128
  fi
])


AC_DEFUN([OPAL_CHECK_C11_CSWAP_INT128], [
  OPAL_VAR_SCOPE_PUSH([atomic_compare_exchange_result atomic_compare_exchange_CFLAGS_save atomic_compare_exchange_LIBS_save])

  atomic_compare_exchange_CFLAGS_save=$CFLAGS
  atomic_compare_exchange_LIBS_save=$LIBS

  # Do we have C11 atomics on 128-bit integers?
  # Use a special macro because we need to check with a few different
  # CFLAGS/LIBS.
  OPAL_ASM_CHECK_ATOMIC_FUNC([atomic_compare_exchange_strong_16],
      [AC_LANG_SOURCE(OPAL_ATOMIC_COMPARE_EXCHANGE_STRONG_TEST_SOURCE)],
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

  AC_DEFINE_UNQUOTED([OPAL_HAVE_C11_CSWAP_INT128],
        [$atomic_compare_exchange_result],
        [Whether C11 atomic compare swap is both supported and lock-free on 128-bit values])

  dnl If we could not find decent support for 128-bits atomic let's
  dnl try the GCC _sync
  AS_IF([test $atomic_compare_exchange_result -eq 0],
      [OPAL_CHECK_SYNC_BUILTIN_CSWAP_INT128])

  OPAL_VAR_SCOPE_POP
])


dnl #################################################################
dnl
dnl OPAL_CHECK_CMPXCHG16B
dnl
dnl #################################################################
AC_DEFUN([OPAL_CMPXCHG16B_TEST_SOURCE],[[
#include <stdint.h>
#include <assert.h>

union opal_counted_pointer_t {
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
typedef union opal_counted_pointer_t opal_counted_pointer_t;

int main(int argc, char* argv) {
    volatile opal_counted_pointer_t a;
    opal_counted_pointer_t b;

    a.data.counter = 0;
    a.data.item = 0x1234567890ABCDEF;

    b.data.counter = a.data.counter;
    b.data.item = a.data.item;

    /* bozo checks */
    assert(16 == sizeof(opal_counted_pointer_t));
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

AC_DEFUN([OPAL_CHECK_CMPXCHG16B],[
    OPAL_VAR_SCOPE_PUSH([cmpxchg16b_result])

    OPAL_ASM_CHECK_ATOMIC_FUNC([cmpxchg16b],
                               [AC_LANG_PROGRAM([[unsigned char tmp[16];]],
                                                [[__asm__ __volatile__ ("lock cmpxchg16b (%%rsi)" : : "S" (tmp) : "memory", "cc");]])],
                               [cmpxchg16b_result=1],
                               [cmpxchg16b_result=0])
    # If we have it, make sure it works.
    AS_IF([test $cmpxchg16b_result -eq 1],
          [AC_MSG_CHECKING([if cmpxchg16b_result works])
           AC_RUN_IFELSE([AC_LANG_SOURCE(OPAL_CMPXCHG16B_TEST_SOURCE)],
                         [AC_MSG_RESULT([yes])],
                         [cmpxchg16b_result=0
                          AC_MSG_RESULT([no])],
                         [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
          ])

    AC_DEFINE_UNQUOTED([OPAL_HAVE_CMPXCHG16B], [$cmpxchg16b_result],
        [Whether the processor supports the cmpxchg16b instruction])
    OPAL_VAR_SCOPE_POP
])dnl


dnl #################################################################
dnl
dnl OPAL_CHECK_INLINE_GCC([action-if-found], [action-if-not-found])
dnl
dnl Check if the compiler is capable of doing GCC-style inline
dnl assembly.  Some compilers emit a warning and ignore the inline
dnl assembly (xlc on OS X) and compile without error.  Therefore,
dnl the test attempts to run the emitted code to check that the
dnl assembly is actually run.
dnl
dnl For testing ia32 assembly, the assembly instruction xaddl is
dnl tested.  The xaddl instruction is used by some of the atomic
dnl implementations so it makes sense to test for it.  In addition,
dnl some compilers (i.e. earlier versions of Sun Studio 12) do not
dnl necessarily handle xaddl properly, so that needs to be detected
dnl during configure time.
dnl
dnl DEFINE OPAL_C_GCC_INLINE_ASSEMBLY to 0 or 1 depending on GCC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([OPAL_CHECK_INLINE_C_GCC],[
    AC_CACHE_CHECK([if $CC supports GCC inline assembly],
       [opal_cv_asm_gcc_inline_assembly],
       [OPAL_VAR_SCOPE_PUSH([asm_result opal_gcc_inline_assign OPAL_C_GCC_INLINE_ASSEMBLY])

        asm_result="unknown"

        opal_gcc_inline_assign=""
        case "${host}" in
            x86_64-*x32|i?86-*|x86_64*|amd64*)
                opal_gcc_inline_assign='"xaddl %1,%0" : "=m"(ret), "+r"(negone) : "m"(ret)'
                ;;
            aarch64*)
                opal_gcc_inline_assign='"mov %0, #0" : "=&r"(ret)'
                ;;
            powerpc-*|powerpc64-*|powerpcle-*|powerpc64le-*|rs6000-*|ppc-*)
                opal_gcc_inline_assign='"1: li %0,0" : "=&r"(ret)'
                ;;
        esac

        AS_IF([test  "$opal_gcc_inline_assign" != ""],
            [AC_RUN_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT],[[
int ret = 1;
int negone = -1;
__asm__ __volatile__ ($opal_gcc_inline_assign);
return ret;
                ]])],
               [asm_result="yes"], [asm_result="no"],
               [asm_result="unknown"])],
            [asm_result="no - architecture not supported"])

        # if we're cross compiling, just try to compile and figure good enough
        AS_IF([test "$asm_result" = "unknown"],
            [AC_LINK_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT],[[
int ret = 1;
int negone = -1;
__asm__ __volatile__ ($opal_gcc_inline_assign);
return ret;
                ]])],
            [asm_result="yes"], [asm_result="no"])])
    opal_cv_asm_gcc_inline_assembly="$asm_result"
    OPAL_VAR_SCOPE_POP])

    AS_IF([test "$opal_cv_asm_gcc_inline_assembly" = "yes"],
          [OPAL_C_GCC_INLINE_ASSEMBLY=1
           $1],
          [OPAL_C_GCC_INLINE_ASSEMBLY=0
           $2])

    AC_DEFINE_UNQUOTED([OPAL_C_GCC_INLINE_ASSEMBLY],
                       [$OPAL_C_GCC_INLINE_ASSEMBLY],
                       [Whether C compiler supports GCC style inline assembly])
])dnl


dnl #################################################################
dnl
dnl OPAL_CONFIG_ASM
dnl
dnl Configure assembly support.  AC_DEFINES the following:
dnl   - OPAL_C_GCC_INLINE_ASSEMBLY - 1 if C compiler supports
dnl         GCC-style inline assembly
dnl   - OPAL_USE_C11_ATOMICS - 1 if atomics implementation should
dnl         use C11-style atomics
dnl   - OPAL_USE_GCC_BUILTIN_ATOMICS - 1 if atomics implementation
dnl         should use GCC built-in style atomics
dnl   - OPAL_USE_ASM_ATOMICS - 1 if atomics implementation should
dnl         use inline assembly (using GCC-style inline assembly)
dnl        for atomics implementaiton
dnl
dnl #################################################################
AC_DEFUN([OPAL_CONFIG_ASM],[
    AC_REQUIRE([OPAL_SETUP_CC])

    OPAL_VAR_SCOPE_PUSH([atomics_found want_c11_atomics want_gcc_builtin_atomics want_asm_atomics])

    # only assembly style we support today is gcc-style inline
    # assembly, find out if it works.  We need this even for C11/GCC
    # builtin atomics cases, because we use inline assembly for
    # timers, LLSC, and 16 byte compare and swap routines.
    OPAL_CHECK_INLINE_C_GCC([gcc_inline=1], [gcc_inline=0])

    atomics_found=no
    want_c11_atomics=0
    want_gcc_builtin_atomics=0
    want_asm_atomics=0

    AC_ARG_ENABLE([c11-atomics],
        [AS_HELP_STRING([--enable-c11-atomics],
             [Enable use of C11 atomics if available (default: use if available, disabled by default on 64-bit PowerPC)])])

    AC_ARG_ENABLE([builtin-atomics],
        [AS_HELP_STRING([--enable-builtin-atomics],
             [Enable use of GCC built-in atomics.  Note that C11 atomics are preferred over built-in atomics.   (default: use if available, disabled by default on 64-bit PowerPC)])])

    AC_ARG_ENABLE([builtin-atomics-for-ppc],
        [AS_HELP_STRING([--enable-builtin-atomics-for-ppc],
             [For performance reasons, 64-bit POWER architectures will not use C11 or GCC built-in atomics, even if --enable-c11-atomics is passed to configure.  Enabling this option will re-enable support for both C11 and GCC built-in atomics.])])

    # See the following github PR and some performance numbers/discussion:
    # https://github.com/open-mpi/ompi/pull/8649
    #
    # This logic is a bit convoluted, but matches existing logic in v4.x.
    case "${host}" in
    powerpc-*|powerpc64-*|powerpcle-*|powerpc64le-*|rs6000-*|ppc-*)
          AS_IF([test "$ac_cv_sizeof_long" = "8" -a "$enable_builtin_atomics_for_ppc" != "yes"],
                [AS_IF([test "$enable_c11_atomics" != "no" -a "$enable_builtin_atomics" != "no"],
                       [AC_MSG_NOTICE([Disabling built-in and C11 atomics due to known performance issues on Powerpc])])
                 AS_IF([test "$enable_c11_atomics" = "yes" -o "$enable_builtin_atomics" = "yes"],
                       [AC_MSG_WARN([Ignoring --enable-c11-atomics and --enable-builtin-atomics options on POWER.  Set
--enable-builtin-atomics-for-ppc to re-enable.])])
                 enable_c11_atomics="no"
                 enable_builtin_atomics="no"])
    ;;
    esac

    # Option 1 for atomics: C11
    #
    # We currently always disable C11 atomics with the Intel compilers.
    # We know builds older than 20200310 are broken with respect to
    # C11 atomics, but have not apparently found a build we are happy
    # with.  In the future, this should be changed to a check for a
    # particular Intel version.
    AS_IF([test "$enable_c11_atomics" != "no" -a "$opal_cv_c11_supported" = "yes" -a "$opal_cv_c_compiler_vendor" != "intel"],
          [AC_MSG_NOTICE([Using C11 atomics])
           OPAL_CHECK_C11_CSWAP_INT128
           want_c11_atomics=1
           atomics_found="C11 atomics"],
          [test "$enable_c11_atomics" = "yes"],
          [AC_MSG_WARN([C11 atomics were requested but are not supported])
           AC_MSG_ERROR([Cannot continue])])

    # Option 2 for atomics: GCC-style Builtin
    AS_IF([test "$atomics_found" = "no" -a "$enable_builtin_atomics" != "no"],
          [OPAL_CHECK_GCC_ATOMIC_BUILTINS
           AS_IF([test $opal_cv_have___atomic = "yes"],
	         [AC_MSG_NOTICE([Using GCC built-in style atomics])
                  atomics_found="GCC built-in style atomics"
                  want_gcc_builtin_atomics=1],
                 [test "$enable_builtin_atomics" = "yes"],
                 [AC_MSG_WARN([GCC built-in atomics requested but not found.])
		  AC_MSG_ERROR([Cannot continue])])])

    # Option 3 for atomics: inline assembly
    AS_IF([test "$atomics_found" = "no" -a "$gcc_inline" = "1"],
          [case "${host}" in
           x86_64-*x32|i?86-*|x86_64*|amd64*)
               AS_IF([test "$ac_cv_sizeof_long" = "8"],
                     [OPAL_CHECK_CMPXCHG16B
                      atomics_found="x86_64 assembly"])
            ;;

            aarch64*)
                atomics_found="aarch64 assembly"
            ;;

            powerpc-*|powerpc64-*|powerpcle-*|powerpc64le-*|rs6000-*|ppc-*)
                AS_IF([test "$ac_cv_sizeof_long" = "8"],
                      [atomics_found="PowerPC asssembly"])
            ;;
            esac

            AS_IF([test "$atomics_found" != "no"],
                  [want_asm_atomics=1])
            AC_MSG_CHECKING([for inline assembly atomics])
            AC_MSG_RESULT([$atomics_found])])

    AS_IF([test "$aomics_found" = "no"],
          [AC_MSG_ERROR([No usable atomics implementation found.  Cannot continue.])])

    AC_DEFINE_UNQUOTED([OPAL_USE_C11_ATOMICS],
        [$want_c11_atomics],
        [Whether to use C11 atomics for atomics implementation])
    AC_DEFINE_UNQUOTED([OPAL_USE_GCC_BUILTIN_ATOMICS],
        [$want_gcc_builtin_atomics],
        [Whether to use GCC-style built-in atomics for atomics implementation])
    AC_DEFINE_UNQUOTED([OPAL_USE_ASM_ATOMICS],
        [$want_asm_atomics],
        [Whether to use assembly-coded atomics for atomics implementation])

    OPAL_SUMMARY_ADD([[Miscellaneous]],[[Atomics]],[],[$atomics_found])

    OPAL_VAR_SCOPE_POP
])dnl
