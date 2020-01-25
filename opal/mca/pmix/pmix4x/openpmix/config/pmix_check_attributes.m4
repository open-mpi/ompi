# -*- shell-script -*-
# PMIx copyrights:
# Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
#
#########################
#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2010 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
# Copyright (c) 2010-2018 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
#########################
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# Search the generated warnings for
# keywords regarding skipping or ignoring certain attributes
#   Intel: ignore
#   Sun C++: skip
#
AC_DEFUN([_PMIX_ATTRIBUTE_FAIL_SEARCH],[
    AC_REQUIRE([AC_PROG_GREP])
    if test -s conftest.err ; then
        # icc uses 'invalid attribute' and 'attribute "__XXX__"  ignored'
        # Sun 12.1 emits 'warning: attribute parameter "__printf__" is undefined'
        for i in invalid ignore skip undefined ; do
            $GREP -iq $i conftest.err
            if test "$?" = "0" ; then
                pmix_cv___attribute__[$1]=0
                break;
            fi
        done
    fi
])

#
# Check for one specific attribute by compiling with C
#
# The last argument is for specific CFLAGS, that need to be set
# for the compiler to generate a warning on the cross-check.
# This may need adaption for future compilers / CFLAG-settings.
#
AC_DEFUN([_PMIX_CHECK_SPECIFIC_ATTRIBUTE], [
    AC_MSG_CHECKING([for __attribute__([$1])])
    AC_CACHE_VAL(pmix_cv___attribute__[$1], [
        #
        # Try to compile using the C compiler
        #
        AC_TRY_COMPILE([$2],[],
                       [
                        #
                        # In case we did succeed: Fine, but was this due to the
                        # attribute being ignored/skipped? Grep for IgNoRe/skip in conftest.err
                        # and if found, reset the pmix_cv__attribute__var=0
                        #
                        pmix_cv___attribute__[$1]=1
                        _PMIX_ATTRIBUTE_FAIL_SEARCH([$1])
                       ],
                       [pmix_cv___attribute__[$1]=0])
    ])

    if test "$pmix_cv___attribute__[$1]" = "1" ; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
])


#
# Test the availability of __attribute__ and with the help
# of _PMIX_CHECK_SPECIFIC_ATTRIBUTE for the support of
# particular attributes. Compilers, that do not support an
# attribute most often fail with a warning (when the warning
# level is set).
# The compilers output is parsed in _PMIX_ATTRIBUTE_FAIL_SEARCH
#
# To add a new attributes __NAME__ add the
#   pmix_cv___attribute__NAME
# add a new check with _PMIX_CHECK_SPECIFIC_ATTRIBUTE (possibly with a cross-check)
#   _PMIX_CHECK_SPECIFIC_ATTRIBUTE([name], [int foo (int arg) __attribute__ ((__name__));], [], [])
# and define the corresponding
#   AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_NAME, [$pmix_cv___attribute__NAME],
#                      [Whether your compiler has __attribute__ NAME or not])
# and decide on a correct macro (in pmix/include/pmix_config_bottom.h):
#  #  define __pmix_attribute_NAME(x)  __attribute__(__NAME__)
#
# Please use the "__"-notation of the attribute in order not to
# clash with predefined names or macros (e.g. const, which some compilers
# do not like..)
#


AC_DEFUN([PMIX_CHECK_ATTRIBUTES], [
  AC_LANG(C)
  AC_MSG_CHECKING(for __attribute__)

  AC_CACHE_VAL(pmix_cv___attribute__, [
    AC_TRY_COMPILE(
      [#include <stdlib.h>
       /* Check for the longest available __attribute__ (since gcc-2.3) */
       struct foo {
           char a;
           int x[2] __attribute__ ((__packed__));
        };
      ],
      [],
      [pmix_cv___attribute__=1],
      [pmix_cv___attribute__=0],
    )

    if test "$pmix_cv___attribute__" = "1" ; then
        AC_TRY_COMPILE(
          [#include <stdlib.h>
           /* Check for the longest available __attribute__ (since gcc-2.3) */
           struct foo {
               char a;
               int x[2] __attribute__ ((__packed__));
            };
          ],
          [],
          [pmix_cv___attribute__=1],
          [pmix_cv___attribute__=0],
        )
    fi
    ])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE, [$pmix_cv___attribute__],
                     [Whether your compiler has __attribute__ or not])

#
# Now that we know the compiler support __attribute__ let's check which kind of
# attributed are supported.
#
  if test "$pmix_cv___attribute__" = "0" ; then
    AC_MSG_RESULT([no])
    pmix_cv___attribute__aligned=0
    pmix_cv___attribute__always_inline=0
    pmix_cv___attribute__cold=0
    pmix_cv___attribute__const=0
    pmix_cv___attribute__deprecated=0
    pmix_cv___attribute__deprecated_argument=0
    pmix_cv___attribute__format=0
    pmix_cv___attribute__format_funcptr=0
    pmix_cv___attribute__hot=0
    pmix_cv___attribute__malloc=0
    pmix_cv___attribute__may_alias=0
    pmix_cv___attribute__no_instrument_function=0
    pmix_cv___attribute__nonnull=0
    pmix_cv___attribute__noreturn=0
    pmix_cv___attribute__noreturn_funcptr=0
    pmix_cv___attribute__packed=0
    pmix_cv___attribute__pure=0
    pmix_cv___attribute__sentinel=0
    pmix_cv___attribute__unused=0
    pmix_cv___attribute__visibility=0
    pmix_cv___attribute__warn_unused_result=0
    pmix_cv___attribute__destructor=0
    pmix_cv___attribute__optnone=0
    pmix_cv___attribute__extension=0
  else
    AC_MSG_RESULT([yes])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([aligned],
        [struct foo { char text[4]; }  __attribute__ ((__aligned__(8)));],
        [],
        [])

    #
    # Ignored by PGI-6.2.5; -- recognized by output-parser
    #
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([always_inline],
        [int foo (int arg) __attribute__ ((__always_inline__));],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([cold],
        [
         int foo(int arg1, int arg2) __attribute__ ((__cold__));
         int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }
        ],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([const],
        [
         int foo(int arg1, int arg2) __attribute__ ((__const__));
         int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }
        ],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([deprecated],
        [
         int foo(int arg1, int arg2) __attribute__ ((__deprecated__));
         int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }
        ],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([deprecated_argument],
        [
         int foo(int arg1, int arg2) __attribute__ ((__deprecated__("compiler allows argument")));
         int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }
        ],
        [],
        [])

    ATTRIBUTE_CFLAGS=
    case "$pmix_c_vendor" in
        gnu)
            ATTRIBUTE_CFLAGS="-Wall"
            ;;
        intel)
            # we want specifically the warning on format string conversion
            ATTRIBUTE_CFLAGS="-we181"
            ;;
    esac
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([format],
        [
         int this_printf (void *my_object, const char *my_format, ...) __attribute__ ((__format__ (__printf__, 2, 3)));
        ],
        [
         static int usage (int * argument);
         extern int this_printf (int arg1, const char *my_format, ...) __attribute__ ((__format__ (__printf__, 2, 3)));

         static int usage (int * argument) {
             return this_printf (*argument, "%d", argument); /* This should produce a format warning */
         }
         /* The autoconf-generated main-function is int main(), which produces a warning by itself */
         int main(void);
        ],
        [$ATTRIBUTE_CFLAGS])

    ATTRIBUTE_CFLAGS=
    case "$pmix_c_vendor" in
        gnu)
            ATTRIBUTE_CFLAGS="-Wall"
            ;;
        intel)
            # we want specifically the warning on format string conversion
            ATTRIBUTE_CFLAGS="-we181"
            ;;
    esac
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([format_funcptr],
        [
         int (*this_printf)(void *my_object, const char *my_format, ...) __attribute__ ((__format__ (__printf__, 2, 3)));
        ],
        [
         static int usage (int * argument);
         extern int (*this_printf) (int arg1, const char *my_format, ...) __attribute__ ((__format__ (__printf__, 2, 3)));

         static int usage (int * argument) {
             return (*this_printf) (*argument, "%d", argument); /* This should produce a format warning */
         }
         /* The autoconf-generated main-function is int main(), which produces a warning by itself */
         int main(void);
        ],
        [$ATTRIBUTE_CFLAGS])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([hot],
        [
         int foo(int arg1, int arg2) __attribute__ ((__hot__));
         int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }
        ],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([malloc],
        [
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
         int * foo(int arg1) __attribute__ ((__malloc__));
         int * foo(int arg1) { return (int*) malloc(arg1); }
        ],
        [],
        [])


    #
    # Attribute may_alias: No suitable cross-check available, that works for non-supporting compilers
    # Ignored by intel-9.1.045 -- turn off with -wd1292
    # Ignored by PGI-6.2.5; ignore not detected due to missing cross-check
    #
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([may_alias],
        [int * p_value __attribute__ ((__may_alias__));],
        [],
        [])


    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([no_instrument_function],
        [int * foo(int arg1) __attribute__ ((__no_instrument_function__));],
        [],
        [])


    #
    # Attribute nonnull:
    # Ignored by intel-compiler 9.1.045 -- recognized by cross-check
    # Ignored by PGI-6.2.5 (pgCC) -- recognized by cross-check
    #
    ATTRIBUTE_CFLAGS=
    case "$pmix_c_vendor" in
        gnu)
            ATTRIBUTE_CFLAGS="-Wall"
            ;;
        intel)
            # we do not want to get ignored attributes warnings, but rather real warnings
            ATTRIBUTE_CFLAGS="-wd1292"
            ;;
    esac
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([nonnull],
        [
         int square(int *arg) __attribute__ ((__nonnull__));
         int square(int *arg) { return *arg; }
        ],
        [
         static int usage(int * argument);
         int square(int * argument) __attribute__ ((__nonnull__));
         int square(int * argument) { return (*argument) * (*argument); }

         static int usage(int * argument) {
             return square( ((void*)0) );    /* This should produce an argument must be nonnull warning */
         }
         /* The autoconf-generated main-function is int main(), which produces a warning by itself */
         int main(void);
        ],
        [$ATTRIBUTE_CFLAGS])


    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([noreturn],
        [
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
         void fatal(int arg1) __attribute__ ((__noreturn__));
         void fatal(int arg1) { exit(arg1); }
        ],
        [],
        [])


    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([noreturn_funcptr],
        [
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#endif
         extern void (*fatal_exit)(int arg1) __attribute__ ((__noreturn__));
         void fatal(int arg1) { fatal_exit (arg1); }
        ],
        [],
        [$ATTRIBUTE_CFLAGS])


    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([packed],
        [
         struct foo {
             char a;
             int x[2] __attribute__ ((__packed__));
         };
        ],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([pure],
        [
         int square(int arg) __attribute__ ((__pure__));
         int square(int arg) { return arg * arg; }
        ],
        [],
        [])

    #
    # Attribute sentinel:
    # Ignored by the intel-9.1.045 -- recognized by cross-check
    #                intel-10.0beta works fine
    # Ignored by PGI-6.2.5 (pgCC) -- recognized by output-parser and cross-check
    # Ignored by pathcc-2.2.1 -- recognized by cross-check (through grep ignore)
    #
    ATTRIBUTE_CFLAGS=
    case "$pmix_c_vendor" in
        gnu)
            ATTRIBUTE_CFLAGS="-Wall"
            ;;
        intel)
            # we do not want to get ignored attributes warnings
            ATTRIBUTE_CFLAGS="-wd1292"
            ;;
    esac
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([sentinel],
        [
         int my_execlp(const char * file, const char *arg, ...) __attribute__ ((__sentinel__));
        ],
        [
         static int usage(int * argument);
         int my_execlp(const char * file, const char *arg, ...) __attribute__ ((__sentinel__));

         static int usage(int * argument) {
             void * last_arg_should_be_null = argument;
             return my_execlp ("lala", "/home/there", last_arg_should_be_null);   /* This should produce a warning */
         }
         /* The autoconf-generated main-function is int main(), which produces a warning by itself */
         int main(void);
        ],
        [$ATTRIBUTE_CFLAGS])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([unused],
        [
         int square(int arg1 __attribute__ ((__unused__)), int arg2);
         int square(int arg1, int arg2) { return arg2; }
        ],
        [],
        [])


    #
    # Ignored by PGI-6.2.5 (pgCC) -- recognized by the output-parser
    #
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([visibility],
        [
         int square(int arg1) __attribute__ ((__visibility__("hidden")));
        ],
        [],
        [])


    #
    # Attribute warn_unused_result:
    # Ignored by the intel-compiler 9.1.045 -- recognized by cross-check
    # Ignored by pathcc-2.2.1 -- recognized by cross-check (through grep ignore)
    #
    ATTRIBUTE_CFLAGS=
    case "$pmix_c_vendor" in
        gnu)
            ATTRIBUTE_CFLAGS="-Wall"
            ;;
        intel)
            # we do not want to get ignored attributes warnings
            ATTRIBUTE_CFLAGS="-wd1292"
            ;;
    esac
    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([warn_unused_result],
        [
         int foo(int arg) __attribute__ ((__warn_unused_result__));
         int foo(int arg) { return arg + 3; }
        ],
        [
         static int usage(int * argument);
         int foo(int arg) __attribute__ ((__warn_unused_result__));

         int foo(int arg) { return arg + 3; }
         static int usage(int * argument) {
           foo (*argument);        /* Should produce an unused result warning */
           return 0;
         }

         /* The autoconf-generated main-function is int main(), which produces a warning by itself */
         int main(void);
        ],
        [$ATTRIBUTE_CFLAGS])


    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([destructor],
        [
        void foo(void) __attribute__ ((__destructor__));
        void foo(void) { return ; }
        ],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([optnone],
        [
        void __attribute__ ((__optnone__)) foo(void);
        void foo(void) { return ; }
        ],
        [],
        [])

    _PMIX_CHECK_SPECIFIC_ATTRIBUTE([extension],
        [
        int i = __extension__ 3;
        ],
        [],
        [])
  fi

  # Now that all the values are set, define them

  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_ALIGNED, [$pmix_cv___attribute__aligned],
                     [Whether your compiler has __attribute__ aligned or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_ALWAYS_INLINE, [$pmix_cv___attribute__always_inline],
                     [Whether your compiler has __attribute__ always_inline or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_COLD, [$pmix_cv___attribute__cold],
                     [Whether your compiler has __attribute__ cold or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_CONST, [$pmix_cv___attribute__const],
                     [Whether your compiler has __attribute__ const or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_DEPRECATED, [$pmix_cv___attribute__deprecated],
                     [Whether your compiler has __attribute__ deprecated or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_DEPRECATED_ARGUMENT, [$pmix_cv___attribute__deprecated_argument],
                     [Whether your compiler has __attribute__ deprecated with optional argument])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_FORMAT, [$pmix_cv___attribute__format],
                     [Whether your compiler has __attribute__ format or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_FORMAT_FUNCPTR, [$pmix_cv___attribute__format_funcptr],
                     [Whether your compiler has __attribute__ format and it works on function pointers])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_HOT, [$pmix_cv___attribute__hot],
                     [Whether your compiler has __attribute__ hot or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_MALLOC, [$pmix_cv___attribute__malloc],
                     [Whether your compiler has __attribute__ malloc or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_MAY_ALIAS, [$pmix_cv___attribute__may_alias],
                     [Whether your compiler has __attribute__ may_alias or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_NO_INSTRUMENT_FUNCTION, [$pmix_cv___attribute__no_instrument_function],
                     [Whether your compiler has __attribute__ no_instrument_function or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_NONNULL, [$pmix_cv___attribute__nonnull],
                     [Whether your compiler has __attribute__ nonnull or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_NORETURN, [$pmix_cv___attribute__noreturn],
                     [Whether your compiler has __attribute__ noreturn or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_NORETURN_FUNCPTR, [$pmix_cv___attribute__noreturn_funcptr],
                     [Whether your compiler has __attribute__ noreturn and it works on function pointers])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_PACKED, [$pmix_cv___attribute__packed],
                     [Whether your compiler has __attribute__ packed or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_PURE, [$pmix_cv___attribute__pure],
                     [Whether your compiler has __attribute__ pure or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_SENTINEL, [$pmix_cv___attribute__sentinel],
                     [Whether your compiler has __attribute__ sentinel or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_UNUSED, [$pmix_cv___attribute__unused],
                     [Whether your compiler has __attribute__ unused or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_VISIBILITY, [$pmix_cv___attribute__visibility],
                     [Whether your compiler has __attribute__ visibility or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT, [$pmix_cv___attribute__warn_unused_result],
                     [Whether your compiler has __attribute__ warn unused result or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_WEAK_ALIAS, [$pmix_cv___attribute__weak_alias],
                     [Whether your compiler has __attribute__ weak alias or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_DESTRUCTOR, [$pmix_cv___attribute__destructor],
                     [Whether your compiler has __attribute__ destructor or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_OPTNONE, [$pmix_cv___attribute__optnone],
                     [Whether your compiler has __attribute__ optnone or not])
  AC_DEFINE_UNQUOTED(PMIX_HAVE_ATTRIBUTE_EXTENSION, [$pmix_cv___attribute__extension],
                     [Whether your compiler has __attribute__ extension or not])
])
