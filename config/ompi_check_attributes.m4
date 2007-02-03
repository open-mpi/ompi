# -*- shell-script -*-
#
# Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OMPI_CHECK____ATTRIBUTE__], [
  AC_MSG_CHECKING(for __attribute__)
  AC_CACHE_VAL(ac_cv___attribute__, [
    AC_TRY_COMPILE(
      [#include <stdlib.h>
       /* Check for the longest available __attribute__ (since gcc-2.3) */
       struct foo {
           char a;
           int x[2] __attribute__ ((packed));
        };
      ],
      [],
      [ac_cv___attribute__=1],
      [ac_cv___attribute__=0],
    )

    if test "$ac_cv___attribute__" = "1" ; then
        AC_TRY_COMPILE(
          [#include <stdlib.h>
           /* Check for the longest available __attribute__ (since gcc-2.3) */
           struct foo {
               char a;
               int x[2] __attribute__ ((packed));
            };
          ],
          [],
          [ac_cv___attribute__=1 ac_cv___attribute__msg=yes],
          [ac_cv___attribute__=0 ac_cv___attribute__msg=no],
        )
    fi
    ])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE, [$ac_cv___attribute__],
                     [Whether your compiler has __attribute__ or not])
  AC_MSG_RESULT($ac_cv___attribute__msg)

#
# Now that we know the compiler support __attribute__ let's check which kind of
# attributed are supported.
#
  if test "$ac_cv___attribute__msg" = "no"; then
    ac_cv___attribute__aligned=0
    ac_cv___attribute__always_inline=0
    ac_cv___attribute__const=0
    ac_cv___attribute__deprecated=0
    ac_cv___attribute__format=0
    ac_cv___attribute__malloc=0
    ac_cv___attribute__may_alias=0
    ac_cv___attribute__nonnull=0
    ac_cv___attribute__noreturn=0
    ac_cv___attribute__pure=0
    ac_cv___attribute__packed=0
    ac_cv___attribute__unused=0
    ac_cv___attribute__sentinel=0
    ac_cv___attribute__visibility=0
    ac_cv___attribute__warn_unused_result=0
    ac_cv___attribute__weak_alias=0
  else

#
# All attributes may be specified in Header files -- requiring them to be
# compilable with both the selected C- and the C++ compilers.
#

    AC_MSG_CHECKING([for __attribute__ ((aligned))])
    AC_CACHE_VAL(ac_cv___attribute__aligned, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         struct foo { char text[4]; }  __attribute__ ((aligned(8))); ],
        [],
        [ac_cv___attribute__aligned=1], [ac_cv___attribute__aligned=0]
      )

      if test "$ac_cv___attribute__aligned" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             // No need to declare extern "C"
             struct foo { char text[4]; }  __attribute__ ((aligned(8))); ],
            [],
            [ac_cv___attribute__aligned=1 ac_cv___attribute__aligned_msg=yes], [ac_cv___attribute__aligned=0 ac_cv___attribute__aligned_msg=no]
          )
          AC_LANG_POP(C++)
      fi

      ])
    AC_MSG_RESULT($ac_cv___attribute__aligned_msg)


    AC_MSG_CHECKING([for __attribute__ ((always_inline))])
    AC_CACHE_VAL(ac_cv___attribute__always_inline, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
          int foo (int arg) __attribute__ ((always_inline));],
        [],
        [ac_cv___attribute__always_inline=1],
        [ac_cv___attribute__always_inline=0]
      )

      if test "$ac_cv___attribute__always_inline" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int foo (int arg) __attribute__ ((always_inline));
             }
            ],
            [],
            [ac_cv___attribute__always_inline=1 ac_cv___attribute__always_inline_msg=yes],
            [ac_cv___attribute__always_inline=0 ac_cv___attribute__always_inline_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__always_inline_msg)


    AC_MSG_CHECKING([for __attribute__ ((const))])
    AC_CACHE_VAL(ac_cv___attribute__const, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int foo(int arg1, int arg2) __attribute__ ((const));
         int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }],
        [],
        [ac_cv___attribute__const=1],
        [ac_cv___attribute__const=0]
      )

      if test "$ac_cv___attribute__const" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int foo(int arg1, int arg2) __attribute__ ((const));
               int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }
             }
            ],
            [],
            [ac_cv___attribute__const=1 ac_cv___attribute__const_msg=yes],
            [ac_cv___attribute__const=0 ac_cv___attribute__const_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__const_msg)


    AC_MSG_CHECKING([for __attribute__ ((deprecated))])
    AC_CACHE_VAL(ac_cv___attribute__deprecated, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int foo(int arg1, int arg2) __attribute__ ((deprecated));
         int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }],
        [],
        [ac_cv___attribute__deprecated=1],
        [ac_cv___attribute__deprecated=0]
      )

      if test "$ac_cv___attribute__deprecated" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int foo(int arg1, int arg2) __attribute__ ((deprecated));
               int foo(int arg1, int arg2) { return arg1 * arg2 + arg1; }
             }
            ],
            [],
            [ac_cv___attribute__deprecated=1 ac_cv___attribute__deprecated_msg=yes],
            [ac_cv___attribute__deprecated=0 ac_cv___attribute__deprecated_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__deprecated_msg)


    AC_MSG_CHECKING([for __attribute__ ((format))])
    AC_CACHE_VAL(ac_cv___attribute__format, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int this_printf (void *my_object, const char *my_format, ...) __attribute__ ((format (printf, 2, 3)));],
        [],
        [ac_cv___attribute__format=1],
        [ac_cv___attribute__format=0]
      )

      if test "$ac_cv___attribute__format" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int this_printf (void *my_object, const char *my_format, ...) __attribute__ ((format (printf, 2, 3)));
             }
            ],
            [],
            [ac_cv___attribute__format=1 ac_cv___attribute__format_msg=yes],
            [ac_cv___attribute__format=0 ac_cv___attribute__format_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__format_msg)


    AC_MSG_CHECKING([for __attribute__ ((malloc))])
    AC_CACHE_VAL(ac_cv___attribute__malloc, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int * foo(int arg1) __attribute__ ((malloc));
         int * foo(int arg1) { return (int*) malloc(arg1); }],
        [],
        [ac_cv___attribute__malloc=1],
        [ac_cv___attribute__malloc=0]
      )

      if test "$ac_cv___attribute__malloc" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int * foo(int arg1) __attribute__ ((malloc));
               int * foo(int arg1) { return (int*) malloc(arg1); }
             }
            ],
            [],
            [ac_cv___attribute__malloc=1 ac_cv___attribute__malloc_msg=yes],
            [ac_cv___attribute__malloc=0 ac_cv___attribute__malloc_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__malloc_msg)


    AC_MSG_CHECKING([for __attribute__ ((may_alias))])
    AC_CACHE_VAL(ac_cv___attribute__may_alias, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int * arg __attribute__ ((may_alias));],
        [],
        [ac_cv___attribute__may_alias=1],
        [ac_cv___attribute__may_alias=0]
      )

      if test "$ac_cv___attribute__may_alias" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             // No need to declare extern "C"
             int * arg __attribute__ ((may_alias));],
            [],
            [ac_cv___attribute__may_alias=1 ac_cv___attribute__may_alias_msg=yes],
            [ac_cv___attribute__may_alias=0 ac_cv___attribute__may_alias_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__may_alias_msg)


    AC_MSG_CHECKING([for __attribute__ ((nonnull))])
    AC_CACHE_VAL(ac_cv___attribute__nonnull, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int square(int *arg) __attribute__ ((nonnull));
         int square(int *arg) { return *arg; }],
        [
         int value=1;
         value = square (&value);
        ],
        [ac_cv___attribute__nonnull=1],
        [ac_cv___attribute__nonnull=0]
      )

      if test "$ac_cv___attribute__nonnull" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int square(int *arg) __attribute__ ((nonnull));
               int square(int *arg) { return *arg; }
             }
            ],
            [
             int value=1;
             value = square (&value);
            ],
            [ac_cv___attribute__nonnull=1 ac_cv___attribute__nonnull_msg=yes],
            [ac_cv___attribute__nonnull=0 ac_cv___attribute__nonnull_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__nonnull_msg)


    AC_MSG_CHECKING([for __attribute__ ((noreturn))])
    AC_CACHE_VAL(ac_cv___attribute__noreturn, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int fatal(int arg1) __attribute__ ((noreturn));
         int fatal(int arg1) { exit(arg1); }],
        [],
        [ac_cv___attribute__noreturn=1],
        [ac_cv___attribute__noreturn=0]
      )

      if test "$ac_cv___attribute__noreturn" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int fatal(int arg1) __attribute__ ((noreturn));
               int fatal(int arg1) { exit(arg1); }
             }
            ],
            [],
            [ac_cv___attribute__noreturn=1 ac_cv___attribute__noreturn_msg=yes],
            [ac_cv___attribute__noreturn=0 ac_cv___attribute__noreturn_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__noreturn_msg)


    AC_MSG_CHECKING([for __attribute__ ((packed))])
    AC_CACHE_VAL(ac_cv___attribute__packed, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         struct foo {
             char a;
             int x[2] __attribute__ ((packed));
         };],
        [],
        [ac_cv___attribute__packed=1],
        [ac_cv___attribute__packed=0]
      )

      if test "$ac_cv___attribute__packed" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             // No need to declare extern "C"
             struct foo {
                 char a;
                 int x[2] __attribute__ ((packed));
             };
            ],
            [],
            [ac_cv___attribute__packed=1 ac_cv___attribute__packed_msg=yes],
            [ac_cv___attribute__packed=0 ac_cv___attribute__packed_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__packed_msg)


    AC_MSG_CHECKING([for __attribute__ ((pure))])
    AC_CACHE_VAL(ac_cv___attribute__pure, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int square(int arg) __attribute__ ((pure));
         int square(int arg) { return arg * arg; }],
        [],
        [ac_cv___attribute__pure=1],
        [ac_cv___attribute__pure=0]
      )

      if test "$ac_cv___attribute__pure" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int square(int arg) __attribute__ ((pure));
               int square(int arg) { return arg * arg; }
             }
            ],
            [],
            [ac_cv___attribute__pure=1 ac_cv___attribute__pure_msg=yes],
            [ac_cv___attribute__pure=0 ac_cv___attribute__pure_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__pure_msg)


    AC_MSG_CHECKING([for __attribute__ ((sentinel))])
    AC_CACHE_VAL(ac_cv___attribute__sentinel, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int my_execlp(const char * file, const char *arg, ...) __attribute__ ((sentinel));],
        [],
        [ac_cv___attribute__sentinel=1],
        [ac_cv___attribute__sentinel=0]
      )

      if test "$ac_cv___attribute__sentinel" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int my_execlp(const char * file, const char *arg, ...) __attribute__ ((sentinel));
             }
            ],
            [],
            [ac_cv___attribute__sentinel=1 ac_cv___attribute__sentinel_msg=yes],
            [ac_cv___attribute__sentinel=0 ac_cv___attribute__sentinel_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])

    AC_MSG_RESULT($ac_cv___attribute__sentinel_msg)


    AC_MSG_CHECKING([for __attribute__ ((unused))])
    AC_CACHE_VAL(ac_cv___attribute__unused, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int square(int arg1 __attribute__ ((unused)), int arg2);
         int square(int arg1, int arg2) { return arg2; }],
        [],
        [ac_cv___attribute__unused=1],
        [ac_cv___attribute__unused=0]
      )

      if test "$ac_cv___attribute__unused" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int square(int arg1 __attribute__ ((unused)), int arg2);
               int square(int arg1, int arg2) { return arg2; }
             }
            ],
            [],
            [ac_cv___attribute__unused=1 ac_cv___attribute__unused_msg=yes],
            [ac_cv___attribute__unused=0 ac_cv___attribute__unused_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__unused_msg)


    AC_MSG_CHECKING([for __attribute__ ((visibility))])
    AC_CACHE_VAL(ac_cv___attribute__visibility, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int square(int arg1) __attribute__ ((visibility("hidden")));],
        [],
        [ac_cv___attribute__visibility=1],
        [ac_cv___attribute__visibility=0]
      )

      if test "$ac_cv___attribute__visibility" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int square(int arg1) __attribute__ ((visibility("hidden")));
             }
            ],
            [],
            [ac_cv___attribute__visibility=1 ac_cv___attribute__visibility_msg=yes],
            [ac_cv___attribute__visibility=0 ac_cv___attribute__visibility_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__visibility_msg)


    AC_MSG_CHECKING([for __attribute__ ((warn_unused_result))])
    AC_CACHE_VAL(ac_cv___attribute__warn_unused_result, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int foo(int arg) __attribute__ ((warn_unused_result));
         int foo(int arg) { return arg + 3; }
         int foo2(int arg) { return foo (arg); } ],
        [],
        [ac_cv___attribute__warn_unused_result=1],
        [ac_cv___attribute__warn_unused_result=0]
      )

      if test "$ac_cv___attribute__warn_unused_result" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int foo(int arg) __attribute__ ((warn_unused_result));
               int foo(int arg) { return arg + 3; }
               int foo2(int arg) { return foo (arg); }
             }
            ],
            [],
            [ac_cv___attribute__warn_unused_result=1 ac_cv___attribute__warn_unused_result_msg=yes],
            [ac_cv___attribute__warn_unused_result=0 ac_cv___attribute__warn_unused_result_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__warn_unused_result_msg)


    AC_MSG_CHECKING([for __attribute__ ((weak, alias..))])
    AC_CACHE_VAL(ac_cv___attribute__weak_alias, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
           int foo(int arg);
           int foo(int arg) { return arg + 3; }
           int foo2(int arg) __attribute__ ((weak, alias("foo")));],
        [],
        [ac_cv___attribute__weak_alias=1],
        [ac_cv___attribute__weak_alias=0]
      )

      if test "$ac_cv___attribute__weak_alias" = "1" ; then
          AC_LANG_PUSH(C++)
          AC_TRY_COMPILE(
            [#include <stdlib.h>
             extern "C" {
               int foo(int arg) { return arg + 3; }
               int foo2(int) __attribute__ ((weak, alias("foo")));
             }],
            [],
            [ac_cv___attribute__weak_alias=1 ac_cv___attribute__weak_alias_msg=yes],
            [ac_cv___attribute__weak_alias=0 ac_cv___attribute__weak_alias_msg=no]
          )
          AC_LANG_POP(C++)
      fi
      ])
    AC_MSG_RESULT($ac_cv___attribute__weak_alias_msg)

  fi

  # Now that all the values are set, define them

  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_ALIGNED, [$ac_cv___attribute__aligned],
                     [Whether your compiler has __attribute__ aligned or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_ALWAYS_INLINE, [$ac_cv___attribute__always_inline],
                     [Whether your compiler has __attribute__ always_inline or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_CONST, [$ac_cv___attribute__const],
                     [Whether your compiler has __attribute__ const or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_DEPRECATED, [$ac_cv___attribute__deprecated],
                     [Whether your compiler has __attribute__ deprecated or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_FORMAT, [$ac_cv___attribute__format],
                     [Whether your compiler has __attribute__ format or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_MALLOC, [$ac_cv___attribute__malloc],
                     [Whether your compiler has __attribute__ malloc or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_MAY_ALIAS, [$ac_cv___attribute__may_alias],
                     [Whether your compiler has __attribute__ may_alias or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_NONNULL, [$ac_cv___attribute__nonnull],
                     [Whether your compiler has __attribute__ nonnull or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_NORETURN, [$ac_cv___attribute__noreturn],
                     [Whether your compiler has __attribute__ noreturn or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_PACKED, [$ac_cv___attribute__packed],
                     [Whether your compiler has __attribute__ packed or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_PURE, [$ac_cv___attribute__pure],
                     [Whether your compiler has __attribute__ pure or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_SENTINEL, [$ac_cv___attribute__sentinel],
                     [Whether your compiler has __attribute__ sentinel or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_UNUSED, [$ac_cv___attribute__unused],
                     [Whether your compiler has __attribute__ unused or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_VISIBILITY, [$ac_cv___attribute__visibility],
                     [Whether your compiler has __attribute__ visibility or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_WARN_UNUSED_RESULT, [$ac_cv___attribute__warn_unused_result],
                     [Whether your compiler has __attribute__ warn unused result or not])
  AC_DEFINE_UNQUOTED(OMPI_HAVE_ATTRIBUTE_WEAK_ALIAS, [$ac_cv___attribute__weak_alias],
                     [Whether your compiler has __attribute__ weak alias or not])
])
