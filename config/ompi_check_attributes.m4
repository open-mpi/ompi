# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
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
      [#include <stdlib.h>],
      [static void foo(void) __attribute__ ((unused));
       static void foo(void) { exit(1); }],
      [ac_cv___attribute__=1 ac_cv___attribute__msg=yes],
      [ac_cv___attribute__=0 ac_cv___attribute__msg=no],
    )])
  AC_DEFINE_UNQUOTED( HAVE___ATTRIBUTE__, [$ac_cv___attribute__],
                      [define if your compiler has __attribute__] )
  AC_MSG_RESULT($ac_cv___attribute__msg)

#
# Now that we know the compiler support __attribute__ let's check which kind of
# attributed are supported.
#
  if test "$ac_cv___attribute__msg" = "yes"; then
    AC_MSG_CHECKING("for __attribute__ ((alias))" )
    AC_CACHE_VAL(ac_cv___attribute__alias, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int foo(int arg) { return arg + 3; }
         int foo2(int) __attribute__ ((weak, alias ("foo")));],
        [],
        [ac_cv___attribute__alias=1 ac_cv___attribute__alias_msg=yes],
        [ac_cv___attribute__alias=0 ac_cv___attribute__alias_msg=no]
      )])
    AC_DEFINE_UNQUOTED( HAVE___ATTRIBUTE__WEAK_ALIAS, [$ac_cv___attribute__alias],
                        [define if your compiler has __attribute__ alias weak] )
    AC_MSG_RESULT($ac_cv___attribute__alias_msg)
    

    AC_MSG_CHECKING("for __attribute__ ((const))" )
    AC_CACHE_VAL(ac_cv___attribute__const, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int foo(int arg1, int arg2) __attribute__ ((const)) { return arg1 * arg2 + arg1; }],
        [],
        [ac_cv___attribute__const=1 ac_cv___attribute__const_msg=yes],
        [ac_cv___attribute__const=0 ac_cv___attribute__const_msg=no]
      )])
    AC_DEFINE_UNQUOTED( HAVE___ATTRIBUTE__CONST, [$ac_cv___attribute__const],
                        [define if your compiler has __attribute__ const] )
    AC_MSG_RESULT($ac_cv___attribute__const_msg)

    AC_MSG_CHECKING("for __attribute__ ((malloc))" )
    AC_CACHE_VAL(ac_cv___attribute__malloc, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int foo(int arg1) __attribute__ ((malloc)) { return malloc(arg1); }],
        [],
        [ac_cv___attribute__malloc=1 ac_cv___attribute__malloc_msg=yes],
        [ac_cv___attribute__malloc=0 ac_cv___attribute__malloc_msg=no]
      )])
    AC_DEFINE_UNQUOTED( HAVE___ATTRIBUTE__MALLOC, [$ac_cv___attribute__malloc],
                        [define if your compiler has __attribute__ malloc] )
    AC_MSG_RESULT($ac_cv___attribute__malloc_msg)

    AC_MSG_CHECKING("for __attribute__ ((noreturn))" )
    AC_CACHE_VAL(ac_cv___attribute__noreturn, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int fatal(int arg1) __attribute__ ((noreturn)) { exit(arg1); }],
        [],
        [ac_cv___attribute__noreturn=1 ac_cv___attribute__noreturn_msg=yes],
        [ac_cv___attribute__noreturn=0 ac_cv___attribute__noreturn_msg=no]
      )])
    AC_DEFINE_UNQUOTED( HAVE___ATTRIBUTE__NORETURN, [$ac_cv___attribute__noreturn],
                        [define if your compiler has __attribute__ noreturn] )
    AC_MSG_RESULT($ac_cv___attribute__noreturn_msg)

    AC_MSG_CHECKING("for __attribute__ ((pure))" )
    AC_CACHE_VAL(ac_cv___attribute__pure, [
      AC_TRY_COMPILE(
        [#include <stdlib.h>
         int square(int arg) __attribute__ ((pure)) { return arg * arg; }],
        [],
        [ac_cv___attribute__pure=1 ac_cv___attribute__pure_msg=yes],
        [ac_cv___attribute__pure=0 ac_cv___attribute__pure_msg=no]
      )])
    AC_DEFINE_UNQUOTED( HAVE___ATTRIBUTE__PURE, [$ac_cv___attribute__PURE],
                        [define if your compiler has __attribute__ PURE] )
    AC_MSG_RESULT($ac_cv___attribute__pure_msg)

  fi
])
