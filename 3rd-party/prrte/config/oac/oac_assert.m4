dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow


dnl OAC_ASSERT_LITERAL: Assert if first argument is not an Autoconf literal
dnl
dnl 1 -> Variable which must be a literal
dnl 2 -> Argument reference string (usually an integer argument number)
dnl
dnl Assert that the first argument is a literal (in the Autoconf sense
dnl of the word) Second argument is the argument number (ie, a bare
dnl number) to make the error message easier to parse.
AC_DEFUN([OAC_ASSERT_LITERAL],
[AS_LITERAL_IF([$1], [], [m4_fatal([argument $2 ($1) must be a literal])])])dnl


dnl OAC_ASSERT_BEFORE: Assert the first argument is evaluated before
dnl                    the second argument
dnl
dnl 1 -> Macro which must be evaluated before second argument
dnl 2 -> Macro currently calling assert (for debugging print)
dnl
dnl Common usage would be similar to the commonly used check that
dnl OAC macros which require init to be called:
dnl OAC_ASSERT_BEFORE([OAC_INIT], [$0])
AC_DEFUN([OAC_ASSERT_BEFORE],
[AC_PROVIDE_IFELSE([$1], [], [m4_fatal([$1 must be evaluated before $2])])])dnl


dnl OAC_ASSERT_PREFIX_DEFINED: Assert that the OAC program prefix is defined
dnl
dnl 1 -> Calling macro name
dnl
dnl Generally only internally useful, but assert that the program prefix has
dnl been defined, so that the calling macro can rely on oac_program_prefix
dnl having a rational value.
AC_DEFUN([OAC_ASSERT_PREFIX_DEFINED],
[m4_ifdef([_oac_program_prefix], [],
          [m4_fatal([OAC prefix not defined.  Evaluate OAC_PUSH_PREFIX before evaluating $1])])])dnl
