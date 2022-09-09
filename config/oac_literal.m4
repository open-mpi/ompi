dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
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
