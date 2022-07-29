dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow

dnl OAC_IFBLANK: conditionally evaluate condition based on existance of argument
dnl
dnl 1 -> condition to check
dnl 2 -> if-blank
dnl 3 -> if-not-blank
dnl
dnl If condition is empty or consists only of blanks (space, tab, dnl
dnl newline), then expand if-blank.  Otherwise, expand if-not-blank.
dnl This macro is slightly more in-depth than m4_ifnblank, in that it
dnl will also handle the shell-evaluation time case.  That is, if the
dnl condition is clearly blank (ie, $1 contains no characters other than
dnl space, tab, and newline), then it evaluates if-blank.  If the
dnl condition is clearly not blank (ie, $1 contains other chacters and is
dnl not a shell variable) then it evaluates if-not-blank.  If the
dnl condition looks like it might be a shell variable, it emits shell code
dnl to conditionally execute either if-blank or if-not-blank depending on
dnl the results of test -z.

AC_DEFUN([OAC_IFBLANK], [
    m4_ifblank([$1],
        [$2],
        [AS_LITERAL_IF([$1],
             [$3],
             [AS_IF([test -z "$1"], [$2], [$3])])])
])dnl


dnl OAC_IFNBLANK: Inverse of OAC_IFBLANK
dnl
dnl $1 -> condition to check
dnl $2 -> if-not-blank
dnl $3 -> if-blank
dnl
dnl Check if condition is not blank.  This is the inverse of OAC_IFBLANK
dnl and is here to avoid hurting programmer's brains with argument ordering.
AC_DEFUN([OAC_IFNBLANK], [
    OAC_IFBLANK([$1], [$3], [$2])
])dnl
