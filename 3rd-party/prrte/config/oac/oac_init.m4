dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$

dnl OAC is meant to be a "behind the scenes" kind of thing -- we don't
dnl any any AC_DEFINEd or AC_SUBSTed symbols that begin with "OAC_".
m4_pattern_forbid([OAC_])

dnl OAC_PUSH_PREFIX: Set a new prefix for AC_DEFINE/AC_SUBST names
dnl
dnl 1 -> new prefix
dnl
dnl Override (prehaps temporarily) the prefix name specified to
dnl OAC_INIT.
AC_DEFUN([OAC_PUSH_PREFIX],
[m4_pushdef([_oac_program_prefix], [$1])])


dnl OAC_POP_PREFIX: Undo last OAC_PUSH_PREFIX
dnl
dnl <no arguments>
dnl
dnl Undo the last call to OAC_PUSH_PREFIX.
AC_DEFUN([OAC_POP_PREFIX],
[m4_popdef([_oac_program_prefix])])
