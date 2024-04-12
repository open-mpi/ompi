dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl PRTE_TRY_ASSEMBLE(asm-code, [action-if-success], [action-if-fail])
dnl
dnl Attempt to assemble asm-code.  If success, run action-if-success.
dnl Otherwise, run action-if-fail.  Neither action-if-success nor
dnl action-if-fail are required.
dnl
dnl No preprocessing is guaranteed to be done on asm-code.  Some
dnl compilers do not run the preprocessor on assembly files.
dnl
dnl On failure, asm-test.s will be included in config.out
AC_DEFUN([PRTE_TRY_ASSEMBLE],
[cat >conftest.s <<EOF
[$1]
EOF
if test "$CC" = "$CCAS" ; then
    prte_assemble="$CCAS $CCASFLAGS -c conftest.s >conftest.out 2>&1"
else
    prte_assemble="$CCAS $CCASFLAGS -o conftest.o conftest.s >conftest.out 2>&1"
fi
if AC_TRY_EVAL(prte_assemble); then
  # save the warnings
  cat conftest.out >&AS_MESSAGE_LOG_FD
  ifelse([$2],,:,[$2])
else
  # save compiler output and failed program
  cat conftest.out >&AS_MESSAGE_LOG_FD
  echo "configure: failed program was:" >&AS_MESSAGE_LOG_FD
  cat conftest.s >&AS_MESSAGE_LOG_FD
  ifelse([$3],,:,[$3])
fi
rm -rf conftest*
unset prte_assemble
])dnl
