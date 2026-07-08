dnl -*- shell-script -*-
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
dnl Copyright (c) 2014      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# _OPAL_C_WEAK_SYMBOLS(action_if_found, [action_if_not_found])
# ------------------------------------------------------------
# This tests for support of plain weak symbols: a symbol that is
# *defined* weakly (via "#pragma weak") can be overridden by a strong
# definition of the same symbol without provoking a duplicate-symbol
# link error.  This is a distinct linker feature from weak symbol
# *aliases* (see OPAL_C_WEAK_ALIASES); notably, macOS / Mach-O supports
# weak symbols but does *not* support weak aliases.
AC_DEFUN([_OPAL_C_WEAK_SYMBOLS],[
    # Use two files because icc will incorrectly not create the symbols
    # if they are not used in the object file in which they are defined.
    # To get it to compile with icc, put the declaration in a separate
    # header.
    cat > conftest_weak.h <<EOF
int real(int i);
EOF

    # conftest_weak.c provides a *weak* definition of real().
    cat > conftest_weak.c <<EOF
#include "conftest_weak.h"
#pragma weak real
int real(int i) { return i + 42; }
EOF

    # conftest.c provides a *strong* definition of real() that must
    # override the weak one at link time.
    cat > conftest.c <<EOF
#include "conftest_weak.h"
int real(int i) { return i; }
int main() { return real(3); }
EOF

# Try the compile
OPAL_LOG_COMMAND(
    [$CC $CFLAGS  -c conftest_weak.c],
    [OPAL_LOG_COMMAND(
        [$CC $CFLAGS  conftest.c conftest_weak.o -o conftest $LDFLAGS $LIBS],
        [opal_c_weak_symbols_happy=1],
        [opal_c_weak_symbols_happy=0])],
    [opal_c_weak_symbols_happy=0])

    AS_IF([test "$opal_c_weak_symbols_happy" = "1"], [$1], [$2])

    unset opal_c_weak_symbols_happy
    rm -f conftest_weak.h conftest_weak.c conftest.c conftest
])


# OPAL_C_WEAK_SYMBOLS()
# ---------------------
# sets OPAL_C_HAVE_WEAK_SYMBOLS=1 if C compiler has support for weak
# symbols
AC_DEFUN([OPAL_C_WEAK_SYMBOLS],[
    AC_CACHE_CHECK([for weak symbol support],
                   [opal_cv_c_weak_symbols],
                   [_OPAL_C_WEAK_SYMBOLS([opal_cv_c_weak_symbols="yes"],
                                         [opal_cv_c_weak_symbols="no"])])

    AS_IF([test "$opal_cv_c_weak_symbols" = "yes"],
          [OPAL_C_HAVE_WEAK_SYMBOLS=1], [OPAL_C_HAVE_WEAK_SYMBOLS=0])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_WEAK_SYMBOLS], [$OPAL_C_HAVE_WEAK_SYMBOLS],
        [Whether the C compiler supports plain weak symbols or not])
]) dnl
