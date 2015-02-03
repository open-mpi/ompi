dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
dnl defines:
dnl   OPAL_$1_USE_PRAGMA_IDENT
dnl   OPAL_$1_USE_IDENT
dnl   OPAL_$1_USE_CONST_CHAR_IDENT
dnl

# OPAL_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, lang) Try to compile a source file containing
# a #pragma ident, and determine whether the ident was
# inserted into the resulting object file
# -----------------------------------------------------------
AC_DEFUN([OPAL_CHECK_IDENT], [
    AC_MSG_CHECKING([for $4 ident string support])

    opal_pragma_ident_happy=0
    opal_ident_happy=0
    opal_static_const_char_happy=0
    _OPAL_CHECK_IDENT(
        [$1], [$2], [$3],
        [[#]pragma ident], [],
        [opal_pragma_ident_happy=1
         opal_message="[#]pragma ident"],
        _OPAL_CHECK_IDENT(
            [$1], [$2], [$3],
            [[#]ident], [],
            [opal_ident_happy=1
             opal_message="[#]ident"],
            _OPAL_CHECK_IDENT(
                [$1], [$2], [$3],
                [[#]pragma comment(exestr, ], [)],
                [opal_pragma_comment_happy=1
                 opal_message="[#]pragma comment"],
                [opal_static_const_char_happy=1
                 opal_message="static const char[[]]"])))

    AC_DEFINE_UNQUOTED([OPAL_$1_USE_PRAGMA_IDENT],
        [$opal_pragma_ident_happy], [Use #pragma ident strings for $4 files])
    AC_DEFINE_UNQUOTED([OPAL_$1_USE_IDENT],
        [$opal_ident_happy], [Use #ident strings for $4 files])
    AC_DEFINE_UNQUOTED([OPAL_$1_USE_PRAGMA_COMMENT],
        [$opal_pragma_comment_happy], [Use #pragma comment for $4 files])
    AC_DEFINE_UNQUOTED([OPAL_$1_USE_CONST_CHAR_IDENT],
        [$opal_static_const_char_happy], [Use static const char[] strings for $4 files])

    AC_MSG_RESULT([$opal_message])

    unset opal_pragma_ident_happy opal_ident_happy opal_static_const_char_happy opal_message
])

# _OPAL_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, header_prefix, header_suffix, action-if-success, action-if-fail)
# Try to compile a source file containing a #-style ident,
# and determine whether the ident was inserted into the
# resulting object file
# -----------------------------------------------------------
AC_DEFUN([_OPAL_CHECK_IDENT], [
    eval opal_compiler="\$$1"
    eval opal_flags="\$$2"

    opal_ident="string_not_coincidentally_inserted_by_the_compiler"
    cat > conftest.$3 <<EOF
#define IDENT_MSG "$opal_ident"
$4 IDENT_MSG $5
int main(int argc, char** argv);
int main(int argc, char** argv) { return 0; }
EOF

    # "strings" won't always return the ident string.  objdump isn't
    # universal (e.g., OS X doesn't have it), and ...other
    # complications.  So just try to "grep" for the string in the
    # resulting object file.  If the ident is found in "strings" or
    # the grep succeeds, rule that we have this flavor of ident.

    OPAL_LOG_COMMAND([$opal_compiler $opal_flags -c conftest.$3 -o conftest.${OBJEXT}],
                     [AS_IF([test -f conftest.${OBJEXT}],
                            [opal_output="`strings -a conftest.${OBJEXT} | grep $opal_ident`"
                             grep $opal_ident conftest.${OBJEXT} 2>&1 1>/dev/null
                             opal_status=$?
                             AS_IF([test "$opal_output" != "" || test "$opal_status" = "0"],
                                   [$6],
                                   [$7])],
                            [OPAL_LOG_MSG([the failed program was:])
                             OPAL_LOG_FILE([conftest.$3])
                             $7]
                            [$7])])

    unset opal_compiler opal_flags opal_output opal_status
    rm -rf conftest.* conftest${EXEEXT}
])dnl
