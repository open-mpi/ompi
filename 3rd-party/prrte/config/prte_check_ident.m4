dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
dnl defines:
dnl   PRTE_$1_USE_PRAGMA_IDENT
dnl   PRTE_$1_USE_IDENT
dnl   PRTE_$1_USE_CONST_CHAR_IDENT
dnl

# PRTE_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, lang) Try to compile a source file containing
# a #pragma ident, and determine whether the ident was
# inserted into the resulting object file
# -----------------------------------------------------------
AC_DEFUN([PRTE_CHECK_IDENT], [
    AC_MSG_CHECKING([for $4 ident string support])

    prte_pragma_ident_happy=0
    prte_ident_happy=0
    prte_static_const_char_happy=0
    _PRTE_CHECK_IDENT(
        [$1], [$2], [$3],
        [[#]pragma ident], [],
        [prte_pragma_ident_happy=1
         prte_message="[#]pragma ident"],
        _PRTE_CHECK_IDENT(
            [$1], [$2], [$3],
            [[#]ident], [],
            [prte_ident_happy=1
             prte_message="[#]ident"],
            _PRTE_CHECK_IDENT(
                [$1], [$2], [$3],
                [[#]pragma comment(exestr, ], [)],
                [prte_pragma_comment_happy=1
                 prte_message="[#]pragma comment"],
                [prte_static_const_char_happy=1
                 prte_message="static const char[[]]"])))

    AC_DEFINE_UNQUOTED([PRTE_$1_USE_PRAGMA_IDENT],
        [$prte_pragma_ident_happy], [Use #pragma ident strings for $4 files])
    AC_DEFINE_UNQUOTED([PRTE_$1_USE_IDENT],
        [$prte_ident_happy], [Use #ident strings for $4 files])
    AC_DEFINE_UNQUOTED([PRTE_$1_USE_PRAGMA_COMMENT],
        [$prte_pragma_comment_happy], [Use #pragma comment for $4 files])
    AC_DEFINE_UNQUOTED([PRTE_$1_USE_CONST_CHAR_IDENT],
        [$prte_static_const_char_happy], [Use static const char[] strings for $4 files])

    AC_MSG_RESULT([$prte_message])

    unset prte_pragma_ident_happy prte_ident_happy prte_static_const_char_happy prte_message
])

# _PRTE_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, header_prefix, header_suffix, action-if-success, action-if-fail)
# Try to compile a source file containing a #-style ident,
# and determine whether the ident was inserted into the
# resulting object file
# -----------------------------------------------------------
AC_DEFUN([_PRTE_CHECK_IDENT], [
    eval prte_compiler="\$$1"
    eval prte_flags="\$$2"

    prte_ident="string_not_coincidentally_inserted_by_the_compiler"
    cat > conftest.$3 <<EOF
$4 "$prte_ident" $5
int main(int argc, char** argv);
int main(int argc, char** argv) { return 0; }
EOF

    # "strings" won't always return the ident string.  objdump isn't
    # universal (e.g., OS X doesn't have it), and ...other
    # complications.  So just try to "grep" for the string in the
    # resulting object file.  If the ident is found in "strings" or
    # the grep succeeds, rule that we have this flavor of ident.

    echo "configure:__oline__: $1" >&5
    prte_output=`$prte_compiler $prte_flags -c conftest.$3 -o conftest.${OBJEXT} 2>&1 1>/dev/null`
    prte_status=$?
    AS_IF([test $prte_status = 0],
          [test -z "$prte_output"
           prte_status=$?])
    PRTE_LOG_MSG([\$? = $prte_status], 1)
    AS_IF([test $prte_status = 0 && test -f conftest.${OBJEXT}],
          [prte_output="`strings -a conftest.${OBJEXT} | grep $prte_ident`"
           grep $prte_ident conftest.${OBJEXT} 2>&1 1>/dev/null
           prte_status=$?
           AS_IF([test "$prte_output" != "" || test "$prte_status" = "0"],
                 [$6],
                 [$7])],
          [PRTE_LOG_MSG([the failed program was:])
           PRTE_LOG_FILE([conftest.$3])
           $7])

    unset prte_compiler prte_flags prte_output prte_status
    rm -rf conftest.* conftest${EXEEXT}
])dnl
