dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Intel, Inc. All rights reserved
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl
dnl defines:
dnl   PMIX_$1_USE_PRAGMA_IDENT
dnl   PMIX_$1_USE_IDENT
dnl   PMIX_$1_USE_CONST_CHAR_IDENT
dnl

# PMIX_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, lang) Try to compile a source file containing
# a #pragma ident, and determine whether the ident was
# inserted into the resulting object file
# -----------------------------------------------------------
AC_DEFUN([PMIX_CHECK_IDENT], [
    AC_MSG_CHECKING([for $4 ident string support])

    pmix_pragma_ident_happy=0
    pmix_ident_happy=0
    pmix_static_const_char_happy=0
    _PMIX_CHECK_IDENT(
        [$1], [$2], [$3],
        [[#]pragma ident], [],
        [pmix_pragma_ident_happy=1
         pmix_message="[#]pragma ident"],
        _PMIX_CHECK_IDENT(
            [$1], [$2], [$3],
            [[#]ident], [],
            [pmix_ident_happy=1
             pmix_message="[#]ident"],
            _PMIX_CHECK_IDENT(
                [$1], [$2], [$3],
                [[#]pragma comment(exestr, ], [)],
                [pmix_pragma_comment_happy=1
                 pmix_message="[#]pragma comment"],
                [pmix_static_const_char_happy=1
                 pmix_message="static const char[[]]"])))

    AC_DEFINE_UNQUOTED([PMIX_$1_USE_PRAGMA_IDENT],
        [$pmix_pragma_ident_happy], [Use #pragma ident strings for $4 files])
    AC_DEFINE_UNQUOTED([PMIX_$1_USE_IDENT],
        [$pmix_ident_happy], [Use #ident strings for $4 files])
    AC_DEFINE_UNQUOTED([PMIX_$1_USE_PRAGMA_COMMENT],
        [$pmix_pragma_comment_happy], [Use #pragma comment for $4 files])
    AC_DEFINE_UNQUOTED([PMIX_$1_USE_CONST_CHAR_IDENT],
        [$pmix_static_const_char_happy], [Use static const char[] strings for $4 files])

    AC_MSG_RESULT([$pmix_message])

    unset pmix_pragma_ident_happy pmix_ident_happy pmix_static_const_char_happy pmix_message
])

# _PMIX_CHECK_IDENT(compiler-env, compiler-flags,
# file-suffix, header_prefix, header_suffix, action-if-success, action-if-fail)
# Try to compile a source file containing a #-style ident,
# and determine whether the ident was inserted into the
# resulting object file
# -----------------------------------------------------------
AC_DEFUN([_PMIX_CHECK_IDENT], [
    eval pmix_compiler="\$$1"
    eval pmix_flags="\$$2"

    pmix_ident="string_not_coincidentally_inserted_by_the_compiler"
    cat > conftest.$3 <<EOF
$4 "$pmix_ident" $5
int main(int argc, char** argv);
int main(int argc, char** argv) { return 0; }
EOF

    # "strings" won't always return the ident string.  objdump isn't
    # universal (e.g., OS X doesn't have it), and ...other
    # complications.  So just try to "grep" for the string in the
    # resulting object file.  If the ident is found in "strings" or
    # the grep succeeds, rule that we have this flavor of ident.

    echo "configure:__oline__: $1" >&5
    pmix_output=`$pmix_compiler $pmix_flags -c conftest.$3 -o conftest.${OBJEXT} 2>&1 1>/dev/null`
    pmix_status=$?
    AS_IF([test $pmix_status = 0],
          [test -z "$pmix_output"
           pmix_status=$?])
    PMIX_LOG_MSG([\$? = $pmix_status], 1)
    AS_IF([test $pmix_status = 0 && test -f conftest.${OBJEXT}],
          [pmix_output="`strings -a conftest.${OBJEXT} | grep $pmix_ident`"
           grep $pmix_ident conftest.${OBJEXT} 2>&1 1>/dev/null
           pmix_status=$?
           AS_IF([test "$pmix_output" != "" || test "$pmix_status" = "0"],
                 [$6],
                 [$7])],
          [PMIX_LOG_MSG([the failed program was:])
           PMIX_LOG_FILE([conftest.$3])
           $7])

    unset pmix_compiler pmix_flags pmix_output pmix_status
    rm -rf conftest.* conftest${EXEEXT}
])dnl
