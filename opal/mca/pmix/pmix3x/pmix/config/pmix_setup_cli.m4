dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2016      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2017-2018 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2018-2020 Cisco, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl PMIX_CAPTURE_CONFIGURE_CLI
dnl
dnl Capture configure command line and do the AC substitution
dnl
dnl Arguments: the variable in which command line will be captured
dnl
dnl Dependencies: None
dnl
AC_DEFUN([PMIX_CAPTURE_CONFIGURE_CLI],[
    # Capture configure command line do the AC substitution
    PMIX_VAR_SCOPE_PUSH([sed_quote_subst arg quoted_arg])
    $1=
    for arg in "$[]@"; do
        sed_quote_subst='s/\(@<:@`"\\@:>@\)/\\\1/g'
        case "$arg" in
          *@<:@\\\`\"\$[]@:>@*)
	    quoted_arg=\'`echo "$arg" | sed $sed_quote_subst`\' ;;
          *)
            quoted_arg="\'$arg\'" ;;
        esac

        eval "$1=\$$1\\ \$quoted_arg"
    done
    AC_DEFINE_UNQUOTED([$1], ["$$1"], [Capture the configure cmd line])
    PMIX_VAR_SCOPE_POP
    AC_SUBST($1)
])
