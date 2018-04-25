# -*- shell-script -*-
#
# Copyright (c) 2013-2018 Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_mpool_memkind_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_mpool_memkind_happy])
    AC_CONFIG_FILES([opal/mca/mpool/memkind/Makefile])

    AC_ARG_WITH([memkind], [AC_HELP_STRING([--with-memkind(=DIR)]),
		[Build with MEMKIND, searching for headers in DIR])])
    OPAL_CHECK_WITHDIR([memkind], [$with_memkind], [include/memkind.h])

    opal_mpool_memkind_happy="no"

    if test "$with_memkind" != "no" ; then
	    if test -n "$with_memkind" -a "$with_memkind" != "yes" ; then
	        opal_check_memkind_dir=$with_memkind
	    fi

            #
            # memkind_create_kind was introduced with memkind v1.4.0 release
            #
            OPAL_CHECK_PACKAGE([mpool_memkind], [memkind.h], [memkind], [memkind_create_kind], [ -lnuma],
	        [$opal_check_memkind_dir], [], [opal_mpool_memkind_happy="yes"], [])

	    if test "$opal_mpool_memkind_happy" != "yes" -a -n "$with_memkind" ; then
	        AC_MSG_ERROR([MEMKIND support requested but not found.  Aborting])
	    fi
    fi

    AS_IF([test "$opal_mpool_memkind_happy" = "yes"], [$1], [$2])

    # substitute in the things needed to build memkind
    AC_SUBST([mpool_memkind_CPPFLAGS])
    AC_SUBST([mpool_memkind_LDFLAGS])
    AC_SUBST([mpool_memkind_LIBS])
    OPAL_VAR_SCOPE_POP
])dnl
