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

    AC_ARG_WITH([memkind], [AS_HELP_STRING([--with-memkind(=DIR)]),
		[Build with MEMKIND, searching for headers in DIR])])

    #
    # memkind_create_kind was introduced with memkind v1.4.0 release
    #
    OAC_CHECK_PACKAGE([memkind],
                      [mpool_memkind],
                      [memkind.h],
                      [memkind -lnuma],
                      [memkind_create_kind],
                      [opal_mpool_memkind_happy="yes"],
                      [opal_mpool_memkind_happy="no"])

    AS_IF([test "$ompi_mpool_memkind_happy" = "yes"],
          [$1],
          [AS_IF([test -n "$with_memkind" && test "$with_memkind" != "no"],
                 [AC_MSG_ERROR([Lustre support requested but not found.  Aborting])])
           $2])

    # substitute in the things needed to build memkind
    AC_SUBST([mpool_memkind_CPPFLAGS])
    AC_SUBST([mpool_memkind_LDFLAGS])
    AC_SUBST([mpool_memkind_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
