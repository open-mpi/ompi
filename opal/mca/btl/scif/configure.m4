# -*- shell-script -*-
#
# Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_btl_scif_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([opal_btl_scif_happy])
    AC_CONFIG_FILES([opal/mca/btl/scif/Makefile])

    AC_ARG_WITH([scif], [AC_HELP_STRING([--with-scif(=DIR)]),
		[Build with SCIF, searching for headers in DIR])])
    OPAL_CHECK_WITHDIR([scif], [$with_scif], [include/scif.h])

    opal_btl_scif_happy="no"

    if test "$with_scif" != "no" ; then
	if test -n "$with_scif" && test "$with_scif" != "yes" ; then
	    opal_check_scif_dir=$with_scif
	fi

	OPAL_CHECK_PACKAGE([btl_scif], [scif.h], [scif], [scif_open], [],
	                   [$opal_check_scif_dir], [], [opal_btl_scif_happy="yes"], [])

	if test "$opal_btl_scif_happy" != "yes" && test -n "$with_scif" ; then
	    AC_MSG_ERROR([SCIF support requested but not found.  Aborting])
	fi
    fi

    AS_IF([test "$opal_btl_scif_happy" = "yes"], [$1], [$2])

    # substitute in the things needed to build scif
    AC_SUBST([btl_scif_CPPFLAGS])
    AC_SUBST([btl_scif_LDFLAGS])
    AC_SUBST([btl_scif_LIBS])
    OPAL_VAR_SCOPE_POP
])dnl
