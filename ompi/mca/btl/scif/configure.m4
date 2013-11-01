# -*- shell-script -*-
#
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_btl_scif_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/scif/Makefile])

    AC_ARG_WITH([scif], [AC_HELP_STRING([--with-scif(=DIR)]),
		[Build with SCIF, searching for headers in DIR])])
    OMPI_CHECK_WITHDIR([scif], [$with_scif], [include/scif.h])

    btl_scif_happy="no"

    if test "$with_scif" != "no" ; then
	if test -n "$with_scif" -a "$with_scif" != "yes" ; then
	    ompi_check_scif_dir=$with_scif
	fi

	OMPI_CHECK_PACKAGE([btl_scif], [scif.h], [scif], [scif_open], [],
	                   [$ompi_check_scif_dir], [], [btl_scif_happy="yes"], [])

	if test "$btl_scif_happy" != "yes" -a -n "$with_scif" ; then
	    AC_MSG_ERROR([SCIF support requested but not found.  Aborting])
	fi
    fi

    AS_IF([test "$btl_scif_happy" = "yes"], [$1], [$2])

    # substitute in the things needed to build scif
    AC_SUBST([btl_scif_CPPFLAGS])
    AC_SUBST([btl_scif_LDFLAGS])
    AC_SUBST([btl_scif_LIBS])
])dnl
