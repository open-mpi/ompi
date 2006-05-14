# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# _MCA_btl_portals_CONFIG_PLATFORM()
# ----------------------------------
AC_DEFUN([MCA_btl_portals_CONFIG_PLATFORM], [
    # Configure Portals for our local environment
    BTL_PORTALS_UTCP=0
    BTL_PORTALS_REDSTORM=0
    BTL_PORTALS_COMPAT=""
    BTL_PORTALS_HAVE_EVENT_UNLINK=0
    btl_portals_compat="none"
    btl_portals_header_prefix=
    btl_portals_starting_table_id=0
    AC_ARG_WITH([portals-config],
            AC_HELP_STRING([--with-portals-config],
                           [configuration to use for Portals support.
                            One of "utcp", "redstorm".  (default: utcp)]))
    AC_MSG_CHECKING([for Portals configuration])
    if test "$with_portals_config" = "" ; then
        with_portals_config="utcp"
    fi
    case "$with_portals_config" in
        "utcp")
            BTL_PORTALS_UTCP=1
            BTL_PORTALS_HAVE_EVENT_UNLINK=1
            btl_portals_LIBS="-lp3utcp -lp3api -lp3lib -lp3rt -lp3utcp"
            btl_portals_compat="utcp"
            btl_portals_header_prefix=
            btl_portals_starting_table_id=0
            AC_MSG_RESULT([utcp])
            ;;
        "redstorm")
            BTL_PORTALS_REDSTORM=1
            BTL_PORTALS_HAVE_EVENT_UNLINK=0
            btl_portals_LIBS=
            btl_portals_compat="redstorm"
            btl_portals_header_prefix="portals/"
            btl_portals_starting_table_id=30
            AC_MSG_RESULT([red storm])
            ;;
        *)
            # ok to call ERROR here - the user specified something invalid.
            # that should be brought to his attention
            AC_MSG_ERROR([unknown Portals configuration.  Can not continue])
            ;;
    esac

    # Try to find all the portals libraries (this is not fun!)
    AC_ARG_WITH([portals-libs], 
        [AC_HELP_STRING([--with-portals-libs=LIBS],
                       [Libraries to link with for portals])])
    if test -n "$with_portals_libs" ; then
        btl_portals_LIBS=""
        for lib in $with_portals_libs ; do
            btl_portals_LIBS="$btl_portals_LIBS -l$lib"
        done
    fi

    AC_DEFINE_UNQUOTED([OMPI_BTL_PORTALS_HAVE_EVENT_UNLINK], 
                        [$BTL_PORTALS_HAVE_EVENT_UNLINK],
                        [Does Portals send a BTL_EVENT_UNLINK event])

    AC_DEFINE_UNQUOTED([OMPI_BTL_PORTALS_UTCP], [$BTL_PORTALS_UTCP],
                       [Use the UTCP reference implementation or Portals])
    AC_DEFINE_UNQUOTED([OMPI_BTL_PORTALS_REDSTORM], [$BTL_PORTALS_REDSTORM],
                       [Use the Red Storm implementation or Portals])

    AC_DEFINE_UNQUOTED([OMPI_BTL_PORTALS_STARTING_TABLE_ID],
                       [$btl_portals_starting_table_id],
                       [first table id to use for portals btl])

    AC_CONFIG_LINKS([ompi/mca/btl/portals/btl_portals_compat.c:ompi/mca/btl/portals/btl_portals_compat_${btl_portals_compat}.c])
])


# MCA_btl_portals_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_btl_portals_CONFIG],[
    # save compiler flags so that we don't alter them for later
    # components.
    btl_portals_save_CPPFLAGS="$CPPFLAGS"
    btl_portals_save_LDFLAGS="$LDFLAGS"
    btl_portals_save_LIBS="$LIBS"

    # allow user a way to say where the Portals installation is
    AC_ARG_WITH(portals, 
        AC_HELP_STRING([--with-portals=DIR],
                       [Specify the installation directory of PORTALS]))

    AS_IF([test -n "$with_portals"],
          [AS_IF([test -d "$with_portals/include"],
                 [btl_portals_CPPFLAGS="-I$with_portals/include"
                  CPPFLAGS="$CPPFLAGS $btl_portals_CPPFLAGS"], [])
           AS_IF([test -d "$with_portals/lib"],
                 [btl_portals_LDFLAGS="-L$with_portals/lib"
                  LDFLAGS="$LDFLAGS $btl_portals_LDFLAGS"], [])])

    # try to get our platform configuration
    MCA_btl_portals_CONFIG_PLATFORM()

    # check for portals
    LIBS="$LIBS $btl_portals_LIBS"
    AC_CHECK_HEADERS([${btl_portals_header_prefix}portals3.h],
        [AC_MSG_CHECKING([if possible to link Portals application])
         AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <${btl_portals_header_prefix}portals3.h>], 
                                         [int i; PtlInit(&i);])],
              [AC_MSG_RESULT([yes])
               btl_portals_WRAPPER_EXTRA_LDFLAGS="$btl_portals_LDFLAGS"
               btl_portals_WRAPPER_EXTRA_LIBS="$btl_portals_LIBS"
               $1],
              [AC_MSG_RESULT([no])
               $2])],
        [$2])

    # substitute in the things needed to build Portals
    AC_SUBST([btl_portals_CPPFLAGS])
    AC_SUBST([btl_portals_LDFLAGS])
    AC_SUBST([btl_portals_LIBS])

    # reset the flags for the next test
    CPPFLAGS="$btl_portals_save_CPPFLAGS"
    LDFLAGS="$btl_portals_save_LDFLAGS"
    LIBS="$btl_portals_save_LIBS"
])dnl
