# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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


# _MCA_pml_portals_CONFIG_PLATFORM()
# ----------------------------------
AC_DEFUN([MCA_pml_portals_CONFIG_PLATFORM], [
    # Configure Portals for our local environment
    PML_PORTALS_UTCP=0
    PML_PORTALS_REDSTORM=0
    PML_PORTALS_COMPAT=""
    PML_PORTALS_HAVE_EVENT_UNLINK=0
    pml_portals_compat="none"
    pml_portals_header_prefix=
    pml_portals_starting_table_id=0
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
            PML_PORTALS_UTCP=1
            PML_PORTALS_HAVE_EVENT_UNLINK=1
            pml_portals_LIBS="-lp3utcp -lp3api -lp3lib -lp3rt -lp3utcp"
            pml_portals_compat="utcp"
            pml_portals_header_prefix=
            pml_portals_starting_table_id=0
            AC_MSG_RESULT([utcp])
            ;;
        "redstorm")
            PML_PORTALS_REDSTORM=1
            PML_PORTALS_HAVE_EVENT_UNLINK=0
            pml_portals_LIBS=
            pml_portals_compat="redstorm"
            pml_portals_header_prefix="portals/"
            pml_portals_starting_table_id=32
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
        pml_portals_LIBS=""
        for lib in $with_portals_libs ; do
            pml_portals_LIBS="$pml_portals_LIBS -l$lib"
        done
    fi

    AC_DEFINE_UNQUOTED([OMPI_PML_PORTALS_HAVE_EVENT_UNLINK], 
                        [$PML_PORTALS_HAVE_EVENT_UNLINK],
                        [Does Portals send a PML_EVENT_UNLINK event])

    AC_DEFINE_UNQUOTED([OMPI_PML_PORTALS_UTCP], [$PML_PORTALS_UTCP],
                       [Use the UTCP reference implementation or Portals])
    AC_DEFINE_UNQUOTED([OMPI_PML_PORTALS_REDSTORM], [$PML_PORTALS_REDSTORM],
                       [Use the Red Storm implementation or Portals])

    AC_DEFINE_UNQUOTED([OMPI_PML_PORTALS_STARTING_TABLE_ID],
                       [$pml_portals_starting_table_id],
                       [first table id to use for portals pml])
])


# MCA_pml_portals_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pml_portals_CONFIG],[
    # save compiler flags so that we don't alter them for later
    # components.
    pml_portals_save_CPPFLAGS="$CPPFLAGS"
    pml_portals_save_LDFLAGS="$LDFLAGS"
    pml_portals_save_LIBS="$LIBS"

    AC_ARG_ENABLE([pml-portals],
                  [AC_HELP_STRING([--enable-pml-portals],
                                  [Enable building of experimental Portals PML (default: disabled)])])
  AS_IF([test "$enable_pml_portals" = "yes"], [
    # allow user a way to say where the Portals installation is
    AC_ARG_WITH(portals, 
        AC_HELP_STRING([--with-portals=DIR],
                       [Specify the installation directory of PORTALS]))

    AS_IF([test -n "$with_portals"],
          [AS_IF([test -d "$with_portals/include"],
                 [pml_portals_CPPFLAGS="-I$with_portals/include"
                  CPPFLAGS="$CPPFLAGS $pml_portals_CPPFLAGS"], [])
           AS_IF([test -d "$with_portals/lib"],
                 [pml_portals_LDFLAGS="-L$with_portals/lib"
                  LDFLAGS="$LDFLAGS $pml_portals_LDFLAGS"], [])])

    # try to get our platform configuration
    MCA_pml_portals_CONFIG_PLATFORM()

    # check for portals
    LIBS="$LIBS $pml_portals_LIBS"
    AC_CHECK_HEADERS([${pml_portals_header_prefix}portals3.h],
        [AC_MSG_CHECKING([if possible to link Portals application])
         AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <${pml_portals_header_prefix}portals3.h>], 
                                         [int i; PtlInit(&i);])],
              [AC_MSG_RESULT([yes])
               pml_portals_WRAPPER_EXTRA_LDFLAGS="$pml_portals_LDFLAGS"
               pml_portals_WRAPPER_EXTRA_LIBS="$pml_portals_LIBS"
               $1],
              [AC_MSG_RESULT([no])
               $2])],
        [$2])
  ], [$2])

    # substitute in the things needed to build Portals
    AC_SUBST([pml_portals_CPPFLAGS])
    AC_SUBST([pml_portals_LDFLAGS])
    AC_SUBST([pml_portals_LIBS])

    # reset the flags for the next test
    CPPFLAGS="$pml_portals_save_CPPFLAGS"
    LDFLAGS="$pml_portals_save_LDFLAGS"
    LIBS="$pml_portals_save_LIBS"
])dnl
