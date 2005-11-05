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


# _MCA_pml_portals_config_val(config_name, define_name, 
#                             default_val, descrtiption)
# -----------------------------------------------------
AC_DEFUN([MCA_pml_portals_CONFIG_VAL], [
    AC_ARG_WITH([portals-$1], AC_HELP_STRING([--with-portals-$1], 
                [$4 (default: $3)]))
    case "[$with_]m4_bpatsubst([portals-$1], -, _)" in
        "")
            $2=$3
            ;;
        "no")
            AC_MSG_ERROR([--without-portals-$1 is invalid argument])
            ;;
        *)
            $2="[$with_]m4_bpatsubst([portals-$1], -, _)"
            ;;
    esac
    AC_DEFINE_UNQUOTED([$2], [[$]$2], [$4])
])


# _MCA_pml_portals_CONFIG_VALS()
# ------------------------------
AC_DEFUN([MCA_pml_portals_CONFIG_VALS], [
    # User configuration options
    MCA_pml_portals_CONFIG_VAL([debug-level],
        [OMPI_PML_PORTALS_DEFAULT_DEBUG_LEVEL], [0],
        [debugging level for portals pml])

    MCA_pml_portals_CONFIG_VAL([eager-limit],
        [OMPI_PML_PORTALS_DEFAULT_EAGER_LIMIT], [32768],
        [max size for eager sends])

    MCA_pml_portals_CONFIG_VAL([min-send-size],
        [OMPI_PML_PORTALS_DEFAULT_MIN_SEND_SIZE], [32768],
        [min size for send fragments])
    MCA_pml_portals_CONFIG_VAL([max-send-size],
        [OMPI_PML_PORTALS_DEFAULT_MAX_SEND_SIZE], [65536],
        [max size for send fragments])

    MCA_pml_portals_CONFIG_VAL([md-size],
        [OMPI_PML_PORTALS_DEFAULT_RECV_MD_SIZE], [1048576],
        [Size of receive memory descriptors])
    MCA_pml_portals_CONFIG_VAL([md-size],
        [OMPI_PML_PORTALS_DEFAULT_RECV_MD_NUM], [3],
        [Number of receive memory descriptors])

    MCA_pml_portals_CONFIG_VAL([min-rdma-size],
        [OMPI_PML_PORTALS_DEFAULT_MIN_RDMA_SIZE], [65536],
        [min size for rdma fragments])
    MCA_pml_portals_CONFIG_VAL([max-rdma-size],
        [OMPI_PML_PORTALS_DEFAULT_MAX_RDMA_SIZE], [2147483647],
        [max size for rdma fragments])

    MCA_pml_portals_CONFIG_VAL([max-sends-pending],
        [OMPI_PML_PORTALS_MAX_SENDS_PENDING], [128],
        [max number of sends pending at any time])
    MCA_pml_portals_CONFIG_VAL([recv-queue-size],
        [OMPI_PML_PORTALS_DEFAULT_RECV_QUEUE_SIZE], [512],
        [size of event queue for receiving frags])

    MCA_pml_portals_CONFIG_VAL([free-list-init-num],
        [OMPI_PML_PORTALS_DEFAULT_FREE_LIST_INIT_NUM], [8],
        [starting size of free lists])
    MCA_pml_portals_CONFIG_VAL([free-list-max-num],
        [OMPI_PML_PORTALS_DEFAULT_FREE_LIST_MAX_NUM], [1024],
        [maximum size of free lists])
    MCA_pml_portals_CONFIG_VAL([free-list-inc-num],
        [OMPI_PML_PORTALS_DEFAULT_FREE_LIST_INC_NUM], [32],
        [grow size for freelists])
])


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
            pml_portals_LIBS="-lutcpapi -lutcplib -lp3api -lp3lib -lp3rt"
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
            pml_portals_starting_table_id=30
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
               MCA_pml_portals_CONFIG_VALS()          
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
