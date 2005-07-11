# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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


# _MCA_btl_portals_config_val(config_name, define_name, 
#                             default_val, descrtiption)
# -----------------------------------------------------
AC_DEFUN([MCA_btl_portals_CONFIG_VAL], [
    AC_ARG_WITH([btl-portals-$1], AC_HELP_STRING([--with-btl-portals-$1], 
                [$4 (default: $3)]))
    AC_MSG_CHECKING([for $1 value])
    case "[$with_]m4_bpatsubst([btl-portals-$1], -, _)" in
        "")
            $2=$3
            AC_MSG_RESULT([[$]$2 (default)])
            ;;
        "no")
            AC_MSG_RESULT([error])
            AC_MSG_ERROR([--without-btl-portals-$1 is invalid argument])
            ;;
        *)
            $2="[$with_]m4_bpatsubst([btl-portals-$1], -, _)"
            AC_MSG_RESULT([[$]$2])
            ;;
    esac
    AC_DEFINE_UNQUOTED([$2], [[$]$2], [$4])
])


# _MCA_btl_portals_CONFIG_VALS()
# ------------------------------
AC_DEFUN([MCA_btl_portals_CONFIG_VALS], [
    # User configuration options
    MCA_btl_portals_CONFIG_VAL([send-table-id],
        [BTL_PORTALS_SEND_TABLE_ID], [3],
        [Portals table id to use for send/recv ])

    MCA_btl_portals_CONFIG_VAL([rdma-table-id],
        [BTL_PORTALS_RDMA_TABLE_ID], [4],
        [Portals table id to use for RDMA request])

    MCA_btl_portals_CONFIG_VAL([debug-level],
        [BTL_PORTALS_DEFAULT_DEBUG_LEVEL], [100],
        [debugging level for portals btl])

    MCA_btl_portals_CONFIG_VAL([eager-limit],
        [BTL_PORTALS_DEFAULT_EAGER_LIMIT], [16384],
        [max size for eager sends])

    MCA_btl_portals_CONFIG_VAL([min-send-size],
        [BTL_PORTALS_DEFAULT_MIN_SEND_SIZE], [0],
        [min size for send fragments])
    MCA_btl_portals_CONFIG_VAL([max-send-size],
        [BTL_PORTALS_DEFAULT_MAX_SEND_SIZE], [32768],
        [max size for send fragments])

    MCA_btl_portals_CONFIG_VAL([min-rdma-size],
        [BTL_PORTALS_DEFAULT_MIN_RDMA_SIZE], [0],
        [min size for rdma fragments])
    MCA_btl_portals_CONFIG_VAL([max-rdma-size],
        [BTL_PORTALS_DEFAULT_MAX_RDMA_SIZE], [2147483647],
        [max size for rdma fragments])

    MCA_btl_portals_CONFIG_VAL([max-sends-pending],
        [BTL_PORTALS_MAX_SENDS_PENDING], [128],
        [max number of sends pending at any time])
    MCA_btl_portals_CONFIG_VAL([recv-queue-size],
        [BTL_PORTALS_DEFAULT_RECV_QUEUE_SIZE], [512],
        [size of event queue for receiving frags])

    MCA_btl_portals_CONFIG_VAL([free-list-init-num],
        [BTL_PORTALS_DEFAULT_FREE_LIST_INIT_NUM], [8],
        [starting size of free lists])
    MCA_btl_portals_CONFIG_VAL([free-list-max-num],
        [BTL_PORTALS_DEFAULT_FREE_LIST_MAX_NUM], [1024],
        [maximum size of free lists])
    MCA_btl_portals_CONFIG_VAL([free-list-inc-num],
        [BTL_PORTALS_DEFAULT_FREE_LIST_INC_NUM], [32],
        [grow size for freelists])
])


# _MCA_btl_portals_CONFIG_PLATFORM()
# ----------------------------------
AC_DEFUN([MCA_btl_portals_CONFIG_PLATFORM], [
    # Configure Portals for our local environment
    BTL_PORTALS_UTCP=0
    BTL_PORTALS_REDSTORM=0
    BTL_PORTALS_COMPAT=""
    BTL_PORTALS_HAVE_EVENT_UNLINK=0
    AC_ARG_WITH([btl-portals-config],
            AC_HELP_STRING([--with-btl-portals-config],
                           [configuration to use for Portals support.
                            One of "utcp", "redstorm".  (default: utcp)]))
    AC_MSG_CHECKING([for Portals configuration])
    if test "$with_btl_portals_config" = "" ; then
        with_btl_portals_config="utcp"
    fi
    case "$with_btl_portals_config" in
        "utcp")
            BTL_PORTALS_UTCP=1
            BTL_PORTALS_HAVE_EVENT_UNLINK=1
            btl_portals_LIBS="-lutcpapi -lutcplib -lp3api -lp3lib -lp3rt"
            AC_MSG_RESULT([utcp])
            ;;
        "redstorm")
            BTL_PORTALS_REDSTORM=1
            BTL_PORTALS_HAVE_EVENT_UNLINK=0
            btl_portals_LIBS="-lp3api -lp3lib -lp3rt"
            AC_MSG_RESULT([red storm])
            ;;
        *)
            # ok to call ERROR here - the user specified something invalid.
            # that should be brought to his attention
            AC_MSG_ERROR([unknown Portals configuration.  Can not continue])
            ;;
    esac

    # Try to find all the portals libraries (this is not fun!)
    AC_ARG_WITH(btl-portals-libs, 
        AC_HELP_STRING([--with-btl-portals-libs=LIBS],
                       [Libraries to link with for portals]))
    if test -n "$with_btl_portals_libs" ; then
        btl_portals_LIBS=""
        for lib in $with_btl_portals_libs ; do
            btl_portals_LIBS="$btl_portals_LIBS -l$lib"
        done
    fi

    AC_DEFINE_UNQUOTED([BTL_PORTALS_HAVE_EVENT_UNLINK], 
                        [$BTL_PORTALS_HAVE_EVENT_UNLINK],
                        [Does Portals send a BTL_EVENT_UNLINK event])

    AC_DEFINE_UNQUOTED([BTL_PORTALS_UTCP], [$BTL_PORTALS_UTCP],
                       [Use the UTCP reference implementation or Portals])
    AM_CONDITIONAL([BTL_PORTALS_UTCP], [test "$BTL_PORTALS_UTCP" = "1"])

    AC_DEFINE_UNQUOTED([BTL_PORTALS_REDSTORM], [$BTL_PORTALS_REDSTORM],
                       [Use the Red Storm implementation or Portals])
    AM_CONDITIONAL([BTL_PORTALS_REDSTORM], [test "$BTL_PORTALS_REDSTORM" = "1"])
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
    AC_ARG_WITH(btl-portals, 
        AC_HELP_STRING([--with-btl-portals=DIR],
                       [Specify the installation directory of PORTALS]))

    AS_IF([test -n "$with_btl_portals"],
          [AS_IF([test -d "$with_btl_portals/include"],
                 [btl_portals_CPPFLAGS="-I$with_btl_portals/include"
                  CPPFLAGS="$CPPFLAGS $btl_portals_CPPFLAGS"], [])
           AS_IF([test -d "$with_btl_portals/lib"],
                 [btl_portals_LDFLAGS="-L$with_btl_portals/lib"
                  LDFLAGS="$LDFLAGS $btl_portals_LDFLAGS"], [])])

    # try to get our platform configuration
    MCA_btl_portals_CONFIG_PLATFORM()

    # check for portals
    LIBS="$LIBS $btl_portals_LIBS"
    AC_CHECK_HEADERS([portals3.h],
        [AC_MSG_CHECKING([if possible to link Portals application])
         AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <portals3.h>], 
                                         [int i; PtlInit(&i);])],
              [AC_MSG_RESULT([yes])
               MCA_btl_portals_CONFIG_VALS()          
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
