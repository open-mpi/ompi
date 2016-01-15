# -*- shell-script -*-
#
# Copyright (c) 2010      The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_crs_dmtcp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_crs_dmtcp_CONFIG],[
    AC_CONFIG_FILES([opal/mca/crs/dmtcp/Makefile])

    OPAL_VAR_SCOPE_PUSH([opal_check_crs_dmtcp_good opal_opal_check_crs_dmtcp_save_CPPFLAGS opal_opal_check_crs_dmtcp_save_LDFLAGS opal_opal_check_crs_dmtcp_save_LIBS opal_check_crs_dmtcp_dir_msg opal_check_crs_dmtcp_libdir_msg opal_check_crs_dmtcp_dir opal_check_crs_dmtcp_libdir])


    opal_check_crs_dmtcp_good="no"

    # Configure option to specify where to look for DMTCP headers
    #   --with-dmtcp(=DIR)
    AC_ARG_WITH([dmtcp],
                [AC_HELP_STRING([--with-dmtcp(=DIR)],
                                [Path to DMTCP Installation])])
    OPAL_CHECK_WITHDIR([dmtcp], [$with_dmtcp], [include/mtcp.h])

    # Configure option to specify where to look for DMTCP libraries
    #   (Default: $with_dmtcp/lib)
    #   --with-dmtcp-libdir=DIR
    AC_ARG_WITH([dmtcp-libdir],
                [AC_HELP_STRING([--with-dmtcp-libdir=DIR],
                                [Search for DMTCP libraries in DIR])])
    OPAL_CHECK_WITHDIR([dmtcp-libdir], [$with_dmtcp_libdir], [libmtcp.so])

    #
    # Check if Open MPI was compiled with Checkpoint/Restart support
    # If not, then we do not compile this component
    #
    AS_IF([test "$opal_want_ft" = "0"],
          [opal_check_crs_dmtcp_good="no"],
          [opal_check_crs_dmtcp_good="yes"])

    #
    # Check if the user explicitly requested -not- to build the DMTCP component
    # If so, the we do not compile this component
    #
    AS_IF([test "$with_dmtcp" = "no" || test "$opal_check_crs_dmtcp_good" = "no"],
          [opal_check_crs_dmtcp_good="no"],
          [opal_check_crs_dmtcp_good="yes"])

    # Save some flags
    opal_opal_check_crs_dmtcp_save_CPPFLAGS=$CPPFLAGS
    opal_opal_check_crs_dmtcp_save_LDFLAGS=$LDFLAGS
    opal_opal_check_crs_dmtcp_save_LIBS=$LIBS

    #
    # Now to check if the library is usable
    #
    opal_check_crs_dmtcp_dir_msg="compiler default"
    opal_check_crs_dmtcp_libdir_msg="linker default"
    opal_check_crs_dmtcp_dir=""
    opal_check_crs_dmtcp_libdir=""

    # Determine the search paths for the headers and libraries
    AS_IF([test "$opal_check_crs_dmtcp_good" = "yes"],
          [AS_IF([test ! -z "$with_dmtcp" && test "$with_dmtcp" != "yes"],
                 [opal_check_crs_dmtcp_dir="$with_dmtcp"
                  opal_check_crs_dmtcp_dir_msg="$with_dmtcp (from --with-dmtcp)"])
           AS_IF([test ! -z "$with_dmtcp_libdir" && test "$with_dmtcp_libdir" != "yes"],
                 [opal_check_crs_dmtcp_libdir="$with_dmtcp_libdir"
                  opal_check_crs_dmtcp_libdir_msg="$with_dmtcp_libdir (from --with-dmtcp-libdir)"])
          ])

    # Look for DMTCP.
    AS_IF([test "$opal_check_crs_dmtcp_good" = "yes"],
          [AC_MSG_CHECKING([for DMTCP dir])
           AC_MSG_RESULT([$opal_check_crs_dmtcp_dir_msg])
           AC_MSG_CHECKING([for DMTCP library dir])
           AC_MSG_RESULT([$opal_check_crs_dmtcp_libdir_msg])
           OPAL_CHECK_PACKAGE([crs_dmtcp_check],
                              [mtcp.h],
                              [mtcp],
                              [mtcp_init],
                              [],
                              [$opal_check_crs_dmtcp_dir],
                              [$opal_check_crs_dmtcp_libdir],
                              [opal_check_crs_dmtcp_good="yes"],
                              [opal_check_crs_dmtcp_good="no"])
          ])

    # When we restart a thread, we use execlp() to exec the "mtcp_restart"
    # command.  We don't care what its path is, but it does need to exist in
    # the PATH.
    AC_CHECK_PROG([mtcp_restart_command_exists], ["mtcp_restart"], ["yes"], ["no"])
    AS_IF([test "$mtcp_restart_command_exists" = "no"],
          [opal_check_crs_dmtcp_good="no"
           AS_IF([test ! -z "$with_dmtcp" && test "$with_dmtcp" != "no"],
                 [AC_MSG_WARN([mtcp_restart not found in PATH.])
                  AC_MSG_ERROR([Aborting.])])])

    #
    # If '-lmtcp' or
    # '-I' or '-L' was needed to link to MTCP, then OPAL_CHECK_PACKAGE
    # sets the crs_mtcp_check_* variables, which we use below.
    #

    crs_dmtcp_CFLAGS="$CFLAGS $crs_dmtcp_check_CFLAGS"
    crs_dmtcp_CPPFLAGS="$CPPFLAGS $crs_dmtcp_check_CPPFLAGS"
    crs_dmtcp_LDFLAGS="$LDFLAGS $crs_dmtcp_check_LDFLAGS"
    crs_dmtcp_LIBS="$crs_dmtcp_check_LIBS $LIBS"

    AS_IF([test "$opal_check_crs_dmtcp_good" = "yes"],
          [$1])

    CPPFLAGS=$opal_opal_check_crs_dmtcp_save_CPPFLAGS
    LDFLAGS="$crs_dmtcp_check_LDFLAGS $opal_opal_check_crs_dmtcp_save_LDFLAGS"
    LIBS="$crs_dmtcp_LIBS $opal_opal_check_crs_dmtcp_save_LIBS"

    AC_SUBST([crs_dmtcp_CFLAGS])
    AC_SUBST([crs_dmtcp_CPPFLAGS])
    AC_SUBST([crs_dmtcp_LDFLAGS])
    AC_SUBST([crs_dmtcp_LIBS])

    # If all is good at this point then post any compiler options to
    # the build environment.  If all is not good at this point and
    # DMTCP was explicitly requested, then error out.

    AS_IF([test "$opal_check_crs_dmtcp_good" = "yes"],
          [$1],
          [AS_IF([test ! -z "$with_dmtcp" && test "$with_dmtcp" != "no"],
                 [AC_MSG_WARN([DMTCP support requested but not found.  Perhaps you need to specify the location of the DMTCP libraries.])
                  AC_MSG_ERROR([Aborting.])])
           $2])
    OPAL_VAR_SCOPE_POP
])dnl
