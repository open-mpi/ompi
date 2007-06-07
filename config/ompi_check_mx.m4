# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2007 The University of Tennessee and The University
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

AC_DEFUN([_OMPI_CHECK_MX_EXTENSIONS],[
    #
    # Check if the MX library provide the necessary functions in order to
    # figure out the mapper version and MAC for each board.
    #

    AC_MSG_CHECKING([for a MX version with mx_open_board])
    AC_TRY_LINK([#include <mx_extensions.h>
                 #include <mx_io.h>
                 #include <mx_internals/mx__fops.h>
                 ],
                 [mx_open_board(0, NULL);],
                 [mx_provide_open_board="yes"],
                 [mx_provide_open_board="no"])
    AC_MSG_RESULT([$mx_provide_open_board])

    AC_MSG_CHECKING([for a MX version with mx__get_mapper_state])
    AC_TRY_LINK([#include <mx_extensions.h>
                 #include <mx_io.h>
                 #include <mx_internals/mx__driver_interface.h>
                 ],
             [mx__get_mapper_state(NULL, NULL);],
             [mx_provide_get_mapper_state="yes"],
             [mx_provide_get_mapper_state="no"])
    AC_MSG_RESULT([$mx_provide_get_mapper_state])

    AS_IF([test x"$mx_provide_open_board" = "xyes" -a "$mx_provide_get_mapper_state" = "yes"],
          [mx_provide_mapper_state=1
           $2],
          [mx_provide_mapper_state=0
           $3])
    AC_DEFINE_UNQUOTED([MX_HAVE_MAPPER_STATE], [$mx_provide_mapper_state],
                       [MX installation provide access to the mx_open_board and mx__get_mapper_state functions])
    unset mx_provide_open_board
    unset mx_provide_get_mapper_state
    unset mx_provide_mapper_state
])

AC_DEFUN([_OMPI_CHECK_MX_UNEXP_HANDLER],[
    #
    # Check if the MX library provide the mx_register_unexp_handler function.
    # With this function, OMPI can avoid having a double matching logic
    # (one on the MX library and one on OMPI) by registering our own matching
    # function. Moreover, we can handle all eager messages with just one
    # memcpy.

    AC_MSG_CHECKING([for a MX version with mx_register_unexp_handler])
    AC_TRY_LINK([#include <mx_extensions.h>],
             [mx_register_unexp_handler(0, 0, 0);],
             [mx_provide_unexp_handler="yes"],
             [mx_provide_unexp_handler="no"])
    AC_MSG_RESULT([$mx_provide_unexp_handler])
    AS_IF([test x"$mx_provide_unexp_handler" = "xyes"],
          [mx_provide_unexp_handler=1
           $2],
          [mx_provide_unexp_handler=0
           $3])
    AC_DEFINE_UNQUOTED([MX_HAVE_UNEXPECTED_HANDLER], [$mx_provide_unexp_handler],
                       [MX allow registration of an unexpected handler])
    unset mx_provide_unexp_handler
])

AC_DEFUN([_OMPI_CHECK_MX_FORGET],[
    #
    # Check if the MX library provide the mx_forget function.
    #
    AC_MSG_CHECKING([for a MX version with mx_forget])
    AC_TRY_LINK([#include <mx_extensions.h>],
             [mx_forget(0, 0);],
             [mx_provide_forget="yes"],
             [mx_provide_forget="no"])
    AC_MSG_RESULT([$mx_provide_forget])
    AS_IF([test x"$mx_provide_forget" = "xyes"],
          [mx_provide_forget=1
           $2],
          [mx_provide_forget=0
           $3])
    AC_DEFINE_UNQUOTED([MX_HAVE_FORGET], [$mx_provide_forget],
                       [MX allow to forget the completion event for mx_requests])
    unset mx_provide_forget
])

AC_DEFUN([_OMPI_CHECK_MX_CONFIG],[
    #
    # See if we have MX_API. OpenMPI require a MX version bigger than
    # the first MX relase (0x300)
    #

    # restored at end of OMPI_CHECK_MX
    CPPFLAGS="$ompi_check_mx_$1_save_CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$ompi_check_mx_$1_save_LDFLAGS $$1_LDFLAGS"
    LIBS="$ompi_check_mx_$1_save_LIBS $$1_LIBS"

    AC_MSG_CHECKING([for MX version 1.0 or later])
    AC_TRY_COMPILE([#include <myriexpress.h>],
        [#if MX_API < 0x300
         #error "Version less than 0x300"
         #endif],
         [have_recent_api="yes"],
         [have_recent_api="no"])
    AC_MSG_RESULT([$have_recent_api])

    AC_MSG_CHECKING(for MX_API)
    AS_IF([test x"$have_recent_api" = "xyes"],
          [AC_DEFINE_UNQUOTED([OMPI_MX_API_VERSION], $mx_api_ver,
                [Version of the MX API to use])
           unset mx_api_ver have_mx_api_ver_msg found val msg
           $2],
          [$3])
    AC_MSG_RESULT([$mx_api_ver])
    # Check for the mx_extensions.h
    AC_MSG_CHECKING([for mx_extensions.h header])
    AC_TRY_COMPILE([#include <mx_extensions.h>],
         [return 0;],
         [have_mx_extensions="yes"],
         [have_mx_extensions="no"])
    AS_IF([test x"$have_mx_extensions" = "xyes"],
          [have_mx_extensions=1],
          [have_mx_extensions=0
           AC_MSG_WARN([The MX support for Open MPI will be compiled without the
                        MX extensions. This will result on lower performances.
                        Please install the MX library > 1.2.0 to increase the
                        performance of your cluster])
          ])
    AC_DEFINE_UNQUOTED([MX_HAVE_EXTENSIONS_H], [$have_mx_extensions],
                       [The MX library have support for the mx_extensions.h])
    unset have_recent_api have_mx_extensions
])dnl

# OMPI_CHECK_MX(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if MX support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_MX],[
    AC_ARG_WITH([mx],
        [AC_HELP_STRING([--with-mx(=DIR)],
             [Build MX (Myrinet Express) support, searching for libraries in DIR])])
    AC_ARG_WITH([mx-libdir],
        [AC_HELP_STRING([--with-mx-libdir=DIR],
             [Search for MX (Myrinet Express) libraries in DIR])])

    ompi_check_mx_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_mx_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_mx_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_mx" != "no"],
          [AS_IF([test ! -z "$with_mx" -a "$with_mx" != "yes"],
                 [ompi_check_mx_dir="$with_mx"])
           AS_IF([test ! -z "$with_mx_libdir" -a "$with_mx_libdir" != "yes"],
                 [ompi_check_mx_libdir="$with_mx_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [myriexpress.h],
                              [myriexpress],
                              [mx_finalize],
                              [],
                              [$ompi_check_mx_dir],
                              [$ompi_check_mx_libdir],
                              [ompi_check_mx_happy="yes"],
                              [ompi_check_mx_happy="no"])],
          [ompi_check_mx_happy="no"])

    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [_OMPI_CHECK_MX_CONFIG($1, [ompi_check_mx_happy="yes"],
                                 [ompi_check_mx_happy="no"])])
    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [_OMPI_CHECK_MX_UNEXP_HANDLER($1, [ompi_check_mx_register="yes"],
                                        [ompi_check_mx_register="no"])])
    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [_OMPI_CHECK_MX_FORGET($1, [ompi_check_mx_forget="yes"],
                                 [ompi_check_mx_forget="no"])])
    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [_OMPI_CHECK_MX_EXTENSIONS( $1, [ompi_check_mx_extensions="yes"],
                                          [ompi_check_mx_extensions="no"])])

    CPPFLAGS="$ompi_check_mx_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_mx_$1_save_LDFLAGS"
    LIBS="$ompi_check_mx_$1_save_LIBS"

    AS_IF([test "$ompi_check_mx_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([MX driver does not currently support progress threads.  Disabling BTL.])
           ompi_check_mx_happy="no"])

    AS_IF([test "$ompi_check_mx_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_mx" -a "$with_mx" != "no"],
                 [AC_MSG_ERROR([MX support requested but not found.  Aborting])])
           $3])
])

