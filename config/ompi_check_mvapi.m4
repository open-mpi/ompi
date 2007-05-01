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


# OMPI_CHECK_MVAPI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if MVAPI support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_MVAPI],[
    AC_ARG_WITH([mvapi],
        [AC_HELP_STRING([--with-mvapi(=DIR)],
             [Build MVAPI (InfiniBand) support, searching for libraries in DIR])])
    AC_ARG_WITH([mvapi-libdir],
       [AC_HELP_STRING([--with-mvapi-libdir=DIR],
             [Search for MVAPI (InfiniBand) libraries in DIR])])

    AS_IF([test ! -z "$with_mvapi" -a "$with_mvapi" != "yes"],
          [ompi_check_mvapi_dir="$with_mvapi"])
    AS_IF([test ! -z "$with_mvapi_libdir" -a "$with_mvapi_libdir" != "yes"],
          [ompi_check_mvapi_libdir="$with_mvapi_libdir"])
    AS_IF([test "$with_mvapi" = "no"],
          [ompi_check_mvapi_happy="no"],
          [ompi_check_mvapi_happy="yes"])

    AS_IF([test "$ompi_check_mvapi_happy" = "yes"],
          [AS_IF([test "$THREAD_TYPE" != "posix" -a "$memory_ptmalloc2_happy" = "yes"],
                 [AC_MSG_WARN([POSIX Threads disabled but PTMalloc2 enabled.])
                  AC_MSG_WARN([This will cause memory corruption with MVAPI.])
                  AC_MSG_WARN([Not building component.])
                  ompi_check_mvapi_happy="no"])])

    ompi_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_package_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_package_$1_save_LIBS="$LIBS"

    ompi_check_package_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    ompi_check_package_$1_orig_LDFLAGS="$$1_LDFLAGS"
    ompi_check_package_$1_orig_LIBS="$$1_LIBS"

    # ugly hack for topspin which stores include files in include/vapi
    AS_IF([test -d "$ompi_check_mvapi_dir/include/vapi"],  
	  [CPPFLAGS="$CPPFLAGS -I $ompi_check_mvapi_dir/include/vapi"
           $1_CPPFLAGS="$$1_CPPFLAGS -I $ompi_check_mvapi_dir/include/vapi"]) 

    AS_IF([test "$ompi_check_mvapi_happy" = "yes"],
          [_OMPI_CHECK_PACKAGE_HEADER([$1], 
                [vapi.h],
                [$ompi_check_mvapi_dir],
                [ompi_check_mvapi_happy="yes"],
                [ompi_check_mvapi_happy="no"])])

    # some mellanox vapi implemenations only need lvapi and lmosal,
    # some also need lmpga and lmtl_common
    AS_IF([test "$ompi_check_mvapi_happy" = "yes"],
          [_OMPI_CHECK_PACKAGE_LIB([$1],
                [vapi],
                [VAPI_open_hca],
                [-lmosal],
                [$ompi_check_mvapi_dir],
                [$ompi_check_mvapi_libdir],
                [ompi_check_mvapi_happy="yes"],
                [_OMPI_CHECK_PACKAGE_LIB([$1],
                      [vapi],
                      [VAPI_open_hca],
                      [-lmosal -lmpga -lmtl_common],
                      [$ompi_check_mvapi_dir],
                      [$ompi_check_mvapi_libdir],
                      [ompi_check_mvapi_happy="yes"],
                      [ompi_check_mvapi_happy="no"])])])

    CPPFLAGS="$ompi_check_package_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_package_$1_save_LDFLAGS"
    LIBS="$ompi_check_package_$1_save_LIBS"

    AS_IF([test "$ompi_check_mvapi_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([MVAPI driver does not currently support progress threads.  Disabling component.])
           ompi_check_mvapi_happy="no"])

    AS_IF([test "$ompi_check_mvapi_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_mvapi" -a "$with_mvapi" != "no"],
                 [AC_MSG_ERROR([MVAPI support requested but not found.  Aborting])])
           $3])
])

