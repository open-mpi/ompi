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

    AS_IF([test "$with_mvapi" != "no"],
          [ # check for pthreads and emit a warning that
            # things might go south...
           AS_IF([test "$HAVE_POSIX_THREADS" != "1"],
                 [AC_MSG_WARN([POSIX threads not enabled.  May not be able to link with mvapi])])
    
           ompi_check_mvapi$1_save_CFLAGS="$CFLAGS"
           ompi_check_mvapi$1_save_CPPFLAGS="$CPPFLAGS"
    
           # ugly hack for topspin which stores include files in include/vapi
           AS_IF([test -d "$ompi_check_mvapi_dir/include/vapi"],  
	         [CPPFLAGS="$CPPFLAGS -I $ompi_check_mvapi_dir/include/vapi"
                  $1_CPPFLAGS="$$1_CPPFLAGS -I $ompi_check_mvapi_dir/include/vapi"]) 
    
           # some mellanox vapi implemenations only need lvapi and lmosal    
           OMPI_CHECK_PACKAGE([$1],
	          [vapi.h],
                  [vapi],
                  [VAPI_open_hca],
                  [-lmosal],
                  [$ompi_check_mvapi_dir],
                  [$ompi_check_mvapi_libdir],
                  [ompi_check_mvapi_happy="yes"],
                  [ompi_check_mvapi_happy="no"])

           # if needed use both lmpga and lmtl_common
           AS_IF([test "$ompi_check_mvapi_happy" = "no"], 
	         [OMPI_CHECK_PACKAGE([$1],
		        [vapi.h],
                        [vapi],
                        [VAPI_open_hca],
                        [-lmosal -lmpga -lmtl_common],
                        [$ompi_check_mvapi_dir],
                        [$ompi_check_mvapi_libdir],
                        [ompi_check_mvapi_happy="yes"],
                        [ompi_check_mvapi_happy="no"])])

           AS_IF([test "$ompi_check_mvapi_happy" = "yes" -a "$enable_progress_threads" = "yes"],
                 [AC_MSG_WARN([MVAPI driver does not currently support progress threads.  Disabling BTL.])
                  ompi_check_mvapi_happy="no"])

           CPPFLAGS="$ompi_check_mvapi$1_save_CPPFLAGS"],
          [ompi_check_mvapi_happy="no"])

    AS_IF([test "$ompi_check_mvapi_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_mvapi" -a "$with_mvapi" != "no"],
                 [AC_MSG_ERROR([MVAPI support requested but not found.  Aborting])])
           $3])
])

