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
# Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_OPENIB(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if OPENIB support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_OPENIB],[
    AC_ARG_WITH([openib],
        [AC_HELP_STRING([--with-openib(=DIR)],
             [Build OpenFabrics support, searching for libraries in DIR])])
    AC_ARG_WITH([openib-libdir],
       [AC_HELP_STRING([--with-openib-libdir=DIR],
             [Search for OpenFabrics libraries in DIR])])

    AS_IF([test ! -z "$with_openib" -a "$with_openib" != "yes"],
          [ompi_check_openib_dir="$with_openib"])
    AS_IF([test ! -z "$with_openib_libdir" -a "$with_openib_libdir" != "yes"],
          [ompi_check_openib_libdir="$with_openib_libdir"])
    AS_IF([test "$with_openib" != "no"],
        [ # check for pthreads and emit a warning that things might go south...
         AS_IF([test "$HAVE_POSIX_THREADS" != "1"],
               [AC_MSG_WARN([POSIX threads not enabled.  May not be able to link with OpenFabrics])])

         ompi_check_openib_$1_save_CPPFLAGS="$CPPFLAGS"
         ompi_check_openib_$1_save_LDFLAGS="$LDFLAGS"
         ompi_check_openib_$1_save_LIBS="$LIBS"

         AC_CHECK_HEADER([sysfs/libsysfs.h],
                         [ompi_check_openib_sysfs_h=yes],
                         [ompi_check_openib_sysfs_h=no])

         AS_IF([test "$ompi_check_openib_sysfs_h" != "yes"],
               [AS_IF([test ! -z "$with_openib" -a "$with_openib" != "no"],
                      [AC_MSG_ERROR([OpenFabrics support requested (via --with-openib) but required sysfs/libsysfs.h not found.  Aborting])])])
    
         AC_CHECK_LIB([sysfs], 
	              [sysfs_open_class], 
                      [ompi_check_openib_sysfs=yes
                       LIBS="$LIBS -lsysfs"
                       $1_LIBS="-lsysfs"], 
		      [ompi_check_openib_sysfs=no])

         AS_IF([test "$ompi_check_openib_sysfs" != "yes"],
               [AS_IF([test ! -z "$with_openib" -a "$with_openib" != "no"],
                      [AC_MSG_ERROR([OpenFabrics support requested (via --with-openib) but required sysfs not found.  Aborting])])])

         OMPI_CHECK_PACKAGE([$1],
                            [infiniband/verbs.h],
                            [ibverbs],
                            [ibv_open_device],
                            [],
                            [$ompi_check_openib_dir],
                            [$ompi_check_openib_libdir],
                            [ompi_check_openib_happy="yes"],
                            [ompi_check_openib_happy="no"])

         # ok, now see if ibv_create_cq takes 3 arguments or 6
         CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
         LDFLAGS="$LDFLAGS $$1_LDFLAGS"
         LIBS="$LIBS $$1_LIBS"

         AS_IF([test "$ompi_check_openib_happy" = "yes"],
            [AC_CACHE_CHECK(
              [number of arguments to ibv_create_cq],
              [ompi_cv_func_ibv_create_cq_args],
              [AC_LINK_IFELSE(
                  [AC_LANG_PROGRAM(
                     [[#include <infiniband/verbs.h> ]],
                     [[ibv_create_cq(NULL, 0, NULL, NULL, 0);]])],
                  [ompi_cv_func_ibv_create_cq_args=5],
                  [AC_LINK_IFELSE(
                    [AC_LANG_PROGRAM(
                       [[#include <infiniband/verbs.h> ]],
                       [[ibv_create_cq(NULL, 0, NULL);]])],
                     [ompi_cv_func_ibv_create_cq_args=3],
                     [ompi_cv_func_ibv_create_cq_args="unknown"])])])
            AS_IF([test "$ompi_cv_func_ibv_create_cq_args" = "unknown"],
                  [AC_MSG_ERROR([Can not determine number of args to ibv_create_cq.  Aborting])],
                  [AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_IBV_CREATE_CQ_ARGS],
                                      [$ompi_cv_func_ibv_create_cq_args],
                                      [Number of arguments to ibv_create_cq])])])

         AC_CHECK_FUNCS([ibv_create_srq], [ompi_check_openib_have_srq=1], [ompi_check_openib_have_srq=0])
         AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_HAVE_SRQ],
                            [$ompi_check_openib_have_srq],
		            [Whether install of OpenFabrics includes shared receive queue support])

         AC_CHECK_FUNCS([ibv_get_device_list],
                        [ompi_check_openib_have_device_list=1],
                        [ompi_check_openib_have_device_list=0])
         AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_HAVE_DEVICE_LIST],
                        [$ompi_check_openib_have_device_list],
                        [Whether install of OpenFabrics includes ibv_get_device_list API])

	 AC_CHECK_FUNCS([ibv_resize_cq], [ompi_check_openib_have_resize_cq=1], [ompi_check_openib_have_resize_cq=0])
         AC_DEFINE_UNQUOTED([OMPI_MCA_]m4_translit([$1], [a-z], [A-Z])[_HAVE_RESIZE_CQ],
                            [$ompi_check_openib_have_resize_cq],
			    [Whether install of OpenFabrics includes resize completion queue support])
			    
         CPPFLAGS="$ompi_check_openib_$1_save_CPPFLAGS"
         LDFLAGS="$ompi_check_openib_$1_save_LDFLAGS"
         LIBS="$ompi_check_openib_$1_save_LIBS"],
        [ompi_check_openib_happy="no"])  

    AS_IF([test "$ompi_check_openib_happy" = "yes" -a "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([The OpenFabrics BTL driver does not currently support progress threads.  Disabling BTL.])
           ompi_check_openib_happy="no"])

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_openib" -a "$with_openib" != "no"],
                 [AC_MSG_ERROR([OpenFabrics support requested (via --with-openib) but not found.  Aborting])])
           $3])
])

