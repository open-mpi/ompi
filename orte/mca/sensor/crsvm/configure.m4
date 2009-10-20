# -*- shell-script -*-
#
# Copyright (c) 2009      The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2007      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_CLIB(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if clib (Eliots programming library) support can be found.
# sets prefix_{CPPFLAGS, LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_CLIB],[
    AC_ARG_WITH([clib],
        [AC_HELP_STRING([--with-clib(=DIR)],
                [Build CLIB (Eliots programming library) support, searching for libraries in DIR])])
    AC_ARG_WITH([clib-libdir],
        [AC_HELP_STRING([--with-clib-libdir=DIR],
                [Search for CLIB (Eliots programming library) libraries in DIR])])
    
    AS_IF([test "$with_clib" != "no"],
        [AS_IF([test ! -z "$with_clib" -a "$with_clib" != "yes"],
                [ompi_check_clib_dir="$with_clib"])
            AS_IF([test ! -z "$with_clib_libdir" -a "$with_clib_libdir" != "yes"],
                [ompi_check_clib_libdir="$with_clib_libdir"])
            
            OMPI_CHECK_PACKAGE([$1],
                [clib/error.h],
                [clib],
                [clib_error_free_vector],
                ,
                [$ompi_check_clib_dir],
                [$ompi_check_clib_libdir],
                [ompi_check_clib_happy="yes"],
                [ompi_check_clib_happy="no"])
            ],
        [ompi_check_clib_happy="no"])
    
    AS_IF([test "$ompi_check_clib_happy" = "yes"],
        [$2],
        [AS_IF([test ! -z "$with_clib" -a "$with_clib" != "no"],
                [AC_MSG_ERROR([CLIB (Eliots programming library) support requested but not found.  Aborting])])
            $3])
    ])


# OMPI_CHECK_CRSVM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if crsvm (CRS Shared Virtual Memory) support can be found.
# sets prefix_{CPPFLAGS, LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_CRSVM],[
    AC_ARG_WITH([clib],
        [AC_HELP_STRING([--with-clib(=DIR)],
                [Build CLIB (Eliots programming library) support, searching for libraries in DIR])])
    AC_ARG_WITH([clib-libdir],
        [AC_HELP_STRING([--with-clib-libdir=DIR],
                [Search for CLIB (Eliots programming library) libraries in DIR])])
    AC_ARG_WITH([crsvm],
        [AC_HELP_STRING([--with-crsvm(=DIR)],
                [Build CRS SVM (Shared Virtual Memory) support, searching for libraries in DIR])])
    AC_ARG_WITH([crsvm-libdir],
        [AC_HELP_STRING([--with-crsvm-libdir=DIR],
                [Search for CRS SVM (Shared Virtual Memory) libraries in DIR])])
    
    AS_IF([test "$with_crsvm" != "no"],
        [AS_IF([test ! -z "$with_clib" -a "$with_clib" != "yes"],
                [ompi_check_clib_dir="$with_clib"])
            AS_IF([test ! -z "$with_clib_libdir" -a "$with_clib_libdir" != "yes"],
                [ompi_check_clib_libdir="$with_clib_libdir"])
            AS_IF([test ! -z "$with_crsvm" -a "$with_crsvm" != "yes"],
                [ompi_check_crsvm_dir="$with_crsvm"])
            AS_IF([test ! -z "$with_crsvm_libdir" -a "$with_crsvm_libdir" != "yes"],
                [ompi_check_crsvm_libdir="$with_crsvm_libdir"])

            ompi_check_crsvm_$1_save_CPPFLAGS="$CPPFLAGS"
            ompi_check_crsvm_$1_save_LDFLAGS="$LDFLAGS"
            ompi_check_crsvm_$1_save_LIBS="$LIBS"

            OMPI_CHECK_PACKAGE([$1],
                [clib/error.h],
                [clib],
                [clib_error_free_vector],
                [-lpthread],
                [$ompi_check_clib_dir],
                [$ompi_check_clib_libdir],
                [OMPI_CHECK_PACKAGE([$1],
                    [svmdb.h],
                    [svmdb],
                    [svmdb_map],
                    [-lsvm -lclib -lpthread -lrt],
                    [$ompi_check_crsvm_dir],
                    [$ompi_check_crsvm_libdir],
                    [ompi_check_crsvm_happy="yes"
                        ompi_check_clib_happy="yes"])],
                [ompi_check_crsvm_happy="no"
                    ompi_check_clib_happy="no"])

            CPPFLAGS="$ompi_check_crsvm_$1_save_CPPFLAGS"
            LDFLAGS="$ompi_check_crsvm_$1_save_LDFLAGS"
            LIBS="$ompi_check_crsvm_$1_save_LIBS"
            ],
        [ompi_check_crsvm_happy="no"])
    
    AS_IF([test "$ompi_check_crsvm_happy" = "yes"],
        [$2],
        [AS_IF([test ! -z "$with_crsvm" -a "$with_crsvm" != "no"],
            [AS_IF([test "$ompi_check_clib_happy" = "yes"],
                [AC_MSG_ERROR([CRS SVM (Shared Virtual Memory) support requested but not found.  Aborting])],
                [AC_MSG_ERROR([CLIB (Eliots programming library) support required but not found.  Aborting])]
            )])
            $3])
    ])


# MCA_sensor_crsvm_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_sensor_crsvm_CONFIG], [
    OMPI_CHECK_CRSVM([sensor_crsvm],
                     [sensor_crsvm_happy="yes"],
                     [sensor_crsvm_happy="no"])

    AS_IF([test "$sensor_crsvm_happy" = "yes"],
          [sensor_crsvm_WRAPPER_EXTRA_LDFLAGS="$sensor_crsvm_LDFLAGS"
           sensor_crsvm_WRAPPER_EXTRA_LIBS="$sensor_crsvm_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build crsvm
    AC_SUBST([sensor_crsvm_CFLAGS])
    AC_SUBST([sensor_crsvm_CPPFLAGS])
    AC_SUBST([sensor_crsvm_LDFLAGS])
    AC_SUBST([sensor_crsvm_LIBS])
])dnl
