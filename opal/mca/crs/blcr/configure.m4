# -*- shell-script -*-
#
# Copyright (c) 2004-2007 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_crs_blcr_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_crs_blcr_CONFIG],[
    AC_ARG_WITH([blcr],
                [AC_HELP_STRING([--with-blcr],
                                [Path to BLCR Installation])])

    check_crs_blcr_good="no"
    AS_IF([test "$with_blcr" = "no"], [$2],
        [check_crs_blcr_good="yes"])

    # If we don't want FT, don't compile this component
    AS_IF([test "$check_crs_blcr_good" = "yes" -a "$ompi_want_ft" = "1"],
        [check_crs_blcr_good="yes"],
        [$2])

    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2],
          [AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "yes"],
                 [check_crs_blcr_dir="$with_blcr"],
                 [check_crs_blcr_dir=""])])

    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2], [
        crs_blcr_CFLAGS="$CFLAGS"
        crs_blcr_CPPFLAGS="$CPPFLAGS"
        crs_blcr_LDFLAGS="$LDFLAGS"
        crs_blcr_LIBS="$LIBS"

        AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "yes"], 
              [CPPFLAGS="$CPPFLAGS -I$check_crs_blcr_dir/include"
               LDFLAGS="$LDFLAGS -L$check_crs_blcr_dir/lib"])

        AC_CHECK_HEADERS([libcr.h],
                         [AC_CHECK_LIB([cr], 
                                       [cr_init],
                                       [check_crs_blcr_good="yes"],
                                       [check_crs_blcr_good="no"])],
                         [check_crs_blcr_good="no"])

        CFLAGS="$crs_blcr_CFLAGS"
        CPPFLAGS="$crs_blcr_CPPFLAGS"
        LDFLAGS="$crs_blcr_LDFLAGS"
        LIBS="$crs_blcr_LIBS"

        AS_IF([test "$check_crs_blcr_good" != "yes"], 
              [AS_IF([test ! -z "$with_blcr"],
                      [AC_MSG_ERROR([BLCR support requested but not found.  Perhaps you need to specify the location of the BLCR libraries.])])
                  $2],
              [AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "yes"], 
                     [crs_blcr_CFLAGS="`echo $CFLAGS | sed 's/-pedantic//g'`"
                      crs_blcr_CFLAGS="`echo $crs_blcr_CFLAGS | sed 's/-Wundef//g'`"
                      crs_blcr_CPPFLAGS="`echo $crs_blcr_CPPFLAGS | sed 's/-pedantic//g'`"
                      crs_blcr_CPPFLAGS="`echo $crs_blcr_CPPFLAGS | sed 's/-Wundef//g'`"
                      crs_blcr_CPPFLAGS="$crs_blcr_CPPFLAGS -I$check_crs_blcr_dir/include"
                      crs_blcr_LDFLAGS="$crs_blcr_LDFLAGS -L$check_crs_blcr_dir/lib"
                      ])
               crs_blcr_LIBS="$crs_blcr_LIBS -lcr"])
               
        #
        # Since BLCR libraries are not fully ISO99 C compliant
        # -pedantic and -Wundef raise a bunch of warnings, so
        # we just strip them off for this component
        #
        AS_IF([test "$crs_blcr_CFLAGS" != "$CFLAGS" -a "$check_crs_blcr_good" = "yes"],
            [AC_MSG_WARN([Removed -pedantic and -Wundef from CFLAGS for blcr component because libcr.h is not really ANSI C])])
    ])

    # If check worked, set wraper flags.
    # Evaluate suceed / fail
    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2],
          [crs_blcr_WRAPPER_EXTRA_LDFLAGS="$crs_blcr_LDFLAGS"
           crs_blcr_WRAPPER_EXTRA_LIBS="$crs_blcr_LIBS"
           crs_blcr_WRAPPER_EXTRA_CPPFLAGS="$crs_blcr_CPPFLAGS"
           $1])

    #
    # Check for version >= 0.6.0 which has:
    # - working cr_request_file
    # - 'requester' parameter to checkpoint_info
    #
    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2], [
           prev_CPPFLAGS="$CPPFLAGS"
           prev_LDFLAGS="$LDFLAGS"
           CPPFLAGS="$CPPFLAGS -I$check_crs_blcr_dir/include"
           LDFLAGS="$LDFLAGS -L$check_crs_blcr_dir/lib"

           crs_blcr_have_working_cr_request=0
           AC_MSG_CHECKING(for BLCR working cr_request)
           AC_TRY_COMPILE([#include <libcr.h>],
               [#if CR_RELEASE_MAJOR <= 0 && CR_RELEASE_MINOR < 6
                 #error Version earlier than 0.6.0
                 #endif
               ],
               [crs_blcr_have_working_cr_request=1
                   AC_MSG_RESULT([yes])],
               [crs_blcr_have_working_cr_request=0
                   AC_MSG_RESULT([no])
                   AC_MSG_WARN([This BLCR version does not contain a known working version of cr_request])
               ])
           AC_DEFINE_UNQUOTED([CRS_BLCR_HAVE_CR_REQUEST], [$crs_blcr_have_working_cr_request],
               [BLCR cr_request check])


           crs_blcr_have_info_requester=0
           AC_CHECK_MEMBER([struct cr_checkpoint_info.requester],
               [crs_blcr_have_info_requester=1],
               [AC_MSG_WARN([This BLCR version does not contain a 'requester' member of the 'cr_checkpoint_info' struct])],
               [#include <libcr.h>])
           AC_DEFINE_UNQUOTED([CRS_BLCR_HAVE_INFO_REQUESTER], [$crs_blcr_have_info_requester],
               [BLCRs cr_checkpoint_info.requester member availability])

           CPPFLAGS="$prev_CPPFLAGS"
           LDFLAGS="$prev_LDFLAGS"
           $1])

    #
    AS_IF([test "$check_crs_blcr_good" != "yes"], [$2], [
            AC_SUBST([crs_blcr_crs_blcr_WRAPPER_EXTRA_LDFLAGS])
            AC_SUBST([crs_blcr_crs_blcr_WRAPPER_EXTRA_LIBS])
            AC_SUBST([crs_blcr_crs_blcr_WRAPPER_EXTRA_CPPFLAGS])
            AC_SUBST([crs_blcr_CFLAGS])
            AC_SUBST([crs_blcr_CPPFLAGS])
            AC_SUBST([crs_blcr_LDFLAGS])
            AC_SUBST([crs_blcr_LIBS])
            $1])
    
])dnl
