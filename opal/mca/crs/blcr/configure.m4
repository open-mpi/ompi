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
    # If we don't want FT, don't compile this component
    AS_IF([test "$ompi_want_ft" = "1"],
        [crs_blcr_good="yes"],
        [$2])
    
    AS_IF([test ! -z "$with_blcr" -a "$with_blcr" = "no"],[$2], [ 
        crs_blcr_CFLAGS="$CFLAGS"
        crs_blcr_CPPFLAGS="$CPPFLAGS"
        crs_blcr_LDFLAGS="$LDFLAGS"
        crs_blcr_LIBS="$LIBS"

        AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "yes"], 
              [CPPFLAGS="$CPPFLAGS -I$with_blcr/include"
               LDFLAGS="$LDFLAGS -L$with_blcr/lib"])

        AC_CHECK_HEADERS([libcr.h],
                         [AC_CHECK_LIB([cr], 
                                       [cr_init],
                                       [crs_blcr_good="yes"],
                                       [crs_blcr_good="no"])],
                         [crs_blcr_good="no"])

        CFLAGS="$crs_blcr_CFLAGS"
        CPPFLAGS="$crs_blcr_CPPFLAGS"
        LDFLAGS="$crs_blcr_LDFLAGS"
        LIBS="$crs_blcr_LIBS"

        AS_IF([test "$crs_blcr_good" != "no"], 
              [AS_IF([test ! -z "$with_blcr" -a "$with_blcr" != "yes"], 
                     [crs_blcr_CFLAGS="`echo $CFLAGS | sed 's/-pedantic//g'`"
                      crs_blcr_CFLAGS="`echo $crs_blcr_CFLAGS | sed 's/-Wundef//g'`"
                      crs_blcr_CPPFLAGS="`echo $crs_blcr_CPPFLAGS | sed 's/-pedantic//g'`"
                      crs_blcr_CPPFLAGS="`echo $crs_blcr_CPPFLAGS | sed 's/-Wundef//g'`"
                      crs_blcr_CPPFLAGS="$crs_blcr_CPPFLAGS -I$with_blcr/include"
                      crs_blcr_LDFLAGS="$crs_blcr_LDFLAGS -L$with_blcr/lib"
                      ])
               crs_blcr_LIBS="$crs_blcr_LIBS -lcr"], 
              [AS_IF([test ! -z "$with_blcr"],
                     [AC_MSG_ERROR([BLCR support requested but not found.  Perhaps you need to specify the location of the BLCR libraries.])])
               $2])
               
        #
        # Since BLCR libraries are not fully ISO99 C compliant
        # -pedantic and -Wundef raise a bunch of warnings, so
        # we just strip them off for this component
        #
        AS_IF([test "$crs_blcr_CFLAGS" != "$CFLAGS" -a "$crs_blcr_good" = "yes"],
            [AC_MSG_WARN([Removed -pedantic and -Wundef from CFLAGS for blcr component because libcr.h is not really ANSI C])])
    ])

    # If check worked, set wraper flags.
    # Evaluate suceed / fail
    AS_IF([test "$crs_blcr_good" = "yes"],
          [crs_blcr_WRAPPER_EXTRA_LDFLAGS="$crs_blcr_LDFLAGS"
           crs_blcr_WRAPPER_EXTRA_LIBS="$crs_blcr_LIBS"
           crs_blcr_WRAPPER_EXTRA_CPPFLAGS="$crs_blcr_CPPFLAGS"
           $1],
          [$2])
          
    #
    AC_SUBST([crs_blcr_crs_blcr_WRAPPER_EXTRA_LDFLAGS])
    AC_SUBST([crs_blcr_crs_blcr_WRAPPER_EXTRA_LIBS])
    AC_SUBST([crs_blcr_crs_blcr_WRAPPER_EXTRA_CPPFLAGS])
    AC_SUBST([crs_blcr_CFLAGS])
    AC_SUBST([crs_blcr_CPPFLAGS])
    AC_SUBST([crs_blcr_LDFLAGS])
    AC_SUBST([crs_blcr_LIBS])

])dnl
