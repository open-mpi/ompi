dnl
dnl Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2006-2008 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OPAL_SETUP_LIBLTDL],[
    OPAL_HAVE_LTDL_ADVISE=0

    # If --enable-dlopen was passed, OPAL_ENABLE_DLOPEN_SUPPORT==1
    # If --disable-dlopen was passed, OPAL_ENABLE_DLOPEN_SUPPORT==0
    # If neither was passed, OPAL_ENABLE_DLOPEN_SUPPORT==-1

    AS_IF([test $OPAL_ENABLE_DLOPEN_SUPPORT -ne 0],
          [ # See if we can find the Right stuff for libltdl
           OPAL_CHECK_PACKAGE([libltdl],
                              [ltdl.h],
                              [ltdl],
                              [lt_dlopen],
                              [],
                              [],
                              [],
                              [OPAL_ENABLE_DLOPEN_SUPPORT=1],
                              [AS_IF([test $OPAL_ENABLE_DLOPEN_SUPPORT -eq 1],
                                     [AC_MSG_WARN([Cannot find libltdl support,])
                                      AC_MSG_WARN([but --enable-dlopen was specified])
                                      AC_MSG_WARN([or this is a developer build.])
                                      AC_MSG_ERROR([Cannot continue.])])
                               OPAL_ENABLE_DLOPEN_SUPPORT=0])
          ])

    AS_IF([test $OPAL_ENABLE_DLOPEN_SUPPORT -eq 1],
          [CPPFLAGS="$CPPFLAGS $libltdl_CPPFLAGS"
           LDFLAGS="$LDFLAGS $libltdl_LDFLAGS"
           LIBS="$LIBS $libltdl_LIBS"

           # Check for lt_dladvise_init; warn if we don't have it
           # (because it's just a good thing to have).
           AC_CHECK_FUNC([lt_dladvise_init],
                         [OPAL_HAVE_LTDL_ADVISE=1],
                         [AC_MSG_WARN([*********************************************])
                          AC_MSG_WARN([Could not find lt_dladvise_init in libltdl])
                          AC_MSG_WARN([This could mean that your libltdl version])
                          AC_MSG_WARN([is old.  If you could upgrade, that would be great.])
                          AC_MSG_WARN([*********************************************])
                         ])

           AC_MSG_CHECKING([for lt_dladvise])
           AS_IF([test $OPAL_HAVE_LTDL_ADVISE -eq 1],
                 [AC_MSG_RESULT([yes])],
                 [AC_MSG_RESULT([no])])
          ])

    # If we have no dlopen support, we have to tell the OPAL MCA setup
    # macro that we need to build all the components as static.
    AS_IF([test $OPAL_ENABLE_DLOPEN_SUPPORT -eq 0],
          [enable_mca_dso=no
           enable_mca_static=yes])

    AC_DEFINE_UNQUOTED(OPAL_HAVE_LTDL_ADVISE, $OPAL_HAVE_LTDL_ADVISE,
        [Whether libltdl has the lt_dladvise interface])
    AC_DEFINE_UNQUOTED(OPAL_WANT_LIBLTDL, $OPAL_ENABLE_DLOPEN_SUPPORT,
        [Whether to include support for libltdl or not])

    AM_CONDITIONAL(OPAL_HAVE_DLOPEN,
                   [test $OPAL_ENABLE_DLOPEN_SUPPORT -eq 1])
])dnl
