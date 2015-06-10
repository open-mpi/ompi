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
dnl Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
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
    OPAL_VAR_SCOPE_PUSH([HAPPY])

    opal_show_subtitle "GNU libltdl setup"

    # AC_CONFIG_SUBDIRS appears to be broken for non-gcc compilers (i.e.,
    # passing precious variables down to the sub-configure).
    #
    # Finally, make ltdl follow the same shared/static convention that was
    # user for the main OMPI libraries.  So manually examine
    # $enable_shared and $enable_static and pass down the corresponding
    # flags.

    LIBLTDL_SUBDIR=
    OPAL_HAVE_LTDL_ADVISE=0
    OPAL_LIBLTDL_INTERNAL=0

    AS_IF([test "$OPAL_ENABLE_DLOPEN_SUPPORT" = "0"],
          [AC_MSG_WARN([libltdl support disabled (by --disable-dlopen)])
           LIBLTDL=
           LDTLINCL=
           OPAL_WRAPPER_FLAGS_ADD(LIBS, "$LIBS")],
          [
           # Default to building the internal copy.  After this,
           # paffinity_hwloc_location is guaranteed to be set to one of:
           # "internal", a directory name (i.e., whatever the user
           # supplied), or "no".
           libltdl_location=$with_libltdl
           AS_IF([test -z "$libltdl_location" -o "$libltdl_location" = "yes"],
                 [libltdl_location=internal])

           AC_MSG_CHECKING([location of libltdl])
           case $libltdl_location in
           no)
              AC_MSG_WARN([--without-libltdl specified in conjunction with])
              AC_MSG_WARN([--enable-dlopen (or --disable-dlopen was not specified)])
              AC_MSG_WARN([Cannot have dlopen without libltdl])
              AC_MSG_ERROR([Cannot continue])
              ;;
           internal)
              AC_MSG_RESULT([internal copy])
              _OPAL_SETUP_LIBLTDL_INTERNAL
              ;;
           external)
              AC_MSG_RESULT([external copy (unspecified)])
              # If we're using an extern libltdl, then reset the
              # LTDLINCL that was set earlier (ie., there's no need to
              # -I into our internal libltdl tree).
              LIBLTDL=
              LDTLINCL=
              libltdl_location=
              libltdl_need_external=1
              ;;
           *) 
              AC_MSG_RESULT([external copy ($libltdl_location)])
              OPAL_CHECK_WITHDIR([libltdl], [$libltdl_location], 
                                 [include/ltdl.h])
              # If we're using an extern libltdl, then reset the
              # LTDLINCL that was set earlier (ie., there's no need to
              # -I into our internal libltdl tree).
              LIBLTDL=
              LDTLINCL=
              libltdl_need_external=1
              ;;
            esac

            AS_IF([test "$libltdl_need_external" = "1"],
                  [OPAL_CHECK_PACKAGE([libltdl],
                                 [ltdl.h],
                                 [ltdl],
                                 [lt_dlopen],
                                 [],
                                 [$libltdl_location],
                                 [],
                                 [],
                                 [AC_MSG_WARN([External libltdl installation not found])
                                  AC_MSG_WARN([or not usable.])
                                  AC_MSG_ERROR([Cannot continue.])])
                   CPPFLAGS="$CPPFLAGS $libltdl_CPPFLAGS"
                   LDFLAGS="$LDFLAGS $libltdl_LDFLAGS"
                   LIBS="$LIBS $libltdl_LIBS"

                   # Check for lt_dladvise_init; warn if we don't have
                   # it
                   AC_CHECK_FUNC([lt_dladvise_init],
		       [OPAL_HAVE_LTDL_ADVISE=1],
                       [AC_MSG_WARN([*********************************************])
                        AC_MSG_WARN([Could not find lt_dladvise_init in the])
                        AC_MSG_WARN([external libltdl installation.])
                        AC_MSG_WARN([This could mean that your libltdl version])
                        AC_MSG_WARN([is old.  We recommend that you re-configure])
                        AC_MSG_WARN([Open MPI with --with-libltdl=internal to])
                        AC_MSG_WARN([use the internal libltdl copy in Open MPI.])
                        AC_MSG_WARN([])
                        AC_MSG_WARN([Sleeping 10 seconds to give you a])
                        AC_MSG_WARN([chance to read this message.])
                        AC_MSG_WARN([*********************************************])
                        sleep 10
                   ])
            ])
    ])

    AC_SUBST(LTDLINCL)
    AC_SUBST(LIBLTDL)
    AC_SUBST(LIBLTDL_SUBDIR)

    AC_MSG_CHECKING([for lt_dladvise])
    AS_IF([test $OPAL_HAVE_LTDL_ADVISE -eq 1],
          [AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])])
    AC_DEFINE_UNQUOTED(OPAL_HAVE_LTDL_ADVISE, $OPAL_HAVE_LTDL_ADVISE,
        [Whether libltdl appears to have the lt_dladvise interface])

    AC_DEFINE_UNQUOTED(OPAL_WANT_LIBLTDL, $OPAL_ENABLE_DLOPEN_SUPPORT,
        [Whether to include support for libltdl or not])
    AC_DEFINE_UNQUOTED(OPAL_LIBLTDL_INTERNAL, $OPAL_LIBLTDL_INTERNAL,
        [Whether we are using the internal libltdl or not])

    AM_CONDITIONAL(OPAL_HAVE_DLOPEN, 
                   [test "$OPAL_ENABLE_DLOPEN_SUPPORT" = "1"])
    OPAL_VAR_SCOPE_POP([HAPPY])
])dnl


#
# Setup to build the internal copy of libltdl
#
AC_DEFUN([_OPAL_SETUP_LIBLTDL_INTERNAL],[
    OPAL_VAR_SCOPE_PUSH([CFLAGS_save CPPFLAGS_save])

    opal_subdir_args="$opal_subdir_args --enable-ltdl-convenience --disable-ltdl-install"
    if test "$enable_shared" = "yes"; then
        opal_subdir_args="$opal_subdir_args --enable-shared"
    else
        opal_subdir_args="$opal_subdir_args --disable-shared"
    fi
    if test "$enable_static" = "yes"; then
        opal_subdir_args="$opal_subdir_args --enable-static"
    else
        opal_subdir_args="$opal_subdir_args --disable-static"
    fi

    CFLAGS_save="$CFLAGS"
    CFLAGS="$OPAL_CFLAGS_BEFORE_PICKY $OPAL_VISIBILITY_CFLAGS"

    # VPATH support will be included by default in CONFIG_SUBDIR
    OPAL_CONFIG_SUBDIR(opal/libltdl, [$opal_subdir_args], 
                       [HAPPY=1], [HAPPY=0])
    if test "$HAPPY" = "1"; then
        LIBLTDL_SUBDIR=libltdl
        OPAL_LIBLTDL_INTERNAL=1

        CPPFLAGS_save="$CPPFLAGS"
        CPPFLAGS="-I$srcdir"
        AC_EGREP_HEADER([lt_dladvise_init], [opal/libltdl/ltdl.h],
                        [OPAL_HAVE_LTDL_ADVISE=1])
        CPPFLAGS="$CPPFLAGS_save"

        # --export-dynamic allows exported symbols to be resolved via
        # --dlsym and friends.
        LDFLAGS="-export-dynamic $LDFLAGS"
    else
        AC_MSG_WARN([Failed to build GNU libltdl.  This usually means that something])
        AC_MSG_WARN([is incorrectly setup with your environment.  There may be useful information in])
        AC_MSG_WARN([opal/libltdl/config.log.  You can also disable GNU libltdl, which will disable])
        AC_MSG_WARN([dynamic shared object loading, by configuring with --disable-dlopen.])
        AC_MSG_ERROR([Cannot continue])
    fi
    CFLAGS="$CFLAGS_save"

    OPAL_VAR_SCOPE_POP
])dnl
