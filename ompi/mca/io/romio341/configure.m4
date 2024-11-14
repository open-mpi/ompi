# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015-2022 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_ompi_io_romio341_POST_CONFIG], [
    AM_CONDITIONAL([MCA_io_romio341_SHOULD_BUILD], [test $1 -eq 1])
])


# MCA_io_romio341_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_io_romio341_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/io/romio341/Makefile])

    OPAL_VAR_SCOPE_PUSH([io_romio341_flags io_romio341_flags_define io_romio341_happy io_romio341_save_LIBS])
    AC_ARG_ENABLE([io-romio],
                  [AS_HELP_STRING([--disable-io-romio],
                                  [Disable the ROMIO MPI-IO component])])
    AC_ARG_WITH([io-romio-flags],
                [AS_HELP_STRING([--with-io-romio-flags=FLAGS],
                                [Pass FLAGS to the ROMIO distribution configuration script])])
    AC_DEFINE_UNQUOTED([MCA_io_romio341_USER_CONFIGURE_FLAGS], ["$with_io_romio_flags"], [Set of user-defined configure flags given to ROMIOs configure script via --with-io-romio-flags])
    AC_MSG_CHECKING([if want ROMIO component])
    AS_IF([test "$enable_io_romio" = "no"],
           [AC_MSG_RESULT([no])
            $2],
           [AC_MSG_RESULT([yes])
            AC_MSG_CHECKING([if MPI profiling is enabled])
            AS_IF([test "$enable_mpi_profile" = "no"],
                  [AC_MSG_RESULT([no])
                   AC_MSG_WARN([*** The ROMIO io component requires the MPI profiling layer])
                   AS_IF([test "$enable_io_romio" = "yes"],
                         [AC_MSG_ERROR([*** ROMIO requested but not available.  Aborting])])
                   $2],
                  [AC_MSG_RESULT([yes])

                   AS_IF([test -n "$with_io_romio_flags" && test "$with_io_romio_flags" != "no"],
                         [io_romio341_flags="$with_io_romio_flags $io_romio341_flags"],
                         [io_romio341_flags=])
                   # If ROMIO is going to end up in a DSO, all we need is
                   # shared library-ized objects, as we're only building a
                   # DSO (which is always shared).  Otherwise, build with
                   # same flags as OMPI, as we might need any combination of
                   # shared and static-ized objects...
                   AS_IF([test "$compile_mode" = "dso"],
                         [io_romio341_shared=enable
                          io_romio341_static=disable],
                         [AS_IF([test "$enable_shared" = "yes"],
                                [io_romio341_shared=enable],
                                [io_romio341_shared=disable])
                          AS_IF([test "$enable_static" = "yes"],
                                [io_romio341_static=enable],
                                [io_romio341_static=disable])])
                   AS_IF([test -n "$prefix" && test "$prefix" != "NONE"],
                         [io_romio341_prefix_arg="--prefix=$prefix"],
                         [io_romio341_prefix_arg=])

                   AS_IF([test "$cross_compiling" = "yes"],
                       [AS_IF([test ! -z $build], [io_romio341_flags="$io_romio341_flags --build=$build"])
                        AS_IF([test ! -z $host], [io_romio341_flags="$io_romio341_flags --host=$host"])
                        AS_IF([test ! -z $target], [io_romio341_flags="$io_romio341_flags --target=$target"])])
                   AS_IF([test "$enable_grequest_extensions" = "yes"],
                         [io_romio341_flags="$io_romio341_flags --enable-grequest-extensions"])
                   io_romio341_flags_define="$io_romio341_flags FROM_OMPI=yes CC='$CC' CFLAGS='$CFLAGS -D__EXTENSIONS__' CPPFLAGS='$CPPFLAGS' FFLAGS='$FFLAGS' LDFLAGS='$LDFLAGS' --$io_romio341_shared-shared --$io_romio341_static-static $io_romio341_flags $io_romio341_prefix_arg --disable-aio --disable-weak-symbols --enable-strict --disable-f77 --disable-f90 ac_cv_lib_cuda_cuMemGetAddressRange=no ac_cv_lib_cudart_cudaStreamSynchronize=no"
                   AC_DEFINE_UNQUOTED([MCA_io_romio341_COMPLETE_CONFIGURE_FLAGS], ["$io_romio341_flags_define"], [Complete set of command line arguments given to ROMIOs configure script])

                   io_romio341_flags="$io_romio341_flags FROM_OMPI=yes CC="'"'"$CC"'"'" CFLAGS="'"'"$CFLAGS -D__EXTENSIONS__"'"'" CPPFLAGS="'"'"$CPPFLAGS"'"'" FFLAGS="'"'"$FFLAGS"'"'" LDFLAGS="'"'"$LDFLAGS"'"'" --$io_romio341_shared-shared --$io_romio341_static-static $io_romio341_flags $io_romio341_prefix_arg --disable-aio --disable-weak-symbols --enable-strict --disable-f77 --disable-f90 ac_cv_lib_cuda_cuMemGetAddressRange=no ac_cv_lib_cudart_cudaStreamSynchronize=no"

                   opal_show_subtitle "Configuring ROMIO distribution"
                   OPAL_CONFIG_SUBDIR([3rd-party/romio341],
                                      [$io_romio341_flags],
                                      [io_romio341_happy=1], [io_romio341_happy=0])

                   AS_IF([test "$io_romio341_happy" = "1"],
                         [ # grab the libraries list from ROMIO.  We don't
                           # need this for building the component, as libtool
                           # will figure that part out.  But we do need it for
                           # the wrapper settings
                          io_romio341_save_LIBS="$LIBS"
                          LIBS=
                          . 3rd-party/romio341/localdefs
                          io_romio341_LIBS="$LIBS"
                          LIBS="$io_romio341_save_LIBS"
                          OPAL_3RDPARTY_DIST_SUBDIRS="$OPAL_3RDPARTY_DIST_SUBDIRS romio341"

                          AC_MSG_NOTICE([ROMIO distribution configured successfully])],
                         [AS_IF([test "$enable_io_romio" = "yes"],
                                [AC_MSG_ERROR([ROMIO distribution did not configure successfully])],
                                [AC_MSG_WARN([ROMIO distribution did not configure successfully])])])

                   AS_IF([test "$io_romio341_happy" = "1"],
                         [AC_CHECK_HEADERS([stdatomic.h],
                                           [OPAL_3RDPARTY_SUBDIRS="$OPAL_3RDPARTY_SUBDIRS romio341"],
                                           [AC_MSG_WARN([3rd party ROMIO cannot be built, disqualifying the io/romio341 component])
                                            io_romio341_happy=0])])

                   AS_IF([test "$io_romio341_happy" = "1"],
                         [$1],
                         [OPAL_MAKEDIST_DISABLE="$OPAL_MAKEDIST_DISABLE ROMIO"
			  $2])])])
    OPAL_VAR_SCOPE_POP
])
