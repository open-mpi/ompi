# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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

# MCA_io_romio_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_io_romio_CONFIG],[
    io_romio_LIBS="$LIBS"

    AC_ARG_ENABLE([io-romio],
                  [AC_HELP_STRING([--disable-io-romio],
                                  [Disable the ROMIO MPI-IO component])])
    AC_ARG_WITH([io-romio-flags], 
                [AC_HELP_STRING([--with-io-romio-flags=FLAGS],
                                [Pass FLAGS to the ROMIO distribution configuration script])])
    AC_MSG_CHECKING([if want ROMIO component])
    AS_IF([test "$enable_io_romio" = "no"],
           [AC_MSG_RESULT([no])
            $2], 
           [AC_MSG_RESULT([yes])
            AC_MSG_CHECKING([if MPI profiling is enabled])
            AS_IF([test "$enable_mpi_profile" = "no"],
                  [AC_MSG_RESULT([no])
                   AC_MSG_WARN([*** The ROMIO io component requires the MPI profiling layer])
                   $2],
                  [AC_MSG_RESULT([yes])

                   AS_IF([test -n "$with_io_romio_flags" -a "$with_io_romio_flags" != "no"],
                         [io_romio_flags="$with_io_romio_flags $io_romio_flags"],
                         [io_romio_flags=])
                   AS_IF([test "$compile_mode" = "dso"],
                         [io_romio_shared=enable
                          io_romio_static=disable],
                         [io_romio_shared=disable
                          io_romio_static=enable])
                   AS_IF([test -n "$prefix" -a "$prefix" != "NONE"], 
                         [io_romio_prefix_arg="--prefix=$prefix"], 
                         [io_romio_prefix_arg=])
                   io_romio_flags="CFLAGS="'"'"$CFLAGS"'"'" CPPFLAGS="'"'"$CPPFLAGS"'"'" FFLAGS="'"'"$FFLAGS"'"'" LDFLAGS="'"'"$LSFLAGS"'"'" --$io_romio_shared-shared --$io_romio_static-static $io_romio_flags $io_romio_prefix_arg"

                   AC_CHECK_LIB(aio, main)

                   ompi_show_subtitle "Configuring ROMIO distribution"
                   OMPI_CONFIG_SUBDIR([ompi/mca/io/romio/romio-dist], 
                                      [$io_romio_flags],
                                      [io_romio_happy=1], [io_romio_happy=0])

                   AS_IF([test "$io_romio_happy" = "1"],
                         [echo "ROMIO distribution configured successfully"
                          $1],
                         [LIBS="$io_romio_LIBS"
                          AC_MSG_WARN([ROMIO distribution did not configure successfully])
                          $2])])])
])
