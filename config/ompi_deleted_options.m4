# -*- shell-script -*-
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2020-2021 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OMPI_CHECK_DELETED_OPTIONS],[
    OPAL_VAR_SCOPE_PUSH([with_pmi_given with_pmi_libdir_given ompi_cxx_warn ompi_cxx_error])

    # --with-pmi options were removed in v5.0
    AC_ARG_WITH([pmi],
                [AS_HELP_STRING([--with-pmi(=DIR)],
                                [*DELETED* Build PMI support, optionally adding DIR to the search path (default: no)])],
                [with_pmi_given=yes])

    AC_ARG_WITH([pmi-libdir],
                [AS_HELP_STRING([--with-pmi-libdir=DIR],
                                [*DELETED* Look for libpmi or libpmi2 in the given directory DIR, DIR/lib or DIR/lib64])],
                [with_pmi_libdir_given=yes])

    AS_IF([test "$with_pmi" = "no"],
          [with_pmi_given=no])

    AS_IF([test "$with_pmi_libdir" = "no"],
          [with_pmi_libdir_given=no])

    if test "$with_pmi_given" = "yes" || test "$with_pmi_libdir_given" = "yes"; then
        AC_MSG_WARN([Open MPI no longer supports PMI-1 or PMI-2 libraries.])
        AC_MSG_WARN([PMIx is now required. Either the internal version or an])
        AC_MSG_WARN([external version of PMIx may be used, so long as the])
        AC_MSG_WARN([external version is compatible with the PMIx v2.2])
        AC_MSG_WARN([Standard or higher. Note that cross-version support])
        AC_MSG_WARN([within the OpenPMIx library can be used by this OMPI])
        AC_MSG_WARN([to interact with environments based on other PMIx])
        AC_MSG_WARN([versions.])
        AC_MSG_ERROR([Build cannot continue.])
    fi

    # Note that we always *warn* if someone used a CLI option for a
    # feature that has been deleted.  If, however, they are disabling
    # the deleted feature (e.g., --disable-mpi-cxx), then emitting a
    # warning is good enough -- allow configure to continue.  If,
    # however, the user asked to enable a deleted feature, then
    # configure needs to error out.
    ompi_cxx_warn=0
    ompi_cxx_error=0
    AC_ARG_ENABLE([mpi-cxx],
                  [AS_HELP_STRING([--enable-mpi-cxx],
                                  [*DELETED* Build the MPI C++ bindings])],
                  [ompi_cxx_warn=1
                   AS_IF([test "$enable_mpi_cxx" != "no"],
                         [ompi_cxx_error=1])
                  ])
    AC_ARG_ENABLE([mpi-cxx-seek],
                  [AS_HELP_STRING([--enable-mpi-cxx-seek],
                                  [*DELETED* Build support for MPI::SEEK])],
                  [ompi_cxx_warn=1
                   AS_IF([test "$enable_mpi_cxx_seek" != "no"],
                         [ompi_cxx_error=1])
                  ])
    AC_ARG_ENABLE([cxx-exceptions],
                  [AS_HELP_STRING([--enable-cxx-exceptions],
                                  [*DELETED* Build support for C++ exceptions in the MPI C++ bindings])],
                  [ompi_cxx_warn=1
                   AS_IF([test "$enable_cxx_exceptions" != "no"],
                         [ompi_cxx_error=1])
                  ])

    AS_IF([test $ompi_cxx_warn -eq 1],
          [AC_MSG_WARN([An MPI C++ bindings-related command line option])
           AC_MSG_WARN([was given to "configure".])
           AC_MSG_WARN([ ])
           AC_MSG_WARN([This command line option will be removed in a future])
           AC_MSG_WARN([version of Open MPI; you should discontinue using it.])
           AC_MSG_WARN([You have been warned!])
           AC_MSG_WARN([ ])
           AC_MSG_WARN([The MPI C++ bindings were deprecated in the MPI-2.2])
           AC_MSG_WARN([standard in 2009, and removed from the MPI-3.0])
           AC_MSG_WARN([standard in 2012.  The MPI C++ bindings were then])
           AC_MSG_WARN([removed from Open MPI v5.0.0 in 2022.])
           AC_MSG_WARN([ ])
           AC_MSG_WARN([If you need support for the MPI C++ bindings, you])
           AC_MSG_WARN([will need to use an older version of Open MPI.])
          ])

    AS_IF([test $ompi_cxx_error -eq 1],
          [AC_MSG_ERROR([Build cannot continue.])])

    OPAL_VAR_SCOPE_POP
])
