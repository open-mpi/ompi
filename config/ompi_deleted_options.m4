# -*- shell-script -*-
#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OMPI_CHECK_DELETED_OPTIONS],[
    OPAL_VAR_SCOPE_PUSH([with_pmi_given with_pmi_libdir_given cxx])

    # --with-pmi options were removed in v5.0
    AC_ARG_WITH([pmi],
                [AC_HELP_STRING([--with-pmi(=DIR)],
                                [*DELETED* Build PMI support, optionally adding DIR to the search path (default: no)])],
                [with_pmi_given=yes])

    AC_ARG_WITH([pmi-libdir],
                [AC_HELP_STRING([--with-pmi-libdir=DIR],
                                [*DELETED* Look for libpmi or libpmi2 in the given directory DIR, DIR/lib or DIR/lib64])],
                [with_pmi_libdir_given=yes])

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

    # Open MPI C++ bindings were removed in v5.0
    cxx=0
    cxxseek=0
    cxxex=0
    AC_ARG_ENABLE([mpi-cxx],
                  [AC_HELP_STRING([--enable-mpi-cxx],
                                  [*DELETED* Build the MPI C++ bindings])],
                  [cxx=1])
    AC_ARG_ENABLE([mpi-cxx-seek],
                  [AC_HELP_STRING([--enable-mpi-cxx-seek],
                                  [*DELETED* Build support for MPI::SEEK])],
                  [cxxseek=1])
    AC_ARG_ENABLE([cxx-exceptions],
                  [AC_HELP_STRING([--enable-cxx-exceptions],
                                  [*DELETED* Build support for C++ exceptions in the MPI C++ bindings])],
                  [cxxex=1])

    AS_IF([test "$enable_mpi_cxx" = "no" ],
          [cxx=0])

    AS_IF([test "$enable_mpi_cxx_seek" = "no" ],
          [cxxseek=0])

    AS_IF([test "$enable_cxx_exceptions" = "no" ],
          [cxxex=0])

    AS_IF([test $cxx -eq 1 || test $cxxseek -eq 1 || test $cxxex -eq 1],
          [AC_MSG_WARN([The MPI C++ bindings have been removed from Open MPI.])
           AC_MSG_WARN([If you need support for the MPI C++ bindings, you])
           AC_MSG_WARN([will need to use an older version of Open MPI.])
           AC_MSG_ERROR([Build cannot continue.])
          ])

    OPAL_VAR_SCOPE_POP
])
