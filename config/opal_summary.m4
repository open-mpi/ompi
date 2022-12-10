dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2016-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2016      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2022      IBM Corporation.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl OPAL_SUMMARY_ADD(section, topic, unused, result)
dnl
dnl Turn around and call OAC_SUMMARY_ADD
dnl -----------------------------------------------------------
AC_DEFUN([OPAL_SUMMARY_ADD],[
    OAC_SUMMARY_ADD([$1], [$2], [$4])
])

dnl OPAL_SUMMARY_PRINT
dnl
dnl Print a bunch of Open MPI summary configuration information and
dnl then turn around and call OAC_SUMMARY_PRINT.
dnl -----------------------------------------------------------
AC_DEFUN([OPAL_SUMMARY_PRINT],[
    cat <<EOF

Open MPI configuration:
-----------------------
Version: $OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION.$OMPI_RELEASE_VERSION$OMPI_GREEK_VERSION
MPI Standard Version: $MPI_VERSION.$MPI_SUBVERSION
EOF

    if test "$project_ompi_amc" = "true" ; then
        echo "Build MPI C bindings: yes" >&AS_MESSAGE_FD
    else
        echo "Build MPI C bindings: no" >&AS_MESSAGE_FD
    fi

    dnl Print out the bindings if we are building OMPI
    if test "$project_ompi_amc" = "true" ; then
        if test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_MPIFH_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h" >&AS_MESSAGE_FD
        elif test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_USEMPI_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h, use mpi" >&AS_MESSAGE_FD
        elif test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_USEMPIF08_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h, use mpi, use mpi_f08" >&AS_MESSAGE_FD
        else
            echo "Build MPI Fortran bindings: no" >&AS_MESSAGE_FD
        fi

        if test $WANT_MPI_JAVA_BINDINGS -eq 1 ; then
            echo "Build MPI Java bindings (experimental): yes" >&AS_MESSAGE_FD
        else
            echo "Build MPI Java bindings (experimental): no" >&AS_MESSAGE_FD
        fi
    fi

    if test "$project_oshmem_amc" = "true" ; then
        echo "Build Open SHMEM support: yes" >&AS_MESSAGE_FD
    elif test -z "$project_oshmem_amc" ; then
        echo "Build Open SHMEM support: no" >&AS_MESSAGE_FD
    else
        echo "Build Open SHMEM support: $project_oshmem_amc" >&AS_MESSAGE_FD
    fi

    if test $WANT_DEBUG = 0 ; then
        echo "Debug build: no" >&AS_MESSAGE_FD
    else
        echo "Debug build: yes" >&AS_MESSAGE_FD
    fi

    if test ! -z $with_platform ; then
        echo "Platform file: $with_platform" >&AS_MESSAGE_FD
    else
        echo "Platform file: (none)" >&AS_MESSAGE_FD
    fi

    echo " " >&AS_MESSAGE_FD

    OAC_SUMMARY_PRINT

    if test $WANT_DEBUG = 1 ; then
        cat >&AS_MESSAGE_FD <<EOF
*****************************************************************************
 THIS IS A DEBUG BUILD!  DO NOT USE THIS BUILD FOR PERFORMANCE MEASUREMENTS!
*****************************************************************************

EOF
    fi
])
