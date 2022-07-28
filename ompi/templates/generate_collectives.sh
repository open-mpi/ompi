#!/bin/sh

# Copyright (c) 2019      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

generate() {
    local cn=$1
    local uc=$(echo $cn | tr "a-z" "A-Z")
    local lc=$(echo $cn | tr "A-Z" "a-z")
    local template=$lc
    local proto="$2"
    local args="$3"
    local creq
    local creq_decl
    local intro

    ret=");\n\n    if (NULL != ierr) *ierr = OMPI_INT_2_FINT(c_ierr);"
    if [ X"$4" = Xnb ]; then
        proto=$(echo $proto | sed -e 's/MPI_Fint \*ierr$/MPI_Fint *request, MPI_Fint *ierr/g')
        args=$(echo $args | sed -e 's/ierr$/request, ierr/g')
        creq_decl="\n    MPI_Request c_req;"
        ret="               , \\&c_req${ret}\n    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_req);"
        template=$(echo $lc | cut -b2-)
        intro="\n#if OMPI_BUILD_MPI_PROFILING\n#if OPAL_HAVE_WEAK_SYMBOLS\n#pragma weak PMPI_${uc} = ompi_${lc}_f\n#pragma weak pmpi_${lc} = ompi_${lc}_f\n#pragma weak pmpi_${lc}_ = ompi_${lc}_f\n#pragma weak pmpi_${lc}__ = ompi_${lc}_f\n\n#pragma weak PMPI_${cn}_f = ompi_${lc}_f\n#pragma weak PMPI_${cn}_f08 = ompi_${lc}_f\n#else\nOMPI_GENERATE_F77_BINDINGS (PMPI_${uc},\n                            pmpi_${lc},\n                            pmpi_${lc}_,\n                            pmpi_${lc}__,\n                            pompi_${lc}_f,\n                            (${proto}),\n                            (${args}) )\n#endif\n#endif\n\n#if OPAL_HAVE_WEAK_SYMBOLS\n#pragma weak MPI_${uc} = ompi_${lc}_f\n#pragma weak mpi_${lc} = ompi_${lc}_f\n#pragma weak mpi_${lc}_ = ompi_${lc}_f\n#pragma weak mpi_${lc}__ = ompi_${lc}_f\n\n#pragma weak MPI_${cn}_f = ompi_${lc}_f\n#pragma weak MPI_${cn}_f08 = ompi_${lc}_f\n#else\n#if ! OMPI_BUILD_MPI_PROFILING\nOMPI_GENERATE_F77_BINDINGS (MPI_${uc},\n                            mpi_${lc},\n                            mpi_${lc}_,\n                            mpi_${lc}__,\n                            ompi_${lc}_f,\n                            (${proto}),\n                            (${args}) )\n#else\n#define ompi_${lc}_f pompi_${lc}_f\n#endif\n#endif\n\n\nvoid ompi_${lc}_f(${proto})\n{${creq_decl}"
        pmpi=PMPI_${cn}
        dir=../mpi/fortran/mpif-h
    elif [ X"$4" = Xp ]; then
        proto=$(echo $proto | sed -e 's/MPI_Fint \*ierr$/MPI_Fint *info, MPI_Fint *request, MPI_Fint *ierr/g')
        args=$(echo $args | sed -e 's/ierr$/info, request, ierr/g')
        creq_decl="\n    MPI_Info c_info = PMPI_Info_f2c(*info);\n    MPI_Request c_req;"
        ret="               , c_info, \\&c_req${ret}\n    if (MPI_SUCCESS == c_ierr) *request = PMPI_Request_c2f(c_req);"
        template=$(echo $lc | sed -e 's/_init$//g')
        intro="#include \"ompi\/mpiext\/pcollreq\/mpif-h\/mpiext_pcollreq_prototypes.h\"\n\n#if OMPI_BUILD_MPI_PROFILING\n#if OPAL_HAVE_WEAK_SYMBOLS\n#pragma weak PMPIX_${uc} = ompix_${lc}_f\n#pragma weak pmpix_${lc} = ompix_${lc}_f\n#pragma weak pmpix_${lc}_ = ompix_${lc}_f\n#pragma weak pmpix_${lc}__ = ompix_${lc}_f\n\n#pragma weak PMPIX_${cn}_f = ompix_${lc}_f\n#pragma weak PMPIX_${cn}_f08 = ompix_${lc}_f\n#else\nOMPI_GENERATE_F77_BINDINGS (PMPIX_${uc},\n                            pmpix_${lc},\n                            pmpix_${lc}_,\n                            pmpix_${lc}__,\n                            pompix_${lc}_f,\n                            (${proto}),\n                            (${args}) )\n#endif\n#endif\n\n#if OPAL_HAVE_WEAK_SYMBOLS\n#pragma weak MPIX_${uc} = ompix_${lc}_f\n#pragma weak mpix_${lc} = ompix_${lc}_f\n#pragma weak mpix_${lc}_ = ompix_${lc}_f\n#pragma weak mpix_${lc}__ = ompix_${lc}_f\n\n#pragma weak MPIX_${cn}_f = ompix_${lc}_f\n#pragma weak MPIX_${cn}_f08 = ompix_${lc}_f\n#else\n#if ! OMPI_BUILD_MPI_PROFILING\nOMPI_GENERATE_F77_BINDINGS (MPIX_${uc},\n                            mpix_${lc},\n                            mpix_${lc}_,\n                            mpix_${lc}__,\n                            ompix_${lc}_f,\n                            (${proto}),\n                            (${args}) )\n#else\n#define ompix_${lc}_f pompix_${lc}_f\n#endif\n#endif\n\n\nvoid ompix_${lc}_f(${proto})\n{${creq_decl}"
        pmpi=PMPIX_${cn}
        dir=../mpiext/pcollreq/mpif-h
    else
        ret="                ${ret}"
        intro="\n#if OMPI_BUILD_MPI_PROFILING\n#if OPAL_HAVE_WEAK_SYMBOLS\n#pragma weak PMPI_${uc} = ompi_${lc}_f\n#pragma weak pmpi_${lc} = ompi_${lc}_f\n#pragma weak pmpi_${lc}_ = ompi_${lc}_f\n#pragma weak pmpi_${lc}__ = ompi_${lc}_f\n\n#pragma weak PMPI_${cn}_f = ompi_${lc}_f\n#pragma weak PMPI_${cn}_f08 = ompi_${lc}_f\n#else\nOMPI_GENERATE_F77_BINDINGS (PMPI_${uc},\n                            pmpi_${lc},\n                            pmpi_${lc}_,\n                            pmpi_${lc}__,\n                            pompi_${lc}_f,\n                            (${proto}),\n                            (${args}) )\n#endif\n#endif\n\n#if OPAL_HAVE_WEAK_SYMBOLS\n#pragma weak MPI_${uc} = ompi_${lc}_f\n#pragma weak mpi_${lc} = ompi_${lc}_f\n#pragma weak mpi_${lc}_ = ompi_${lc}_f\n#pragma weak mpi_${lc}__ = ompi_${lc}_f\n\n#pragma weak MPI_${cn}_f = ompi_${lc}_f\n#pragma weak MPI_${cn}_f08 = ompi_${lc}_f\n#else\n#if ! OMPI_BUILD_MPI_PROFILING\nOMPI_GENERATE_F77_BINDINGS (MPI_${uc},\n                            mpi_${lc},\n                            mpi_${lc}_,\n                            mpi_${lc}__,\n                            ompi_${lc}_f,\n                            (${proto}),\n                            (${args}) )\n#else\n#define ompi_${lc}_f pompi_${lc}_f\n#endif\n#endif\n\n\nvoid ompi_${lc}_f(${proto})\n{${creq_decl}"
        pmpi=PMPI_${cn}
        dir=../mpi/fortran/mpif-h
    fi


    sed -e "s/@INTRO@/${intro}/g" -e "s/@CN@/${pmpi}/g" -e "s/@RETURN@/${ret}\\n}/g" ${template}_f.pre > ${dir}/${lc}_f.c
}

build () {
    generate "$1" "$2" "$3"
    generate "I$(echo $1 | tr 'A-Z' 'a-z')" "$2" "$3" nb
    generate "${1}_init" "$2" "$3" p
}

cd ompi/templates

build Bcast 'char *buffer, MPI_Fint *count, MPI_Fint *datatype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr' 'buffer, count, datatype, root, comm, ierr'
build Gather 'char *sendbuf, MPI_Fint *sendcount, MPI_Fint *sendtype, char *recvbuf, MPI_Fint *recvcount, MPI_Fint *recvtype, MPI_Fint *root, MPI_Fint *comm, MPI_Fint *ierr' 'sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierr'



