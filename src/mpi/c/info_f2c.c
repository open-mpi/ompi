/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/interface/c/bindings.h"
#include "lam/lfc/lam_list.h"
#include "mpi/info/info.h"

#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Info_f2c = PMPI_Info_f2c
#endif

/**
 * Converts the MPI_Fint info into a valid C MPI_Info handle
 *
 * @param info Integer handle to an MPI_INFO object
 * @retval C handle corresponding to MPI_INFOO object
 */
MPI_Info MPI_Info_f2c(MPI_Fint info) {
    return (MPI_Info)0;
}
