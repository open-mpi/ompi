/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2025      Bull SAS.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <execinfo.h>
#include <stdint.h>
#include <stdio.h>

#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/include/mpi.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/mca/pml/pml_constants.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "opal/util/output.h"

/* Default ompi_common_ubcl values */
mca_ompi_common_ubcl_component_t mca_ompi_common_ubcl_component = {
    .n_addr = 32,
};

static int mca_common_ubcl_find_rank(const struct ompi_communicator_t *comm, const uint64_t wrank)
{
    mca_pml_ubcl_comm_t *pml_comm = comm->c_pml_comm;

    if (NULL == comm->c_pml_comm) {
        common_ubcl_error("UBCL error: no translation array in comm");
        abort();
    }

    for (uint32_t i = 0; i < pml_comm->size; i++) {
        if (pml_comm->array[i] == wrank) {
            return i;
        }
    }

    common_ubcl_error("UBCL error irank translation");

    return 0;
}

int mca_common_ubcl_get_mpi_rank(const int rank, const struct ompi_communicator_t *comm,
                                 const uint64_t ubcl_rank)
{
    if (OMPI_ANY_SOURCE == rank) {
        return mca_common_ubcl_find_rank(comm, ubcl_rank);
    } else {
        return rank;
    }
}

void mca_common_ubcl_status_to_ompi(ompi_status_public_t *status,
                                    ubcl_status_t ubcl_status,
                                    struct ompi_communicator_t *comm, int rank)
{
    if (MPI_STATUS_IGNORE != status) {
        status->_cancelled = 0;     //TODO output the information of cancel
        status->_ucount = ubcl_status.size;
        status->MPI_TAG = (int) ubcl_status.tag;
        status->MPI_SOURCE = mca_common_ubcl_get_mpi_rank(rank, comm, ubcl_status.remote);
    }
}

int ubcl_error_to_ompi(ubcl_error_t code)
{
    int ret;
    switch (code) {
    case UBCL_SUCCESS:
        ret = OPAL_SUCCESS;
        break;
    case UBCL_ERROR:
        ret = OPAL_ERROR;
        break;
    case UBCL_ERR_RESOURCE_BUSY:
        ret = OPAL_ERR_RESOURCE_BUSY;
        break;
    case UBCL_ERR_OUT_OF_RESOURCE:
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        break;
    case UBCL_ERR_NOT_IMPLEMENTED:
        ret = OPAL_ERR_NOT_IMPLEMENTED;
        break;
    case UBCL_ERR_NOT_AVAILABLE:
        ret = OPAL_ERR_NOT_AVAILABLE;
        break;
    case UBCL_ERR_TEMP_OUT_OF_RESOURCE:
        ret = OPAL_ERR_TEMP_OUT_OF_RESOURCE;
        break;
    case UBCL_ERR_ARG_INVALID:
        ret = OPAL_ERR_BAD_PARAM;
        break;
    case UBCL_ERR_TOO_LATE:
        ret = OPAL_ERR_TIMEOUT;
        break;
    case UBCL_ERR_TRUNCATE:
        ret = MPI_ERR_TRUNCATE;
        break;
    default:
        ret = OPAL_ERROR;
        break;
    }

    return ret;
}

void _mca_common_ubcl_error(char *filename, int line, int err,
                                       char abort, int verbose, int output,
                                       int is_init, int comp_verbose,
                                       char *comp_name, char *format, ...)
{
    int n_addr   = 0;
    void **stack_buffer = NULL;
    char **stack = NULL;

    stack_buffer = malloc(sizeof(void *) * mca_ompi_common_ubcl_component.n_addr);
    n_addr       = backtrace(stack_buffer, mca_ompi_common_ubcl_component.n_addr);
    stack        = backtrace_symbols(stack_buffer, n_addr);

    int char_per_line = 256;
    int n_char = char_per_line * n_addr + 1024;
    char *msg = malloc(n_char * sizeof(char));

    if (NULL == stack || NULL == msg) {
        /* Output small error */
        opal_output_verbose(verbose, output,
                            "========\n== ERROR: Not enough memory while outputting error...\n== "
                            "%s encountered an error (%d) at %s:%d\n========\n",
                            comp_name,  err, filename, line);
    } else {
        /* Output full error */
        int current = 0;
        current += snprintf(msg + current, n_char - current,
                            "========\n== %s encountered an error (%d) at %s:%d\n== %s:\n\t",
                            comp_name, err, filename, line, abort ? "ERROR" : "WARNING");
        va_list arglist;
        va_start(arglist, format);
        current += vsnprintf(msg + current, n_char - current, format, arglist);
        va_end(arglist);

        current += snprintf(msg + current, n_char - current, "\n== STACK:\n");

        for (int i = 0; i < n_addr; i++) {
            size_t min_char = char_per_line < (n_char - current) ? char_per_line : n_char - current;
            current += snprintf(msg + current, min_char, "= [%2d] %s\n", i,
                                stack[i]);
        }

        if (is_init && output > 0) {
            opal_output_verbose(verbose, output,
                                "%s========", msg);
        } else if (abort || comp_verbose >= verbose) {
            fprintf(stderr, "%s\n", msg);
            fflush(stderr);
        }
    }

    if (abort) {
        OMPI_ERRHANDLER_INVOKE(&ompi_mpi_comm_world.comm, err, stack[0]);
        ompi_mpi_abort(&ompi_mpi_comm_world.comm, err);
    }

    free(stack_buffer);
    free(stack);
    free(msg);
}
