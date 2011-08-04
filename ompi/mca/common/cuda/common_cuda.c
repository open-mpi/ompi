/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <errno.h>
#include <unistd.h>
#include <cuda.h>

#include "opal/align.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "common_cuda.h"

static bool initialized = false;
static int mca_common_cuda_verbose;
static int mca_common_cuda_output = 0;
static bool mca_common_cuda_enabled = false;
static bool mca_common_cuda_register_memory = true;
static bool mca_common_cuda_warning = true;

void mca_common_cuda_init(void)
{
    int id, value;
    CUresult res;
    CUcontext cuContext;

    if (initialized) {
        return;
    }

    /* Set different levels of verbosity in the cuda related code. */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_verbose", 
                                     "Set level of common cuda verbosity",
                                     false, false, 0, &mca_common_cuda_verbose);
    mca_common_cuda_output = opal_output_open(NULL);
    opal_output_set_verbosity(mca_common_cuda_output, mca_common_cuda_verbose);

    /* Control whether system buffers get CUDA pinned or not.  Allows for 
     * performance analysis. */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_register_memory",
                                     "Whether to cuMemHostRegister preallocated BTL buffers",
                                     false, false, 
                                     (int) mca_common_cuda_register_memory, &value);
    mca_common_cuda_register_memory = OPAL_INT_TO_BOOL(value);

    /* Control whether we see warnings when CUDA memory registration fails.  This is
     * useful when CUDA support is configured in, but we are running a regular MPI
     * application without CUDA. */
    id = mca_base_param_reg_int_name("mpi", "common_cuda_warning",
                                     "Whether to print warnings when CUDA registration fails",
                                     false, false, 
                                     (int) mca_common_cuda_warning, &value);
    mca_common_cuda_warning = OPAL_INT_TO_BOOL(value);

    /* Check to see if this process is running in a CUDA context.  If
     * so, all is good.  If not, then disable CUDA support. */
    res = cuCtxGetCurrent(&cuContext);
    if (CUDA_SUCCESS != res) {
        if (mca_common_cuda_warning) {
            orte_show_help("help-mpi-common-cuda.txt", "cuCtxGetCurrent failed",
                           true, res);
        }
        mca_common_cuda_enabled = false;
        mca_common_cuda_register_memory = false;
        initialized = true;
        return;
    } else {
        mca_common_cuda_enabled = true;
        opal_output_verbose(20, mca_common_cuda_output,
                            "CUDA: cuCtxGetCurrent succeeded");
    }

    opal_output_verbose(30, mca_common_cuda_output,
                        "CUDA: initialized");
    initialized = true;
}


/**
 * Call the CUDA register function so we pin the memory in the CUDA
 * space.
 */
void mca_common_cuda_register(void *ptr, size_t amount, char *msg) {
    int res;

    if (!initialized) {
        mca_common_cuda_init();
    }

    if (mca_common_cuda_enabled && mca_common_cuda_register_memory) {
        res = cuMemHostRegister(ptr, amount, 0);
        if (res != CUDA_SUCCESS) {
            /* If registering the memory fails, print a message and continue.
             * This is not a fatal error. */
            orte_show_help("help-mpi-common-cuda.txt", "cuMemHostRegister failed",
                           true, ptr, amount, res, msg);
        } else {
            opal_output_verbose(20, mca_common_cuda_output,
                                "CUDA: cuMemHostRegister OK on mpool %s: "
                                "address=%p, bufsize=%d",
                                msg, ptr, (int)amount);
        }
    }
}

/**
 * Call the CUDA unregister function so we unpin the memory in the CUDA
 * space.
 */
void mca_common_cuda_unregister(void *ptr, char *msg) {
    int res;

    if (!initialized) {
        mca_common_cuda_init();
    }

    if (mca_common_cuda_enabled && mca_common_cuda_register_memory) {
        res = cuMemHostUnregister(ptr);
        if (res != CUDA_SUCCESS) {
            /* If unregistering the memory fails, print a message and continue.
             * This is not a fatal error. */
            orte_show_help("help-mpi-common-cuda.txt", "cuMemHostUnregister failed",
                           true, ptr, res, msg);
        } else {
            opal_output_verbose(20, mca_common_cuda_output,
                                "CUDA: cuMemHostUnregister OK on mpool %s: "
                                "address=%p",
                                msg, ptr);
        }
    }
}
