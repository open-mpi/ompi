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
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype_cuda.h"
#include "opal/util/output.h"
#include "orte/util/show_help.h"
#include "common_cuda.h"

static bool common_cuda_initialized = false;
static bool common_cuda_init_function_added = false;
static int mca_common_cuda_verbose;
static int mca_common_cuda_output = 0;
static bool mca_common_cuda_enabled = false;
static bool mca_common_cuda_register_memory = true;
static bool mca_common_cuda_warning = true;
static opal_list_t common_cuda_memory_registrations;

/* Structure to hold memory registrations that are delayed until first
 * call to send or receive a GPU pointer */
struct common_cuda_mem_regs_t {
    opal_list_item_t super;
    void *ptr;
    size_t amount;
    char *msg;
};
typedef struct common_cuda_mem_regs_t common_cuda_mem_regs_t;
OBJ_CLASS_DECLARATION(common_cuda_mem_regs_t);
OBJ_CLASS_INSTANCE( common_cuda_mem_regs_t,
                    opal_list_item_t,
                    NULL,
                    NULL );


static void mca_common_cuda_init(void)
{
    int id, value, i, s;
    CUresult res;
    CUcontext cuContext;
    common_cuda_mem_regs_t *mem_reg;

    if (common_cuda_initialized) {
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
     * so, all is good.  If not, then disable registration of memory. */
    res = cuCtxGetCurrent(&cuContext);
    if (CUDA_SUCCESS != res) {
        if (mca_common_cuda_warning) {
            /* Check for the not initialized error since we can make suggestions to
             * user for this error. */
            if (CUDA_ERROR_NOT_INITIALIZED == res) {
                orte_show_help("help-mpi-common-cuda.txt", "cuCtxGetCurrent failed not initialized",
                               true);
            } else {
                orte_show_help("help-mpi-common-cuda.txt", "cuCtxGetCurrent failed",
                               true, res);
            }
        }
        mca_common_cuda_enabled = false;
        mca_common_cuda_register_memory = false;
    } else if ((CUDA_SUCCESS == res) && (NULL == cuContext)) {
        if (mca_common_cuda_warning) {
            orte_show_help("help-mpi-common-cuda.txt", "cuCtxGetCurrent returned NULL",
                           true);
        }
        mca_common_cuda_enabled = false;
        mca_common_cuda_register_memory = false;
    } else {
        /* All is good.  mca_common_cuda_register_memory will retain its original
         * value.  Normally, that is 1, but the user can override it to disable
         * registration of the internal buffers. */
        mca_common_cuda_enabled = true;
        opal_output_verbose(20, mca_common_cuda_output,
                            "CUDA: cuCtxGetCurrent succeeded");
    }

    s = opal_list_get_size(&common_cuda_memory_registrations);
    for(i = 0; i < s; i++) {
        mem_reg = (common_cuda_mem_regs_t *)
            opal_list_remove_first(&common_cuda_memory_registrations);
        if (mca_common_cuda_enabled && mca_common_cuda_register_memory) {
            res = cuMemHostRegister(mem_reg->ptr, mem_reg->amount, 0);
            if (res != CUDA_SUCCESS) {
                /* If registering the memory fails, print a message and continue.
                 * This is not a fatal error. */
                orte_show_help("help-mpi-common-cuda.txt", "cuMemHostRegister failed",
                               true, mem_reg->ptr, mem_reg->amount, res, mem_reg->msg);
            } else {
                opal_output_verbose(20, mca_common_cuda_output,
                                    "CUDA: cuMemHostRegister OK on mpool %s: "
                                    "address=%p, bufsize=%d",
                                    mem_reg->msg, mem_reg->ptr, (int)mem_reg->amount);
            }
        }
        free(mem_reg->msg);
        OBJ_RELEASE(mem_reg);
    }

    opal_output_verbose(30, mca_common_cuda_output,
                        "CUDA: initialized");
    common_cuda_initialized = true;
}


/**
 * Call the CUDA register function so we pin the memory in the CUDA
 * space.
 */
void mca_common_cuda_register(void *ptr, size_t amount, char *msg) {
    int res;

    if (!common_cuda_initialized) {
        common_cuda_mem_regs_t *regptr;
        if (!common_cuda_init_function_added) {
            opal_cuda_add_initialization_function(&mca_common_cuda_init);
            OBJ_CONSTRUCT(&common_cuda_memory_registrations, opal_list_t);
            common_cuda_init_function_added = true;
        }
        regptr = OBJ_NEW(common_cuda_mem_regs_t);
        regptr->ptr = ptr;
        regptr->amount = amount;
        regptr->msg = strdup(msg);
        opal_list_append(&common_cuda_memory_registrations,
                         (opal_list_item_t*)regptr);
        return;
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

    assert(true == common_cuda_initialized);

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
