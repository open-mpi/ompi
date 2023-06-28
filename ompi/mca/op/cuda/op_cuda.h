/*
 * Copyright (c) 2019-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_OP_CUDA_EXPORT_H
#define MCA_OP_CUDA_EXPORT_H

#include "ompi_config.h"

#include "ompi/mca/mca.h"
#include "opal/class/opal_object.h"

#include "ompi/mca/op/op.h"
#include "ompi/runtime/mpiruntime.h"

#include <cuda.h>
#include <cuda_runtime.h>

BEGIN_C_DECLS


#define xstr(x) #x
#define str(x) xstr(x)

#define CHECK(fn, args)                                       \
    do {                                                      \
        cudaError_t err = fn args;                            \
        if (err != cudaSuccess) {                             \
            fprintf(stderr, "%s:%d: %s failed at line: %s: %s\n", \
                    __FILE__, __LINE__, str(fn), cudaGetErrorName(err), \
                    cudaGetErrorString(err));                 \
            ompi_mpi_abort(MPI_COMM_WORLD, 1);                \
        }                                                     \
    } while (0)


/**
 * Derive a struct from the base op component struct, allowing us to
 * cache some component-specific information on our well-known
 * component struct.
 */
typedef struct {
    /** The base op component struct */
    ompi_op_base_component_1_0_0_t super;

#if 0
    /* a stream on which to schedule kernel calls */
    CUstream cu_stream;
    CUcontext *cu_ctx;
#endif // 0
    int *cu_max_threads_per_block;
    int *cu_max_blocks;
    CUdevice *cu_devices;
    int cu_num_devices;
} ompi_op_cuda_component_t;

/**
 * Globally exported variable.  Note that it is a *cuda* component
 * (defined above), which has the ompi_op_base_component_t as its
 * first member.  Hence, the MCA/op framework will find the data that
 * it expects in the first memory locations, but then the component
 * itself can cache additional information after that that can be used
 * by both the component and modules.
 */
OMPI_DECLSPEC extern ompi_op_cuda_component_t
    mca_op_cuda_component;

OMPI_DECLSPEC extern
ompi_op_base_stream_handler_fn_t ompi_op_cuda_functions[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];

OMPI_DECLSPEC extern
ompi_op_base_3buff_stream_handler_fn_t ompi_op_cuda_3buff_functions[OMPI_OP_BASE_FORTRAN_OP_MAX][OMPI_OP_BASE_TYPE_MAX];

END_C_DECLS

#endif /* MCA_OP_CUDA_EXPORT_H */
