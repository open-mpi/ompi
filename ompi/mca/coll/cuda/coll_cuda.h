/*
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2015 NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_CUDA_EXPORT_H
#define MCA_COLL_CUDA_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"

#include "opal/class/opal_object.h"
#include "ompi/mca/mca.h"

#include "ompi/constants.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/communicator/communicator.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"

BEGIN_C_DECLS

/* API functions */

int mca_coll_cuda_init_query(bool enable_progress_threads,
                             bool enable_mpi_threads);
mca_coll_base_module_t
*mca_coll_cuda_comm_query(struct ompi_communicator_t *comm,
                          int *priority);

int mca_coll_cuda_module_enable(mca_coll_base_module_t *module,
                                struct ompi_communicator_t *comm);

int
mca_coll_cuda_allreduce(const void *sbuf, void *rbuf, int count,
                        struct ompi_datatype_t *dtype,
                        struct ompi_op_t *op,
                        struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_cuda_reduce(const void *sbuf, void *rbuf, int count,
                         struct ompi_datatype_t *dtype,
                         struct ompi_op_t *op,
                         int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_cuda_reduce_local(const void *sbuf, void *rbuf, int count,
                               struct ompi_datatype_t *dtype,
                               struct ompi_op_t *op,
                               mca_coll_base_module_t *module);

int mca_coll_cuda_exscan(const void *sbuf, void *rbuf, int count,
                         struct ompi_datatype_t *dtype,
                         struct ompi_op_t *op,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_cuda_scan(const void *sbuf, void *rbuf, int count,
                       struct ompi_datatype_t *dtype,
                       struct ompi_op_t *op,
                       struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module);

int
mca_coll_cuda_reduce_scatter_block(const void *sbuf, void *rbuf, int rcount,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);


/* Checks the type of pointer
 *
 * @param addr   One pointer to check
 * @retval <0                An error has occurred.
 * @retval 0                 The buffer is NULL or it does not belong to a managed buffer
 *                           in device memory.
 * @retval >0                The buffer belongs to a managed buffer in
 *                           device memory.
 */
static inline int mca_coll_cuda_check_buf(void *addr)
{
    uint64_t flags;
    int dev_id;
    if (OPAL_LIKELY(NULL != addr)) {
        return opal_accelerator.check_addr(addr, &dev_id, &flags);
    } else {
        return 0;
    }
}

static inline void *mca_coll_cuda_memcpy(void *dest, const void *src, size_t size)
{
    int res;
    res = opal_accelerator.mem_copy(MCA_ACCELERATOR_NO_DEVICE_ID, MCA_ACCELERATOR_NO_DEVICE_ID,
                                    dest, src, size, MCA_ACCELERATOR_TRANSFER_UNSPEC);
    if (res != 0) {
        opal_output(0, "CUDA: Error in cuMemcpy: res=%d, dest=%p, src=%p, size=%d", res, dest, src,
                    (int) size);
        abort();
    } else {
        return dest;
    }
}

/* Types */
/* Module */

typedef struct mca_coll_cuda_module_t {
    mca_coll_base_module_t super;

    /* Pointers to all the "real" collective functions */
    mca_coll_base_comm_coll_t c_coll;
} mca_coll_cuda_module_t;

OBJ_CLASS_DECLARATION(mca_coll_cuda_module_t);

/* Component */

typedef struct mca_coll_cuda_component_t {
    mca_coll_base_component_2_4_0_t super;

    int priority; /* Priority of this component */
    int disable_cuda_coll;  /* Force disable of the CUDA collective component */
} mca_coll_cuda_component_t;

/* Globally exported variables */

OMPI_DECLSPEC extern mca_coll_cuda_component_t mca_coll_cuda_component;

END_C_DECLS

#endif /* MCA_COLL_CUDA_EXPORT_H */
