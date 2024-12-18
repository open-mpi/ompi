/*
 * Copyright (c) 2024      NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2014      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2014-2024 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_ACCELERATOR_EXPORT_H
#define MCA_COLL_ACCELERATOR_EXPORT_H

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

#define COLL_ACC_ALLGATHER             0x00000001
#define COLL_ACC_ALLGATHERV            0x00000002
#define COLL_ACC_ALLREDUCE             0x00000004
#define COLL_ACC_ALLTOALL              0x00000008
#define COLL_ACC_ALLTOALLV             0x00000010
#define COLL_ACC_ALLTOALLW             0x00000020
#define COLL_ACC_BARRIER               0x00000040
#define COLL_ACC_BCAST                 0x00000080
#define COLL_ACC_EXSCAN                0x00000100
#define COLL_ACC_GATHER                0x00000200
#define COLL_ACC_GATHERV               0x00000400
#define COLL_ACC_REDUCE                0x00000800
#define COLL_ACC_REDUCE_SCATTER        0x00001000
#define COLL_ACC_REDUCE_SCATTER_BLOCK  0x00002000
#define COLL_ACC_REDUCE_LOCAL          0x00004000
#define COLL_ACC_SCAN                  0x00008000
#define COLL_ACC_SCATTER               0x00010000
#define COLL_ACC_SCATTERV              0x00020000
#define COLL_ACC_NEIGHBOR_ALLGATHER    0x00040000
#define COLL_ACC_NEIGHBOR_ALLGATHERV   0x00080000
#define COLL_ACC_NEIGHBOR_ALLTOALL     0x00100000
#define COLL_ACC_NEIGHBOR_ALLTTOALLV   0x00200000
#define COLL_ACC_NEIGHBOR_ALLTTOALLW   0x00400000
#define COLL_ACC_LASTCOLL              0x00800000

#define COLL_ACCELERATOR_CTS_STR "allreduce,reduce_scatter_block,reduce_local,reduce,scan,exscan"
#define COLL_ACCELERATOR_CTS COLL_ACC_ALLREDUCE            | COLL_ACC_REDUCE       | \
                             COLL_ACC_REDUCE_SCATTER_BLOCK | COLL_ACC_REDUCE_LOCAL | \
                             COLL_ACC_EXSCAN               | COLL_ACC_SCAN

/* API functions */

int mca_coll_accelerator_init_query(bool enable_progress_threads,
                             bool enable_mpi_threads);
mca_coll_base_module_t
*mca_coll_accelerator_comm_query(struct ompi_communicator_t *comm,
                          int *priority);

int
mca_coll_accelerator_allreduce(const void *sbuf, void *rbuf, size_t count,
                        struct ompi_datatype_t *dtype,
                        struct ompi_op_t *op,
                        struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module);

int mca_coll_accelerator_reduce_local(const void *sbuf, void *rbuf, size_t count,
                         struct ompi_datatype_t *dtype,
                         struct ompi_op_t *op,
                         mca_coll_base_module_t *module);

int mca_coll_accelerator_reduce(const void *sbuf, void *rbuf, size_t count,
                         struct ompi_datatype_t *dtype,
                         struct ompi_op_t *op,
                         int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_accelerator_exscan(const void *sbuf, void *rbuf, size_t count,
                         struct ompi_datatype_t *dtype,
                         struct ompi_op_t *op,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module);

int mca_coll_accelerator_scan(const void *sbuf, void *rbuf, size_t count,
                       struct ompi_datatype_t *dtype,
                       struct ompi_op_t *op,
                       struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module);

int
mca_coll_accelerator_reduce_scatter_block(const void *sbuf, void *rbuf, size_t rcount,
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
static inline int mca_coll_accelerator_check_buf(void *addr, int *dev_id)
{
    uint64_t flags;

    if (OPAL_LIKELY(NULL != addr)) {
        return opal_accelerator.check_addr(addr, dev_id, &flags);
    } else {
        *dev_id = MCA_ACCELERATOR_NO_DEVICE_ID;
        return 0;
    }
}

static inline void *mca_coll_accelerator_memcpy(void *dest, int dest_dev, const void *src, int src_dev, size_t size,
						opal_accelerator_transfer_type_t type)
{
    int res;

    res = opal_accelerator.mem_copy(dest_dev, src_dev, dest, src, size, type);
    if (res != 0) {
        opal_output(0, "coll/accelerator: Error in mem_copy: res=%d, dest=%p, src=%p, size=%d", res, dest, src,
                    (int) size);
        abort();
    } else {
        return dest;
    }
}

/* Types */
/* Module */

typedef struct mca_coll_accelerator_module_t {
    mca_coll_base_module_t super;

    /* Pointers to all the "real" collective functions */
    mca_coll_base_comm_coll_t c_coll;
} mca_coll_accelerator_module_t;

OBJ_CLASS_DECLARATION(mca_coll_accelerator_module_t);

/* Component */

typedef struct mca_coll_accelerator_component_t {
    mca_coll_base_component_3_0_0_t super;

    int priority; /* Priority of this component */
    int disable_accelerator_coll;  /* Force disable of the accelerator collective component */
    char *cts; /* String of collective operations which the component shall register itself */
    uint64_t cts_requested;
} mca_coll_accelerator_component_t;

/* Globally exported variables */

OMPI_DECLSPEC extern mca_coll_accelerator_component_t mca_coll_accelerator_component;

END_C_DECLS

#endif /* MCA_COLL_ACCELERATOR_EXPORT_H */
