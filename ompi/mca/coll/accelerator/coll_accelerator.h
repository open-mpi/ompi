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

/* API functions */


extern int mca_coll_accelerator_bcast_thresh;
extern int mca_coll_accelerator_allgather_thresh;
extern int mca_coll_accelerator_alltoall_thresh;

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

int
mca_coll_accelerator_reduce_scatter(const void *sbuf, void *rbuf, ompi_count_array_t rcounts,
                                   struct ompi_datatype_t *dtype,
                                   struct ompi_op_t *op,
                                   struct ompi_communicator_t *comm,
                                   mca_coll_base_module_t *module);

int
mca_coll_accelerator_allgather(const void *sbuf, size_t scount,
			       struct ompi_datatype_t *sdtype,
			       void *rbuf, size_t rcount,
			       struct ompi_datatype_t *rdtype,
			       struct ompi_communicator_t *comm,
			       mca_coll_base_module_t *module);

int
mca_coll_accelerator_alltoall(const void *sbuf, size_t scount,
			      struct ompi_datatype_t *sdtype,
			      void *rbuf, size_t rcount,
			      struct ompi_datatype_t *rdtype,
			      struct ompi_communicator_t *comm,
			      mca_coll_base_module_t *module);

int
mca_coll_accelerator_bcast(void *buff, size_t count,
			   struct ompi_datatype_t *datatype,
			   int root,
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
} mca_coll_accelerator_component_t;

/* Globally exported variables */

OMPI_DECLSPEC extern mca_coll_accelerator_component_t mca_coll_accelerator_component;

END_C_DECLS

#endif /* MCA_COLL_ACCELERATOR_EXPORT_H */
