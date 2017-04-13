/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "coll_sync.h"


/*
 *	gatherv
 *
 *	Function:	- gatherv
 *	Accepts:	- same arguments as MPI_Alltoallv()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_sync_alltoallw(const void *sbuf, const int *scounts, const int *sdisps,
                            struct ompi_datatype_t * const *sdtypes,
                            void *rbuf, const int *rcounts, const int *rdisps,
                            struct ompi_datatype_t * const *rdtypes,
                            struct ompi_communicator_t *comm,
                            mca_coll_base_module_t *module)
{
    mca_coll_sync_module_t *s = (mca_coll_sync_module_t*) module;

    if (s->in_operation) {
        return s->c_coll.coll_alltoallw(sbuf, scounts, sdisps, sdtypes,
                                        rbuf, rcounts, rdisps, rdtypes, comm,
                                        s->c_coll.coll_alltoallw_module);
    } else {
        do {
            int err = MPI_SUCCESS;
            s->in_operation = true;
            if (OPAL_UNLIKELY(++(s->before_num_operations_alltoallw) ==
                              mca_coll_sync_component.barrier_before_nops_alltoallw ||
                              ++(s->before_num_operations) ==
                              mca_coll_sync_component.barrier_before_nops)) {
                s->before_num_operations = 0;
                s->before_num_operations_alltoallw = 0;
                err = s->c_coll.coll_barrier(comm, s->c_coll.coll_barrier_module);
            }
            if (OPAL_LIKELY(MPI_SUCCESS == err))
                err = s->c_coll.coll_alltoallw(sbuf, scounts, sdisps, sdtypes,
                                               rbuf, rcounts, rdisps, rdtypes, comm,
                                               s->c_coll.coll_alltoallw_module);
            if (OPAL_UNLIKELY(++(s->after_num_operations_alltoallw) ==
                              mca_coll_sync_component.barrier_after_nops_alltoallw ||
                              ++(s->after_num_operations) ==
                              mca_coll_sync_component.barrier_after_nops)) {
                s->after_num_operations = 0;
                s->after_num_operations_alltoallw = 0;
                err = s->c_coll.coll_barrier(comm, s->c_coll.coll_barrier_module);
            }
            s->in_operation = false;
            return err;
        } while(0);
    }
}
