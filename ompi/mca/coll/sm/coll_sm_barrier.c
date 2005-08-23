/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/include/constants.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "opal/include/sys/atomic.h"
#include "coll_sm.h"

#if 0
#define D(foo) printf foo
#else
#define D(foo)
#endif

/*
 *	barrier
 *
 *	Function:	- barrier 
 *	Accepts:	- same as MPI_Barrier()
 *	Returns:	- MPI_SUCCESS or error code
 */
int mca_coll_sm_barrier_intra(struct ompi_communicator_t *comm)
{
    mca_coll_base_comm_t *data = comm->c_coll_selected_data;
    uint32_t *my_control_in, *my_control_out;
    uint32_t *parent_control_in;
    int i, rank, segment;
    char *control_in, *control_out;
    int num_children = data->mcb_num_children;

    rank = ompi_comm_rank(comm);
    segment = (++data->mcb_operation_count % data->mcb_mpool_num_segments);
    control_in = data->mcb_mpool_index[segment]->mcbmi_control_fan_in;
    control_out = data->mcb_mpool_index[segment]->mcbmi_control_fan_out;

    /* Pre-calculate some pointers */

    my_control_in = (uint32_t *)
        (control_in + (rank * mca_coll_sm_component.sm_control_size));
    my_control_out = (uint32_t *)
        (control_out + (rank * mca_coll_sm_component.sm_control_size));
    *my_control_out = 0;

    if (0 != rank) {
        parent_control_in = (uint32_t *)
            (control_in + (data->mcb_parent_rank * 
                           mca_coll_sm_component.sm_control_size));
    } else {
        parent_control_in = NULL;
    }

    /* Fan in: wait for my children */

    if (0 != data->mcb_num_children) {
        D(("rank %d waiting for fan in from %d children...\n", rank, num_children));
        while (*my_control_in != (uint32_t) num_children) {
            opal_atomic_wmb();
        }
        *my_control_in = 0;
        D(("rank %d got fan in\n", rank));
    }

    /* Fan in: send to my parent */

    if (NULL != parent_control_in) {
        D(("rank %d writing to parent\n", rank));
        opal_atomic_add(parent_control_in, 1);
        D(("rank %d wrote to parent: %d\n", rank, *parent_control_in));
    }

    /* Fan out: wait for my parent to write to me (don't poll on
       parent's out buffer -- that would cause a lot of network
       traffic / contention / faults / etc. -- this way, the children
       poll on local memory and therefore only num_children messages
       are sent across the network [vs. num_children *each* time all
       the children poll] -- i.e., the memory is only being polled by
       one process, and it is only changed *once* by an external
       process) */

    if (NULL != parent_control_in) {
        D(("rank %d waiting for fan out from parent\n", rank));
        while (0 == *my_control_out) {
            opal_atomic_wmb();
        }
        D(("rank %d got fan out from parent\n", rank));
    }

    /* Fan out: send to my children */

    for (i = data->mcb_child_rank_start; i <= data->mcb_child_rank_end; ++i) {
        D(("rank %d sending to child %d, rank %d\n", 
           rank, i, i - data->mcb_child_rank_start));
        *((uint32_t *) 
          (control_out + (i * mca_coll_sm_component.sm_control_size))) = 1;
        
    }
    D(("rank %d done with barrier\n", rank));

    /* All done!  End state of the control segment:

       my_control_in: 0
       my_control_out: 1
    */

    return OMPI_SUCCESS;
}
