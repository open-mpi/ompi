/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2015-2024 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      Intel, Inc. All rights reserved
 * Copyright (c) 2019-2021 The University of Tennessee at Chattanooga and The University
 *                         of Tennessee Research Foundation. All rights reserved.
 * Copyright (c) 2019-2021 Sandia National Laboratories. All rights reserved.
 * Copyright (c) 2021      University of Alabama at Birmingham. All rights reserved.
 * Copyright (c) 2021      Tennessee Technological University. All rights reserved.
 * Copyright (c) 2021      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021      Bull S.A.S. All rights reserved.
 * Copyright (c) 2024      High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PART_PERSIST_AGGREGATED_H
#define PART_PERSIST_AGGREGATED_H

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include <math.h>

#include "ompi_config.h"
#include "ompi/request/request.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/base/base.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/request/request.h"
#include "opal/sys/atomic.h"

#include "ompi/mca/part/persist_aggregated/part_persist_aggregated_request.h"
#include "ompi/mca/part/base/part_base_precvreq.h"
#include "ompi/mca/part/persist_aggregated/part_persist_aggregated_recvreq.h"
#include "ompi/mca/part/persist_aggregated/part_persist_aggregated_sendreq.h"
#include "ompi/message/message.h"
#include "ompi/mca/pml/pml.h"
BEGIN_C_DECLS

typedef struct mca_part_persist_aggregated_list_t {
    opal_list_item_t        super;
    mca_part_persist_aggregated_request_t *item;
} mca_part_persist_aggregated_list_t;

OPAL_DECLSPEC OBJ_CLASS_DECLARATION(mca_part_persist_aggregated_list_t);


struct ompi_part_persist_aggregated_t {
    mca_part_base_module_t super;
    int                    free_list_num;
    int                    free_list_max;
    int                    free_list_inc;
    opal_list_t           *progress_list;

    int32_t next_send_tag;                /**< This is a counter for send tags for the actual data transfer. */
    int32_t next_recv_tag; 
    ompi_communicator_t   *part_comm; /* This approach requires a separate tag space, so we need a dedicated communicator. */
    ompi_request_t        *part_comm_req;
    int32_t                part_comm_ready;
    ompi_communicator_t   *part_comm_setup; /* We create a second communicator to send set-up messages (rational: these 
                                               messages go in the opposite direction of normal messages, need to use MPI_ANY_SOURCE 
                                               to support different communicators, and thus need to have a unique tag. Because tags 
                                               are controlled by the sender in this model, we cannot assume that the tag will be 
                                               unused in part_comm. */
    ompi_request_t        *part_comm_sreq;
    int32_t                part_comm_sready;
    int32_t                init_comms;    
    int32_t                init_world;
    int32_t                my_world_rank; /* Because the back end communicators use a world rank, we need to communicate ours 
                                             to set up the requests. */
    opal_atomic_int32_t    block_entry;
    opal_mutex_t lock; 
};
typedef struct ompi_part_persist_aggregated_t ompi_part_persist_aggregated_t;
extern ompi_part_persist_aggregated_t ompi_part_persist_aggregated;

int mca_part_persist_aggregated_start(size_t, ompi_request_t**);
int mca_part_persist_aggregated_free(ompi_request_t**);

END_C_DECLS

#endif  /* PART_PERSIST_AGGREGATED_H_HAS_BEEN_INCLUDED */
