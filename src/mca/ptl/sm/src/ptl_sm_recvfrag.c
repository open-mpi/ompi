/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "ptl_sm.h"
#include "ptl_sm_recvfrag.h"


static void mca_ptl_sm_recv_frag_construct(mca_ptl_sm_recv_frag_t* frag);
static void mca_ptl_sm_recv_frag_destruct(mca_ptl_sm_recv_frag_t* frag);

OBJ_CLASS_INSTANCE(
    mca_ptl_sm_recv_frag_t,
    mca_ptl_base_recv_frag_t,
    mca_ptl_sm_recv_frag_construct,
    mca_ptl_sm_recv_frag_destruct
);
                                                                                                           

/*
 * shared memory recv fragment constructor
 */

static void mca_ptl_sm_recv_frag_construct(mca_ptl_sm_recv_frag_t* frag)
{
}


/*
 * shared memory recv fragment destructor
 */

static void mca_ptl_sm_recv_frag_destruct(mca_ptl_sm_recv_frag_t* frag)
{
}

