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
#include "ptl_sm_frag.h"
#include "ptl_sm_address.h"


static void mca_ptl_sm_first_frag_construct(mca_ptl_sm_frag_t* frag);
static void mca_ptl_sm_first_frag_destruct(mca_ptl_sm_frag_t* frag);
static void mca_ptl_sm_second_frag_construct(mca_ptl_sm_frag_t* frag);
static void mca_ptl_sm_second_frag_destruct(mca_ptl_sm_frag_t* frag);

OBJ_CLASS_INSTANCE(
    mca_ptl_sm_frag_t,
    mca_ptl_base_recv_frag_t,
    mca_ptl_sm_first_frag_construct,
    mca_ptl_sm_first_frag_destruct
);

OBJ_CLASS_INSTANCE(
    mca_ptl_sm_second_frag_t,
    mca_ptl_base_recv_frag_t,
    mca_ptl_sm_second_frag_construct,
    mca_ptl_sm_second_frag_destruct
);
                                                                                                           

/*
 * shared memory recv fragment constructor
 */

static void mca_ptl_sm_first_frag_construct(mca_ptl_sm_frag_t* frag)
{
    char *ptr;

    /* set the buffer length */
    frag->buff_length=(size_t)mca_ptl_sm_component.first_fragment_size;
    
    /* set local rank */
    frag->queue_index=mca_ptl_sm_component.my_smp_rank;

    /* set pointer to the sending ptl */
    frag->send_ptl=(mca_ptl_base_module_t *)(&mca_ptl_sm);

    /* set buffer pointer */
    ptr=((char *)frag)+sizeof(mca_ptl_sm_frag_t)+
        mca_ptl_sm_component.fragment_alignment;
    /* align */
    ptr=ptr-(((size_t)ptr)%(mca_ptl_sm_component.fragment_alignment));
    frag->buff=ptr;
}


/*
 * shared memory recv fragment destructor
 */

static void mca_ptl_sm_first_frag_destruct(mca_ptl_sm_frag_t* frag)
{
}

/*
 * shared memory second and above fragments
 */

static void mca_ptl_sm_second_frag_construct(mca_ptl_sm_frag_t* frag)
{
    char *ptr;

    /* set the buffer length */
    frag->buff_length=(size_t)mca_ptl_sm_component.max_fragment_size;

    /* set local rank */
    frag->queue_index=mca_ptl_sm_component.my_smp_rank;

    /* set pointer to the sending ptl */
    frag->send_ptl=(mca_ptl_base_module_t *)(&mca_ptl_sm);

    /* set buffer pointer */
    ptr=((char *)frag)+sizeof(mca_ptl_sm_frag_t)+
        mca_ptl_sm_component.fragment_alignment;
    /* align */
    ptr=ptr-(((size_t)ptr)%(mca_ptl_sm_component.fragment_alignment));
    frag->buff=ptr;
}


/*
 * shared memory second and above fragments
 */

static void mca_ptl_sm_second_frag_destruct(mca_ptl_sm_frag_t* frag)
{
}

