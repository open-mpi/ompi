/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "ptl_sm.h"
#include "ptl_sm_frag.h"


static void mca_ptl_sm_frag_construct(mca_ptl_sm_frag_t* frag);
static void mca_ptl_sm_frag_destruct(mca_ptl_sm_frag_t* frag);

OBJ_CLASS_INSTANCE(
    mca_ptl_sm_frag_t,
    mca_ptl_base_recv_frag_t,
    mca_ptl_sm_frag_construct,
    mca_ptl_sm_frag_destruct
);
                                                                                                           

/*
 * shared memory recv fragment constructor
 */

static void mca_ptl_sm_frag_construct(mca_ptl_sm_frag_t* frag)
{
    char *ptr;

    /* set the buffer length */
    frag->buff_length=(size_t)mca_ptl_sm_component.fragment_size;

    /* set buffer pointer */
    ptr=((char *)frag)+sizeof(mca_ptl_sm_frag_t)+
        mca_ptl_sm_component.fragment_alignment;
    /* align */
    ptr=ptr-(((size_t)ptr)%(mca_ptl_sm_component.fragment_alignment));
}


/*
 * shared memory recv fragment destructor
 */

static void mca_ptl_sm_frag_destruct(mca_ptl_sm_frag_t* frag)
{
}

