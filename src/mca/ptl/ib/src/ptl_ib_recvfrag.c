#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_recvfrag.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_recv_frag_construct(mca_ptl_ib_recv_frag_t* frag);
static void mca_ptl_ib_recv_frag_destruct(mca_ptl_ib_recv_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_ptl_ib_recv_frag_t, 
        mca_ptl_base_recv_frag_t,
        mca_ptl_ib_recv_frag_construct, 
        mca_ptl_ib_recv_frag_destruct);

/*
 * TCP fragment constructor
 */

static void mca_ptl_ib_recv_frag_construct(mca_ptl_ib_recv_frag_t* frag)
{
}


/*
 * TCP fragment destructor
 */

static void mca_ptl_ib_recv_frag_destruct(mca_ptl_ib_recv_frag_t* frag)
{
}
