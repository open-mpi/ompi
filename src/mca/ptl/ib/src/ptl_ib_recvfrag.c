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

void
mca_ptl_ib_recv_frag_done (mca_ptl_base_header_t *header,
        mca_ptl_ib_recv_frag_t* frag,
        mca_pml_base_recv_request_t *request)
{
    frag->super.frag_base.frag_owner->ptl_recv_progress (
            frag->super.frag_base.frag_owner,
            request,
            frag->super.frag_base.frag_size,
            frag->super.frag_base.frag_size);

    OMPI_FREE_LIST_RETURN(&mca_ptl_ib_component.ib_recv_frags,
            (ompi_list_item_t*)frag);

#if 0
    mca_ptl_ib_recv_frag_return(
            frag->super.frag_base.frag_owner, frag);
    /* FIXME:
     * To support the required ACK, do not return
     * until the ack is out */
    if (frag->frag_ack_pending == false) {
    } else {
        /* XXX: Chaining it into the list of
         * completion pending recv_frag,
         *      * Until the ack frag is sent out,
         *      they will stay in the list */
    }
#endif
}

