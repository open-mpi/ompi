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
 * IB fragment constructor
 */

static void mca_ptl_ib_recv_frag_construct(mca_ptl_ib_recv_frag_t* frag)
{
}


/*
 * IB fragment destructor
 */

static void mca_ptl_ib_recv_frag_destruct(mca_ptl_ib_recv_frag_t* frag)
{
}

void
mca_ptl_ib_recv_frag_done (mca_ptl_base_header_t *header,
        mca_ptl_base_recv_frag_t* frag,
        mca_pml_base_recv_request_t *request)
{
    D_PRINT("");
    frag->frag_base.frag_owner->ptl_recv_progress (
            frag->frag_base.frag_owner,
            request,
            frag->frag_base.frag_size,
            frag->frag_base.frag_size);

    /* Return recv frag to free list */
    OMPI_FREE_LIST_RETURN(&mca_ptl_ib_component.ib_recv_frags,
            (ompi_list_item_t*)frag);
}

/*
 * Process incoming receive fragments
 *
 */

void mca_ptl_ib_process_recv(mca_ptl_base_module_t *module, void* addr)
{
    bool matched;
    int rc;
    ib_buffer_t *ib_buf;
    mca_ptl_base_header_t *header;
    ompi_list_item_t *item;
    mca_ptl_ib_recv_frag_t *recv_frag;

    ib_buf = (ib_buffer_t *) (unsigned int) addr;

    header = (mca_ptl_base_header_t *) &ib_buf->buf[0];

    OMPI_FREE_LIST_GET(&mca_ptl_ib_component.ib_recv_frags,
            item, rc);

    while (OMPI_SUCCESS != rc) {
        /* TODO: progress the recv state machine */
        D_PRINT("Retry to allocate a recv fragment\n");
        OMPI_FREE_LIST_GET (&mca_ptl_ib_component.ib_recv_frags,
                item, rc);
    }

    recv_frag = (mca_ptl_ib_recv_frag_t *) item;
    recv_frag->super.frag_base.frag_owner = 
        (mca_ptl_base_module_t *) module; 

    recv_frag->super.frag_base.frag_peer = NULL; 
    recv_frag->super.frag_request = NULL; 
    recv_frag->super.frag_is_buffered = false;

    /* Copy the header, mca_ptl_base_match()
     * does not do what it claims */
    recv_frag->super.frag_base.frag_header = *header;

    /* Taking the data starting point be
     * default */
    recv_frag->super.frag_base.frag_addr =
        (char *) header + sizeof (mca_ptl_base_header_t);
    recv_frag->super.frag_base.frag_size = header->hdr_frag.hdr_frag_length;

    /* match with preposted
     * requests */
    matched = module->ptl_match(
            recv_frag->super.frag_base.frag_owner,
            &recv_frag->super,
            &recv_frag->super.frag_base.frag_header.hdr_match);

    if (!matched) {
        /* Oh my GOD
         * !!! */
        D_PRINT("Can't match buffer. Mama is unhappy\n");
        memcpy (recv_frag->unex_buf,
                (char *) header + sizeof (mca_ptl_base_header_t),
                header->hdr_frag.hdr_frag_length);
        recv_frag->super.frag_is_buffered = true; 
        recv_frag->super.frag_base.frag_addr = recv_frag->unex_buf;

    } else {
        D_PRINT("Message matched!");
    }
}
