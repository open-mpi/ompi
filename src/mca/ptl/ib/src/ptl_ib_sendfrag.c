#include "include/types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_sendfrag.h"

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag);
static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_ptl_ib_send_frag_t, 
        mca_ptl_base_send_frag_t,
        mca_ptl_ib_send_frag_construct, 
        mca_ptl_ib_send_frag_destruct);

/*
 * Placeholders for send fragment constructor/destructors.
 */

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag)
{
    D_PRINT("\n");
}

static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag)
{
    D_PRINT("\n");
}

int mca_ptl_ib_send_frag_init(mca_ptl_ib_send_frag_t* ib_send_frag,
        struct mca_ptl_base_peer_t* base_peer,
        struct mca_pml_base_send_request_t* base_send_req,
        size_t offset,
        size_t* size,
        int flags)
{
    D_PRINT("\n");
    return OMPI_SUCCESS;
}
