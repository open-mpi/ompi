/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "types.h"
#include "datatype/datatype.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "ptl_sm.h"
#include "ptl_sm_sendfrag.h"


static void mca_ptl_sm_send_frag_construct(mca_ptl_sm_send_frag_t* frag);
static void mca_ptl_sm_send_frag_destruct(mca_ptl_sm_send_frag_t* frag);


OBJ_CLASS_INSTANCE(
    mca_ptl_sm_send_frag_t, 
    mca_ptl_base_send_frag_t,
    mca_ptl_sm_send_frag_construct,
    mca_ptl_sm_send_frag_destruct);

                                                                                                           
/*
 * Placeholders for send fragment constructor/destructors.
 */

static void mca_ptl_sm_send_frag_construct(mca_ptl_sm_send_frag_t* frag)
{
}


static void mca_ptl_sm_send_frag_destruct(mca_ptl_sm_send_frag_t* frag)
{
}

