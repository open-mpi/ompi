/*
 * $HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "types.h"
#include "datatype/datatype.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "mca/ptl/base/ptl_base_recvreq.h"
#include "ptl_elan.h"
#include "ptl_elan_peer.h"
#include "ptl_elan_proc.h"
#include "ptl_elan_frag.h"

#define frag_header     super.super.frag_header
#define frag_owner      super.super.frag_owner
#define frag_peer       super.super.frag_peer
#define frag_convertor  super.super.frag_convertor


static void mca_ptl_elan_send_frag_construct (mca_ptl_elan_send_frag_t *
                                              frag);
static void mca_ptl_elan_send_frag_destruct (mca_ptl_elan_send_frag_t *
                                             frag);

static void mca_ptl_elan_recv_frag_construct (mca_ptl_elan_recv_frag_t *
                                              frag);
static void mca_ptl_elan_recv_frag_destruct (mca_ptl_elan_recv_frag_t *
                                             frag);
static bool mca_ptl_elan_recv_frag_header (mca_ptl_elan_recv_frag_t * frag,
                                           int sd,
                                           size_t);
static      bool
mca_ptl_elan_recv_frag_ack (mca_ptl_elan_recv_frag_t * frag,
                            int sd);
static      bool
mca_ptl_elan_recv_frag_frag (mca_ptl_elan_recv_frag_t * frag,
                             int sd);
static      bool
mca_ptl_elan_recv_frag_match (mca_ptl_elan_recv_frag_t * frag,
                              int sd);
static      bool
mca_ptl_elan_recv_frag_data (mca_ptl_elan_recv_frag_t * frag,
                             int sd);
static      bool
mca_ptl_elan_recv_frag_discard (mca_ptl_elan_recv_frag_t * frag,
                                int sd);


ompi_class_t mca_ptl_elan_recv_frag_t_class = {
    "mca_ptl_elan_recv_frag_t",
    OBJ_CLASS (mca_ptl_base_recv_frag_t),
    (ompi_construct_t) mca_ptl_elan_recv_frag_construct,
    (ompi_destruct_t) mca_ptl_elan_recv_frag_destruct
};

/*
 * ELAN fragment constructor
 */

static void
mca_ptl_elan_recv_frag_construct (mca_ptl_elan_recv_frag_t * frag)
{
}


/*
 * ELAN fragment destructor
 */

static void
mca_ptl_elan_recv_frag_destruct (mca_ptl_elan_recv_frag_t * frag)
{
}


/*
 * Initialize a ELAN receive fragment for a specific peer.
 */

void
mca_ptl_elan_recv_frag_init (mca_ptl_elan_recv_frag_t * frag,
                             mca_ptl_elan_peer_t * peer)
{
}

/*
 * Callback from event library when socket has data available
 * for receive.
 */

bool
mca_ptl_elan_recv_frag_handler (mca_ptl_elan_recv_frag_t * frag,
                                int sd)
{
    return false;
}

/*
 * Receive fragment header 
 */

static      bool
mca_ptl_elan_recv_frag_header (mca_ptl_elan_recv_frag_t * frag,
                               int sd,
                               size_t size)
{
    return true;
}


/*
 * Receive and process an ack.
 */

static      bool
mca_ptl_elan_recv_frag_ack (mca_ptl_elan_recv_frag_t * frag,
                            int sd)
{
    return true;
}


/*
 * Receive and process a match request - first fragment.
 */

static      bool
mca_ptl_elan_recv_frag_match (mca_ptl_elan_recv_frag_t * frag,
                              int sd)
{
    return true;
}


/*
 * Receive and process 2nd+ fragments of a multi-fragment message.
 */

static      bool
mca_ptl_elan_recv_frag_frag (mca_ptl_elan_recv_frag_t * frag,
                             int sd)
{
    return true;
}


/*
 * Continue with non-blocking recv() calls until the entire
 * fragment is received.
 */

static      bool
mca_ptl_elan_recv_frag_data (mca_ptl_elan_recv_frag_t * frag,
                             int sd)
{
    return true;
}


/*
 *  If the app posted a receive buffer smaller than the
 *  fragment, receive and discard remaining bytes.
*/

static      bool
mca_ptl_elan_recv_frag_discard (mca_ptl_elan_recv_frag_t * frag,
                                int sd)
{
    return true;
}

ompi_class_t mca_ptl_elan_send_frag_t_class = {
    "mca_ptl_elan_send_frag_t",
    OBJ_CLASS (mca_ptl_base_send_frag_t),
    (ompi_construct_t) mca_ptl_elan_send_frag_construct,
    (ompi_destruct_t) mca_ptl_elan_send_frag_destruct
};

/*
 * Placeholders for send fragment constructor/destructors.
 */

static void
mca_ptl_elan_send_frag_construct (mca_ptl_elan_send_frag_t * frag)
{
}


static void
mca_ptl_elan_send_frag_destruct (mca_ptl_elan_send_frag_t * frag)
{
}


/*
 *  Initialize the fragment based on the current offset into the users
 *  data buffer, and the indicated size.
 */

int
mca_ptl_elan_send_frag_init (mca_ptl_elan_send_frag_t * sendfrag,
                             mca_ptl_elan_peer_t * ptl_peer,
                             mca_ptl_base_send_request_t * sendreq,
                             size_t offset,
                             size_t * size,
                             int flags)
{
    return OMPI_SUCCESS;
}


/*
 * The socket is setup as non-blocking, writes are handled asynchronously,
 * with event callbacks when the socket is ready for writes.
 */

bool
mca_ptl_elan_send_frag_handler (mca_ptl_elan_send_frag_t * frag,
                                int sd)
{
    return true;
}
