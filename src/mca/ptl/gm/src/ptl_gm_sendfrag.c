/*
 *HEADER$
 */
#include <unistd.h>
#include <sys/types.h>
#include <sys/errno.h>
#include "include/types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_gm.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_proc.h"
#include "ptl_gm_sendfrag.h"

#define frag_header     super.super.frag_header
#define frag_owner      super.super.frag_owner
#define frag_peer       super.super.frag_peer
#define frag_convertor  super.super.frag_convertor


static void mca_ptl_gm_send_frag_construct (mca_ptl_gm_send_frag_t * frag);
static void mca_ptl_gm_send_frag_destruct (mca_ptl_gm_send_frag_t * frag);


ompi_class_t mca_ptl_gm_send_frag_t_class = {
    "mca_ptl_gm_send_frag_t",
    OBJ_CLASS (mca_ptl_base_send_frag_t),
    (ompi_construct_t) mca_ptl_gm_send_frag_construct,
    (ompi_destruct_t) mca_ptl_gm_send_frag_destruct
};

/*
 * send fragment constructor/destructors.
 */

static void
mca_ptl_gm_send_frag_construct (mca_ptl_gm_send_frag_t * frag)
{
}


static void
mca_ptl_gm_send_frag_destruct (mca_ptl_gm_send_frag_t * frag)
{
}


/*
static void send_callback(struct gm_port *, gm_status)
{
  gm_status_t status;

  switch (status)
    {
    case GM_SUCCESS:

      break;

    case GM_SEND_TIMED_OUT:
      break;

    case GM_SEND_DROPPED:
      break;

    default:
      break;

    }
}




static void put_callback(struct gm_port *, gm_status)
{

  gm_status_t status;


  switch (status)
    {
    case GM_SUCCESS:
      break;

    case GM_SEND_TIMED_OUT:
      break;

    case GM_SEND_DROPPED:
      break;

    default:
      break;

    }
}

*/
