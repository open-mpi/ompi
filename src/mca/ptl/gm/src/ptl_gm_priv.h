#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "ptl_gm_peer.h"
#include "ptl_gm_sendfrag.h"
#include "gm.h"

/* maintain list of registered buffers for send and receive */

/*struct reg_buf {*/
    /*void       *start;           pointer to registered memory */
    /*int         length;*/
/*};*/


void ptl_gm_ctrl_frag(struct mca_ptl_gm_module_t *ptl,
                    mca_ptl_base_header_t * header);

mca_ptl_gm_recv_frag_t* ptl_gm_data_frag( struct mca_ptl_gm_module_t *ptl,
                                          gm_recv_event_t* event );

mca_ptl_gm_recv_frag_t* ptl_gm_handle_recv( mca_ptl_gm_module_t *ptl,
                                            gm_recv_event_t* event );

int mca_ptl_gm_incoming_recv (mca_ptl_gm_component_t * gm_comp);

int
mca_ptl_gm_peer_send(mca_ptl_gm_peer_t *ptl_peer,
                     mca_ptl_gm_send_frag_t *fragment,
                     struct mca_pml_base_send_request_t *sendreq,
                     size_t offset,
                     size_t *size,
                     int flags);



void send_callback(struct gm_port *port,void * context, gm_status_t
status);

