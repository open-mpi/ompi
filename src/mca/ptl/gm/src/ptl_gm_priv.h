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

#define  PTL_GM_FIRST_FRAG_SIZE (1<<14)
#define  PTL_GM_DBG_NONE    (0x000)
#define  PTL_GM_DBG_INIT    (0x001)
#define  PTL_GM_DBG_COMM    (0x002)

#define  PTL_GM_DBG_FLAG (PTL_GM_DBG_NONE)

#define  GM_DBG(flag, args...)                                 \
do {                                                           \
    if (PTL_GM_DBG_FLAG & flag) {                              \
	char hostname[32]; gethostname(hostname, 32);          \
	fprintf(stderr, "[%s:%s:%d] ",                         \
		hostname, __FUNCTION__, __LINE__);             \
	fprintf(stderr, args);                                 \
    }                                                          \
} while (0)


void ptl_gm_ctrl_frag(struct mca_ptl_gm_module_t *ptl,
                    mca_ptl_base_header_t * header);

mca_ptl_gm_recv_frag_t* ptl_gm_data_frag( struct mca_ptl_gm_module_t *ptl,
                                          gm_recv_event_t* event );

mca_ptl_gm_recv_frag_t* ptl_gm_handle_recv( mca_ptl_gm_module_t *ptl,
                                            gm_recv_event_t* event );

int mca_ptl_gm_incoming_recv (mca_ptl_gm_component_t * gm_comp);


void mca_ptl_gm_outstanding_recv(mca_ptl_gm_module_t *ptl);

int
mca_ptl_gm_peer_send(mca_ptl_gm_peer_t *ptl_peer,
                     mca_ptl_gm_send_frag_t *fragment,
                     struct mca_pml_base_send_request_t *sendreq,
                     size_t offset,
                     size_t *size,
                     int flags);

int
mca_ptl_gm_peer_put(mca_ptl_gm_peer_t *ptl_peer,
                     mca_ptl_gm_send_frag_t *fragment,
                     struct mca_pml_base_send_request_t *sendreq,
                     size_t offset,
                     size_t *size,
                     int flags,
                     void *target_buffer,
                     int bytes);



void send_callback(struct gm_port *port,void * context, gm_status_t
status);

void put_callback(struct gm_port *port,void * context, gm_status_t
status);

