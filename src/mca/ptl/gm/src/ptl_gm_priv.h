/* -*- Mode: C; c-basic-offset:4 ; -*- */

#include "gm.h"

struct mca_ptl_gm_send_frag_t;
struct mca_ptl_gm_peer_t;

#define  PTL_GM_FIRST_FRAG_SIZE (1<<14)
#define  PTL_GM_DBG_NONE    (0x000)
#define  PTL_GM_DBG_INIT    (0x001)
#define  PTL_GM_DBG_COMM    (0x002)

#define  PTL_GM_DBG_FLAG (PTL_GM_DBG_NONE)

/*#define DO_DEBUG(inst)  inst*/
#define DO_DEBUG(inst)

#define  GM_DBG(flag, args...)                                 \
do {                                                           \
    if (PTL_GM_DBG_FLAG & flag) {                              \
	char hostname[32]; gethostname(hostname, 32);              \
	fprintf(stderr, "[%s:%s:%d] ",                             \
		hostname, __FUNCTION__, __LINE__);                     \
	fprintf(stderr, args);                                     \
    }                                                          \
} while (0)

#if 0
#define A_PRINT(fmt, args...) {                                     \
    ompi_output(0, "[%s:%d:%s] " fmt, __FILE__, __LINE__, __func__, \
        ##args);                                                    \
}
#else
#define A_PRINT(fmt, args...)
#endif

int mca_ptl_gm_analyze_recv_event( struct mca_ptl_gm_module_t* ptl, gm_recv_event_t* event );

void mca_ptl_gm_outstanding_recv( struct mca_ptl_gm_module_t *ptl);

int
mca_ptl_gm_peer_send( struct mca_ptl_gm_peer_t *ptl_peer,
		      struct mca_ptl_gm_send_frag_t *fragment,
		      struct mca_pml_base_send_request_t *sendreq,
		      size_t offset,
		      size_t *size,
		      int flags );

int
mca_ptl_gm_peer_put( struct mca_ptl_gm_peer_t *ptl_peer,
                     struct mca_ptl_gm_send_frag_t *fragment,
                     struct mca_pml_base_send_request_t *sendreq,
                     size_t offset,
                     size_t *size,
                     int flags,
                     void *target_buffer,
                     int bytes );



void send_callback(struct gm_port *port,void * context, gm_status_t status);

void put_callback(struct gm_port *port,void * context, gm_status_t status);

