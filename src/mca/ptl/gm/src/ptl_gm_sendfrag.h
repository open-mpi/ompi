/*
 *HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_GM_SEND_FRAG_H
#define MCA_PTL_GM_SEND_FRAG_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "os/atomic.h"
#include "ompi_config.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "ptl_gm.h"
#include "ptl_gm_priv.h"

OBJ_CLASS_DECLARATION (mca_ptl_gm_send_frag_t);

struct mca_ptl_base_peer_t;

/**
 * GM send fragment derived type.
 */
struct mca_ptl_gm_send_frag_t {
    mca_ptl_base_send_frag_t super; /**< base send fragment descriptor */
    struct reg_buf *sbuf;
};
typedef struct mca_ptl_gm_send_frag_t mca_ptl_gm_send_frag_t;


#define MCA_PTL_GM_SEND_FRAG_ALLOC(item, rc)  \
    OMPI_FREE_LIST_GET(&mca_ptl_gm_module.gm_send_frags, item, rc);


#endif
