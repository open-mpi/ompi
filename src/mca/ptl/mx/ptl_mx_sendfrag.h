/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_MX_SEND_FRAG_H
#define MCA_PTL_MX_SEND_FRAG_H

#include "ompi_config.h"
#include "include/sys/atomic.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "ptl_mx.h"


/**
 * MX send fragment derived type.
 */
struct mca_ptl_mx_send_frag_t {
   mca_ptl_base_send_frag_t frag_send;  /**< base send fragment descriptor */
};
typedef struct mca_ptl_mx_send_frag_t mca_ptl_mx_send_frag_t;

#define MCA_PTL_MX_SEND_FRAG_ALLOC(item, rc)  \
    OMPI_FREE_LIST_GET(&mca_ptl_tcp_component.mx_send_frags, item, rc);

#define MCA_PTL_MX_SEND_FRAG_RETURN(item)  \
    OMPI_FREE_LIST_RETURN(&mca_ptl_mx_component.mx_send_frags, item);

OBJ_CLASS_DECLARATION(mca_ptl_mx_send_frag_t);

#endif

