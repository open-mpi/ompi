/* 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_SEND_FRAG_H
#define MCA_PTL_BASE_SEND_FRAG_H

#include "mca/ptl/ptl.h"
#include "mca/ptl/base/ptl_base_fragment.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
extern ompi_class_t mca_ptl_base_send_frag_t_class;

/**
 * Base type for send fragment descriptors 
 */
struct mca_ptl_base_send_frag_t {
    mca_ptl_base_frag_t frag_base;  /**< base fragment descriptor */
    struct mca_pml_base_send_request_t *frag_request;  /**< pointer to send request */
};
typedef struct mca_ptl_base_send_frag_t mca_ptl_base_send_frag_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

