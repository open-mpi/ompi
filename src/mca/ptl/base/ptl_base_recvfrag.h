/*
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_BASE_RECVFRAG_H
#define MCA_PTL_BASE_RECVFRAG_H

#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_fragment.h"
#include "mca/ptl/base/ptl_base_match.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
extern ompi_class_t mca_ptl_base_recv_frag_t_class;

/**
 * Base type for receive fragment descriptors.
 */
struct mca_ptl_base_recv_frag_t {
    mca_ptl_base_frag_t frag_base; /**< base fragment descriptor */
    mca_pml_base_recv_request_t *frag_request; /**< matched posted receive */
    bool frag_is_buffered; /**< does fragment need to be unpacked into users buffer */
};
typedef struct mca_ptl_base_recv_frag_t mca_ptl_base_recv_frag_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

