/*
 * Copyright (c) 2006 Voltaire All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_MVAPI_EAGER_RDMA_BUF_H
#define MCA_BTL_MVAPI_EAGER_RDMA_BUF_H

#include "ompi_config.h"
#include "btl_mvapi.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_btl_mvapi_reg_t;

struct mca_btl_mvapi_eager_rdma_local_t {
	ompi_ptr_t base; /**< buffer for RDMAing eager messages */
	struct mca_btl_mvapi_reg_t *reg;
	uint16_t head; /**< RDMA buffer to poll */
    uint16_t tail; /**< Needed for credit managment */
	int32_t credits; /**< number of RDMA credits */
#ifdef OMPI_ENABLE_DEBUG
    uint32_t seq;
#endif
	opal_mutex_t lock; /**< guard access to RDMA buffer */
};
typedef struct mca_btl_mvapi_eager_rdma_local_t mca_btl_mvapi_eager_rdma_local_t;

struct mca_btl_mvapi_eager_rdma_remote_t {
	ompi_ptr_t base; /**< address of remote buffer */
	uint64_t rkey; /**< RKey for accessing remote buffer */
	uint16_t head; /**< RDMA buffer to post to */
	int32_t tokens; /**< number of rdam tokens */
#ifdef OMPI_ENABLE_DEBUG
    uint32_t seq;
#endif
};
typedef struct mca_btl_mvapi_eager_rdma_remote_t mca_btl_mvapi_eager_rdma_remote_t;

#define MCA_BTL_MVAPI_RDMA_FRAG(F) ((F)->type == MCA_BTL_MVAPI_FRAG_EAGER_RDMA)

#define EAGER_RDMA_BUFFER_REMOTE (0)
#define EAGER_RDMA_BUFFER_LOCAL (0xff)

#ifdef WORDS_BIGENDIAN
#define MCA_BTL_MVAPI_RDMA_FRAG_GET_SIZE(F) ((F)->u.size >> 8)
#define MCA_BTL_MVAPI_RDMA_FRAG_SET_SIZE(F, S) \
                                       ((F)->u.size = (S) << 8)
#else
#define MCA_BTL_MVAPI_RDMA_FRAG_GET_SIZE(F) ((F)->u.size & 0x00ffffff)
#define MCA_BTL_MVAPI_RDMA_FRAG_SET_SIZE(F, S) \
                                       ((F)->u.size = (S) & 0x00ffffff)
#endif

#define MCA_BTL_MVAPI_RDMA_FRAG_LOCAL(F)              \
                        (((volatile uint8_t*)(F)->ftr->u.buf)[3] != EAGER_RDMA_BUFFER_REMOTE)

#define MCA_BTL_MVAPI_RDMA_FRAG_REMOTE(F) \
                        (!MCA_BTL_MVAPI_RDMA_FRAG_LOCAL(F))

#define MCA_BTL_MVAPI_RDMA_MAKE_REMOTE(F) do {                        \
                             ((volatile uint8_t*)(F)->u.buf)[3] = EAGER_RDMA_BUFFER_REMOTE; \
                            }while (0)
        
#define MCA_BTL_MVAPI_RDMA_MAKE_LOCAL(F) do {                        \
                             ((volatile uint8_t*)(F)->u.buf)[3] = EAGER_RDMA_BUFFER_LOCAL; \
                            }while (0)

#define MCA_BTL_MVAPI_GET_LOCAL_RDMA_FRAG(E, I)                         \
            (mca_btl_mvapi_frag_t*)                                     \
            ((char*)(E)->eager_rdma_local.base.pval +                   \
            (I) * (E)->endpoint_btl->eager_rdma_frag_size)

#define MCA_BTL_MVAPI_RDMA_NEXT_INDEX(I) do {                       \
                            (I) = ((I) + 1) %                        \
                            mca_btl_mvapi_component.eager_rdma_num; \
                        } while (0)
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

