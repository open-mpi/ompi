#ifndef _IOF_BASE_FRAGMENT_
#define _IOF_BASE_FRAGMENT_

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_header.h"

#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

/**
 *
 */

struct mca_iof_base_frag_t {
    ompi_list_item_t super;
    mca_iof_base_header_t frag_hdr;
    ompi_process_name_t frag_src;
    unsigned char frag_data[MCA_IOF_BASE_MSG_MAX];
    unsigned char* frag_ptr;
    size_t frag_len;
    struct iovec frag_iov[2];
    struct mca_iof_base_endpoint_t* frag_owner;
};
typedef struct mca_iof_base_frag_t mca_iof_base_frag_t;

OBJ_CLASS_DECLARATION(mca_iof_base_frag_t);


/**
 *
 */

#define MCA_IOF_BASE_FRAG_ALLOC(frag,rc) { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&mca_iof_base.iof_fragments, item,rc); \
    if((frag = (mca_iof_base_frag_t*)item) == NULL) { \
        ompi_output(0, "MCA_IOF_BASE_FRAG_ALLOC failed with status=%d\n", rc); \
    } \
}

#define MCA_IOF_BASE_FRAG_RETURN(frag) \
    OMPI_FREE_LIST_RETURN(&mca_iof_base.iof_fragments, (ompi_list_item_t*)frag)


/**
 * Send an acknowledgment to the peer that this fragment has been received.
 */

int mca_iof_base_frag_ack(mca_iof_base_frag_t*);


#endif

