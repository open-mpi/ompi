#ifndef _IOF_BASE_FRAGMENT_
#define _IOF_BASE_FRAGMENT_

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_header.h"


/**
 *  Fragment used to hold message header/data.
 */

struct orte_iof_base_frag_t {
    ompi_list_item_t super;
    orte_iof_base_header_t frag_hdr;
    orte_process_name_t frag_src;
    unsigned char frag_data[ORTE_IOF_BASE_MSG_MAX];
    unsigned char* frag_ptr;
    size_t frag_len;
    struct iovec frag_iov[2];
    struct orte_iof_base_endpoint_t* frag_owner;
};
typedef struct orte_iof_base_frag_t orte_iof_base_frag_t;

OBJ_CLASS_DECLARATION(orte_iof_base_frag_t);


/**
 *  Free-list allocation of fragments.
 */

#define ORTE_IOF_BASE_FRAG_ALLOC(frag,rc) { \
    ompi_list_item_t* item; \
    OMPI_FREE_LIST_GET(&orte_iof_base.iof_fragments, item,rc); \
    if((frag = (orte_iof_base_frag_t*)item) == NULL) { \
        ompi_output(0, "ORTE_IOF_BASE_FRAG_ALLOC failed with status=%d\n", rc); \
    } \
}

#define ORTE_IOF_BASE_FRAG_RETURN(frag) \
    OMPI_FREE_LIST_RETURN(&orte_iof_base.iof_fragments, (ompi_list_item_t*)frag)


/**
 * Send an acknowledgment to the peer that this fragment has been received.
 */

#define orte_iof_base_frag_ack(frag) _orte_iof_base_frag_ack(frag,__FILE__,__LINE__)
int _orte_iof_base_frag_ack(orte_iof_base_frag_t*, const char*, int);


#endif

