#ifndef _IOF_BASE_ENDPOINT_
#define _IOF_BASE_ENDPOINT_

#include "ompi_config.h"
#include "class/ompi_list.h"
#include "event/event.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/iof_base_header.h"

enum {
    MCA_IOF_EP_OPEN,
    MCA_IOF_EP_CLOSING,
    MCA_IOF_EP_CLOSED
};

/**
 *  Structure that represents a published endpoint.
 */

struct mca_iof_base_endpoint_t {
    ompi_list_item_t super;
    mca_iof_base_mode_t ep_mode;
    ompi_process_name_t ep_name;
    int ep_tag;
    int ep_fd;
    int ep_state;
    uint32_t ep_seq;
    uint32_t ep_ack;
    ompi_event_t ep_event;
    ompi_list_t ep_frags;
};
typedef struct mca_iof_base_endpoint_t mca_iof_base_endpoint_t;

OBJ_CLASS_DECLARATION(mca_iof_base_endpoint_t);

/*
 * Diff between two sequence numbers allowing for rollover
 */

#define MCA_IOF_BASE_SEQDIFF(s1,s2)  \
    ((s1 > s2) ? (s1 - s2) : (s1 + (ULONG_MAX - s2)))


/**
 * Create a local endpoint.
 * 
 * @param name  Process name corresponding to endpoint.
 * @param mode  Source or sink of data (exclusive).
 * @param tag   Logical tag for matching.
 * @aram  fd    Local file descriptor corresponding to endpoint.
 */

int mca_iof_base_endpoint_create(
   const ompi_process_name_t* name,
   mca_iof_base_mode_t mode,
   int tag,
   int fd);

/**
 * Delete all local endpoints matching the specified 
 * name/mask/tag parameters.
 *
 * @paran name  Process name corresponding to one or more endpoint(s).
 * @param mask  Mask used for name comparisons.
 * @param tag   Tag for matching endpoints.
 */

int mca_iof_base_endpoint_delete(
   const ompi_process_name_t* name,
   ompi_ns_cmp_bitmask_t mask,
   int tag);

/**
 * Disable forwarding through the specified endpoint.
 */

int mca_iof_base_endpoint_close(
    mca_iof_base_endpoint_t* endpoint);

/**
 * Attempt to match an endpoint based on the destination
 * process name/mask/tag.
 */

mca_iof_base_endpoint_t* mca_iof_base_endpoint_match(
    const ompi_process_name_t* dst_name,
    ompi_ns_cmp_bitmask_t dst_mask,
    int dst_tag);

/**
 * Forward the specified message out the endpoint.
 */

int mca_iof_base_endpoint_forward(
    mca_iof_base_endpoint_t* endpoint,
    const ompi_process_name_t* src,
    mca_iof_base_msg_header_t* hdr,
    const unsigned char* data);

/*
 * Callback when peer has closed endpoint.
 */

void mca_iof_base_endpoint_closed(
    mca_iof_base_endpoint_t* endpoint);

/**
 * Callback when the specified sequence has been 
 * acknowledged.
 */

int mca_iof_base_endpoint_ack(
    mca_iof_base_endpoint_t* endpoint,
    uint32_t seq);

/**
 * Check for pending I/O
 */

static inline bool mca_iof_base_endpoint_pending(
    mca_iof_base_endpoint_t* endpoint)
{
    return ompi_list_get_size(&endpoint->ep_frags) || (endpoint->ep_seq != endpoint->ep_ack);
}

#endif

