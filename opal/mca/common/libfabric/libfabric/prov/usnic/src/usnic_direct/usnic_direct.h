/*
 * Copyright (c) 2014, Cisco Systems, Inc. All rights reserved.
 *
 * LICENSE_BEGIN
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * LICENSE_END
 *
 *
 */

#ifndef _USNIC_DIRECT_H_
#define _USNIC_DIRECT_H_

#include <sys/types.h>
#include <net/if.h>
#include <netinet/in.h>
#include <net/ethernet.h>
#include <netinet/ip.h>
#include <netinet/udp.h>

#define USD_MAX_DEVICES 8
#define USD_MAX_DEVNAME 16
#define USD_RECV_MAX_SGE 8

enum usd_link_state {
    USD_LINK_DOWN,
    USD_LINK_UP
};

/* forward structure defs */
struct usd_qp;
struct usd_cq_group;
struct usd_device;
struct usd_dest;
struct usd_connection;
struct usd_mr;

struct usd_device_attrs {
    char uda_devname[USD_MAX_DEVNAME];
    char uda_ifname[IFNAMSIZ];
    int uda_ifindex;
    uint8_t uda_mac_addr[ETH_ALEN];

    /* IP config */
    uint32_t uda_ipaddr_be;
    uint32_t uda_netmask_be;
    uint32_t uda_prefixlen;        /* netmask length */
    uint32_t uda_mtu;
    enum usd_link_state uda_link_state;

    /* HW info */
    uint32_t uda_vendor_id;
    uint32_t uda_vendor_part_id;
    uint32_t uda_device_id;
    char uda_firmware[64];

    /* usnic config */
    unsigned uda_num_vf;
    unsigned uda_cq_per_vf;
    unsigned uda_qp_per_vf;
    unsigned uda_max_cq;
    unsigned uda_max_qp;

    /* VIC constants */
    uint32_t uda_bandwidth;
    unsigned uda_max_cqe;
    unsigned uda_max_send_credits;
    unsigned uda_max_recv_credits;

    /* fd that can be used to poll for device events */
    int uda_event_fd;
};

enum usd_completion_status {
    USD_COMPSTAT_SUCCESS,
    USD_COMPSTAT_ERROR_CRC,
    USD_COMPSTAT_ERROR_TRUNC,
    USD_COMPSTAT_ERROR_TIMEOUT,
    USD_COMPSTAT_ERROR_INTERNAL
};
enum usd_completion_type {
    USD_COMPTYPE_SEND=0,
    USD_COMPTYPE_RECV=7,
};

struct usd_completion {
    enum usd_completion_status uc_status;
    enum usd_completion_type uc_type;
    uint32_t uc_bytes;
    uint16_t uc_rkey;
    struct usd_qp *uc_qp;
    void *uc_context;
    u_int16_t uc_retrans;
};

struct usd_recv_desc {
    void *urd_context;
    struct iovec urd_iov[USD_RECV_MAX_SGE];
    size_t urd_iov_cnt;
    struct usd_recv_desc *urd_next;
};

/*
 * Operations that may vary based on transport/QP type
 */
struct usd_qp_ops {
    int (*qo_post_send_one)(struct usd_qp *qp,
            struct usd_dest *dest, const void *buf, size_t len,
            uint32_t flags, void *context);
    int (*qo_post_send_one_prefixed)(struct usd_qp *qp,
            struct usd_dest *dest, const void *buf, size_t len,
            uint32_t flags, void *context);
    int (*qo_post_send_one_copy)(struct usd_qp *qp,
            struct usd_dest *dest, const void *buf, size_t len,
            uint32_t flags, void *context);
    int (*qo_post_send_two_copy)(struct usd_qp *qp,
            struct usd_dest *dest, const void *hdr, size_t hdrlen,
            const void *pkt, size_t pktlen, uint32_t flags, void *context);
};

/*
 * user's view of a CQ
 */
struct usd_cq {
    unsigned ucq_num_entries;
};

/*
 * User's view of a QP
 */
struct usd_qp {
    struct usd_qp_ops uq_ops;
    void *uq_context;           /* place for user to scribble */
};

/*
 * Filters for QPs
 */
enum usd_filter_type {
    USD_FTY_UDP,
    USD_FTY_UDP_SOCK,
    USD_FTY_TCP,
    USD_FTY_MCAST,
    USD_FTY_8915
};
struct usd_filter {
    enum usd_filter_type uf_type;
    union {
        struct {
            uint16_t u_port;
        } uf_udp;
        struct {
            int u_sock;
        } uf_udp_sock;
        struct {
            int t_sock;
            struct sockaddr_in t_remote;
        } uf_tcp;
        struct {
            struct sockaddr_in m_addr;
        } uf_mcast;
    } uf_filter;
};

/*
 * Local address - much like a filter
 * Type is defined by transport specified in create_qp
 */
struct usd_local_addr {
    union {
        struct {
            struct sockaddr_in u_addr;
        } ul_udp;
        struct {
            uint32_t qp_num;
        } ul_8915;
    } ul_addr;
};

enum usd_qp_transport {
    USD_QTR_RAW,    /* no header added */
    USD_QTR_UDP    /* create UDP header based on dest */
};

enum usd_qp_type {
    USD_QTY_NORMAL,
    USD_QTY_PIO,
};

/*
 * Attributes of a queue pair
 */
struct usd_qp_attrs {
    enum usd_qp_transport uqa_transport;
    enum usd_qp_type uqa_qtype;
    struct usd_local_addr uqa_local_addr;

    unsigned uqa_max_send_credits;
    unsigned uqa_max_recv_credits;
    uint64_t uqa_pio_paddr;

    unsigned uqa_max_inline;
    unsigned uqa_hdr_len;    /* length of header for this QP */
};

/*
 * Description of a device event which has occurred
 */
enum usd_device_event_type {
    USD_EVENT_LINK_UP,
    USD_EVENT_LINK_DOWN
};
struct usd_device_event {
    union {
        void *ude_context;
    } ude_context;
    enum usd_device_event_type ude_type;
};

/*
 * Returned form usd_get_available_devices() - array of currently
 * available usd device names
 */
struct usd_device_entry {
    char ude_devname[USD_MAX_DEVNAME];
};

/*
 * Send flags
 */
enum usd_send_flag_shift {
    USD_SFS_SIGNAL
};
#define USD_SF_SIGNAL (1 << USD_SFS_SIGNAL)

/*
 * Headers for defined transport types
 */
struct usd_udp_hdr {
    struct ether_header uh_eth;
    struct iphdr uh_ip;
    struct udphdr uh_udp;
} __attribute__ ((__packed__));


/*
 ****************************************************************
 * Device management
 ****************************************************************
 */
int usd_get_device_list(struct usd_device_entry *dev_array,
        int *num_devs);

int usd_open(const char *devname, struct usd_device **dev_o);

int usd_open_for_attrs(const char *devname, struct usd_device **dev_o);

int usd_close(struct usd_device *dev);

int usd_get_device_attrs(struct usd_device *dev,
        struct usd_device_attrs *attr);

int usd_get_device_event(struct usd_device *dev,
        struct usd_device_event *event);

enum usd_capability {
    USD_CAP_CQ_SHARING,
    USD_CAP_MAP_PER_RES,
    USD_CAP_PIO,
    USD_CAP_MAX
};
int usd_get_cap(struct usd_device *dev, enum usd_capability cap);

/*
 ****************************************************************
 * Queue management
 ****************************************************************
 */

#define USD_CQ_NO_GROUP (NULL)

/*
 * Get a file descriptor which can be used to poll
 * for completions
 */
int usd_get_completion_fd(struct usd_device *dev, int *comp_fd_o);

int usd_put_completion_fd(int comp_fd);

/*
 * Create a CQ group that will be shared over a set of QPs
 *   num_qp - able to be shared by this many QPs
 *   cq_group_id_o - CQ group ID returned
 */
int usd_create_cq_group(struct usd_device *dev, unsigned num_qp,
        struct usd_cq_group **cq_group_o);

/*
 * Destroy a CQ group
 */
int usd_destroy_cq_group(struct usd_cq_group *cq_group);

/*
 * Request a CQ with specified attributes:
 *   dev - device on which to create this CQ
 *   num_cqe - number of CQ entries
 *   cq_group - group ID for this CQ or USD_CQ_NO_GROUP for none
 *   comp_fd - completions will be signalled on this fd or -1 for none
 */
int usd_create_cq(struct usd_device *dev, unsigned num_cqe,
        struct usd_cq_group *cq_group, int comp_fd, struct usd_cq **cq_o);

int usd_destroy_cq(struct usd_cq *cq);

int usd_cq_intr_enable(struct usd_cq *cq);
int usd_cq_intr_disable(struct usd_cq *cq);

/*
 * Get and set interrupt coalescing delay, units are in microseconds
 */
int usd_cq_set_intr_coal(struct usd_cq *cq, unsigned intr_coal_delay);
unsigned usd_cq_get_intr_coal(struct usd_cq *cq);

/*
 * IN:
 *     dev - device on which QP is to be created
 *     transport - what transport to use on this queue
 *     type - type of queue to create
 *     wcq - CQ handle for send completions
 *     rcq - CQ handle for receive completions
 *     send_credits - Number of send credits requested
 *     recv_credite - Number of receive buffer credits requested
 *     port - Requested local port for QP (0 lets library choose)
 *     qp_o - Address to receive QP handle on successful completion
 * OUT:
 *     Returns 0 or code from errno.h
 *      0 - successful completion
 *      EBUSY - port is in use
 *      XXX
 */
int usd_create_qp(struct usd_device *dev,
        enum usd_qp_transport transport,
        enum usd_qp_type qtype,
        struct usd_cq *wcq, struct usd_cq *rcq,
        unsigned send_credits, unsigned recv_credits,
        struct usd_filter *filt, struct usd_qp **qp_o);

int usd_destroy_qp(struct usd_qp *qp);

int usd_enable_qp(struct usd_qp *qp);
int usd_disable_qp(struct usd_qp *qp);

int usd_get_qp_attrs(struct usd_qp *qp,
        struct usd_qp_attrs *qp_attrs_o);

/*
 * Add a filter to a QP
 */
int usd_qp_add_filter(struct usd_qp *qp, struct usd_filter *filter);

/*
 * Get current send credits
 */
unsigned usd_get_send_credits(struct usd_qp *uqp);

/*
 * Get current recv credits
 */
unsigned usd_get_recv_credits(struct usd_qp *uqp);

/*
 ****************************************************************
 * Memory management
 ****************************************************************
 */

int usd_reg_mr(struct usd_device *dev,
        void *buffer, size_t size, struct usd_mr **mr_o);
int usd_dereg_mr(struct usd_mr *mr);

int usd_alloc_mr(struct usd_device *dev, size_t size, void **vaddr_o);
int usd_free_mr(void *vaddr);

/*
 ****************************************************************
 * Destination management
 ****************************************************************
 */

/*
 * Return the distance metric to a specified IP address
 * Metric is:
 *    0 - same VLAN
 *    1..MAXINT - relative distance metric
 *    -1 - unreachable
 */
int usd_get_dest_distance(struct usd_device *dev, uint32_t daddr_be,
        int *metric_o);

/*
 * Settings for address resolution timeout and retry
 */
struct usd_dest_params {
    unsigned dp_arp_timeout;    /* per-try timeout in ms */
    unsigned dp_max_arps;
};

/*
 * Get address resolution settings
 */
int usd_get_dest_params(struct usd_dest_params *params);

/*
 * Set address resolution settings
 * Settings may not be changed while any resolution requests are in progress.
 */
int usd_set_dest_params(struct usd_dest_params *params);

/*
 * Used to create a destination with MAC address is already known.
 */
int usd_create_dest_with_mac(struct usd_device *dev, uint32_t daddr_be,
               uint16_t port_be, uint8_t *dmac, struct usd_dest **dest_o);

/*
 * Synchronously creates a destination
 */
int usd_create_dest(struct usd_device *dev, uint32_t daddr_be,
               uint16_t port_be, struct usd_dest **dest_o);

/*
 * Start the necessary ARP resolution to create a destination
 * Resolution progress is performed in usd_create_dest_query() and
 * usd_create_dest_poll()
 */
int usd_create_dest_start(struct usd_device *dev, uint32_t daddr_be,
               uint16_t dport_be, void *context);

/*
 * Cancel resolution on a not-yet-completed create_dest request
 */
int usd_create_dest_cancel(struct usd_device *dev, void *context);

/*
 * Extract dest port and IP from a destination
 */
int usd_expand_dest(struct usd_dest *dest, uint32_t *dest_ip_be_o,
        uint16_t *dest_port_be_o);

/*
 * Query completion status of a given create_dest request
 * If complete, newly allocated destination is returned in dest_o
 * Returns:
 *    0 - request completed, *status is valid
 *         dest_o valid if *status == 0
 *    -EAGAIN - nothing is complete
 *    other - negative errno code
 */
int usd_create_dest_query(struct usd_device *dev, void *context, int *status,
        struct usd_dest **dest_o);

/*
 * Checks for completed destination creation.
 * context specified in call to usd_create_dest_start is returned,
 * newly allocated destination is returned in dest_o
 * Returns:
 *    0 - request completed, status and context_o valid
 *         dest_o valid if *status == 0
 *    -EAGAIN - nothing is complete
 *    other - negative errno code
 */
int usd_create_dest_poll(struct usd_device *dev, void **context_o, int *status,
        struct usd_dest **dest_o);


int usd_destroy_dest(struct usd_dest *dest);

/*
 ****************************************************************
 * Sending, receiving, and completions
 ****************************************************************
 */

/*
 * Post a receive.  The number of receive credits consumed is equal
 * to the number of entries in the SG list of the recv_desc, or
 * recv_desc.urd_iov_cnt
 */
int usd_post_recv(struct usd_qp *qp,
        struct usd_recv_desc *recv_list);

int usd_poll_cq_multi(struct usd_cq *cq, int max_comps,
        struct usd_completion *comps);
int usd_poll_cq(struct usd_cq *cq, struct usd_completion *comp);

unsigned usd_get_send_credits(struct usd_qp *qp);

unsigned usd_get_recv_credits(struct usd_qp *qp);

/*
 * post a single-buffer send from registered memory
 * IN:
 *     qp
 *     dest
 *     buf -
 * Requires 2 send credits
 */
static inline int
usd_post_send_one(
    struct usd_qp *qp,
    struct usd_dest *dest,
    const void *buf,
    size_t len,
    uint32_t flags,
    void *context)
{
    return qp->uq_ops.qo_post_send_one(
            qp, dest, buf, len, flags, context);
}

/*
 * post a single-buffer send from registered memory
 * Caller must allow sufficient space *before* the packet for usd header
 * For optimal efficieny, the buffer should be aligned on XXX boundary
 * IN:
 *     qp
 *     dest
 *     buf -
 * Requires 1 send credit
 */
static inline int
usd_post_send_one_prefixed(
    struct usd_qp *qp,
    struct usd_dest *dest,
    const void *buf,
    size_t len,
    uint32_t flags,
    void *context)
{
    return qp->uq_ops.qo_post_send_one_prefixed(
            qp, dest, buf, len, flags, context);
}

/*
 * post a single-buffer send from anywhere
 * Data is copied into registered memory by the lib for sending
 * IN:
 *     qp
 *     dest
 *     buf -
 *     len - number of bytes in buffer, must be less than max_inline for the QP
 * Requires 1 send credit
 */
static inline int
usd_post_send_one_copy(struct usd_qp *qp, struct usd_dest *dest,
    const void *buf, size_t len, uint32_t flags, void *context)
{
    return qp->uq_ops.qo_post_send_one_copy(
            qp, dest, buf, len, flags, context);
}

/*
 * post a two-buffer send, the first buffer is a usually a header and must
 * allow space *before* it for our header.
 * For optimal efficieny, the first buffer should be aligned XXX
 * Requires 2 send credits
 */
int usd_post_send_two_prefixed(struct usd_qp *qp, struct usd_dest *dest,
        const void *hdr, size_t hdr_len, const void *pkt, size_t pkt_len,
        uint32_t flags, void *context);

/*
 * post a two-buffer send, the first buffer is a usually a header.
 * The header and the packet will be both be copied into registered
 * memory by usnic_direct and sent.
 * Requires 2 send credits
 */
static inline int
usd_post_send_two_copy(struct usd_qp *qp, struct usd_dest *dest,
    const void *hdr, size_t hdrlen, const void *pkt, size_t pktlen,
    uint32_t flags, void *context)
{
    return qp->uq_ops.qo_post_send_two_copy(
            qp, dest, hdr, hdrlen, pkt, pktlen, flags, context);
}

/*
 * Post an N-buffer send
 * All buffers must be in registered memory.
 * Requires iov_len + 1 send credits
 */
int usd_post_send_sge(struct usd_qp *qp, struct usd_dest *dest,
        const struct iovec *iov, size_t iov_len, uint32_t flags, void *context);

/****************************************************************
 * enum-to-string utility functions (for prettyprinting)
 ****************************************************************/

const char *usd_link_state_str(enum usd_link_state state);

const char *usd_completion_status_str(enum usd_completion_status cstatus);

const char *usd_completion_type_str(enum usd_completion_type ctype);

const char *usd_filter_type_str(enum usd_filter_type ftype);

const char *usd_qp_transport_str(enum usd_qp_transport qpt);

const char *usd_qp_type_str(enum usd_qp_type);

const char *usd_qp_event_event_type_str(enum usd_device_event_type det);

const char *usd_send_flag_sift_str(enum usd_send_flag_shift sfs);

const char *usd_capability(enum usd_capability cap);

/****************************************************************
 * special API holes punched for implementing verbs
 ****************************************************************/

/* open, but use caller's fd for commands */
int usd_open_with_fd(const char *devname, int cmd_fd, int check_ready,
        struct usd_device **dev_o);

/* modify the destination UDP port in a usd_dest */
void usd_dest_set_udp_ports(struct usd_dest *dest, struct usd_qp *src_qp,
        uint16_t dest_port_be);

/* create a dest with only IP addresses set */
int usd_create_ip_dest(struct usd_device *dev, uint32_t dest_ip_be,
        struct usd_dest **dest_o);

#endif /* _USNIC_DIRECT_H_ */
