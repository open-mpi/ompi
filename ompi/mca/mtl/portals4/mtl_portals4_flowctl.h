/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PORTALS_FLOWCTL_H
#define MTL_PORTALS_FLOWCTL_H

#include "opal/class/opal_free_list.h"

#include "mtl_portals4_request.h"

struct mca_mtl_base_endpoint_t;
struct ompi_mtl_portals4_isend_request_t;

struct ompi_mtl_portals4_pending_request_t {
    opal_free_list_item_t super;
    mca_pml_base_send_mode_t mode;
    void *start;
    size_t length;
    int contextid;
    int tag;
    int my_rank;
    int fc_notified;
    ptl_process_t *proc;
    struct ompi_mtl_portals4_isend_request_t *ptl_request;
};
typedef struct ompi_mtl_portals4_pending_request_t ompi_mtl_portals4_pending_request_t;
OBJ_CLASS_DECLARATION(ompi_mtl_portals4_pending_request_t);


struct ompi_mtl_portals4_flowctl_t {
    bool flowctl_active;

    int32_t send_slots;
    int32_t max_send_slots;
    opal_list_t pending_sends;
    opal_free_list_t pending_fl;

    ompi_mtl_portals4_base_request_t alert_req;
    ompi_mtl_portals4_base_request_t fanout_req;

    /** Flow control epoch counter.  Triggered events should be
        based on epoch counter. */
    uint32_t epoch_counter;

    /** Flow control trigger CT.  Only has meaning at root. */
    ptl_handle_ct_t trigger_ct_h;
    /** Flow control trigger ME.  Only has meaning at root.  When an
        event is received on this ME, it triggers the flow control
        alert broadcast.*/
    ptl_handle_me_t trigger_me_h;

    /** Flow control alert tree broadcast CT. */
    ptl_handle_ct_t alert_ct_h;
    /** Flow control alert tree broadcast ME. */
    ptl_handle_me_t alert_me_h;

    /** Flow control restart fan-in CT. */
    ptl_handle_ct_t fanin_ct_h;
    /** Flow control restart fan-in ME. */
    ptl_handle_me_t fanin_me_h;

    /** Flow control restart fan-out CT. */
    ptl_handle_ct_t fanout_ct_h;
    /** Flow control restart fan-out ME. */
    ptl_handle_me_t fanout_me_h;

    /** last restart time */
    struct timeval tv;
    int backoff_count;

    size_t num_procs;
    size_t num_children;
    ptl_process_t children[2];
    ptl_process_t parent;
    ptl_process_t me;
    ptl_process_t root;
    bool i_am_root;
};
typedef struct ompi_mtl_portals4_flowctl_t ompi_mtl_portals4_flowctl_t;


int ompi_mtl_portals4_flowctl_init(void);

int ompi_mtl_portals4_flowctl_fini(void);

int ompi_mtl_portals4_flowctl_add_procs(size_t me,
                                        size_t npeers,
                                        struct ompi_proc_t **procs);

int ompi_mtl_portals4_flowctl_trigger(void);

void ompi_mtl_portals4_pending_list_progress(void);

#endif
