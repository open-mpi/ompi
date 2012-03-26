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

struct ompi_mtl_portals4_flowctl_t {
    /** Flow control epoch counter.  Triggered events should be
        based on epoch counter. */
    uint32_t epoch_counter;
    /** Flow control trigger ME.  Only has meaning at root.  When an
        event is received on this ME, it triggers the flow control
        alert broadcast.*/
    ptl_handle_me_t trigger_me_h;
    /** Flow control trigger CT.  Only has meaning at root. */
    ptl_handle_ct_t trigger_ct_h;
    /** Flow control alert tree broadcast ME. */
    ptl_handle_me_t alert_me_h;
    /** Flow control alert tree broadcast CT. */
    ptl_handle_ct_t alert_ct_h;
    /** Flow control alert tree broadcast ME for a local put to
        generate an event */
    ptl_handle_me_t alert_event_me_h;
    /** Flow control restart fan-in ME. */
    ptl_handle_me_t fanin_me_h;
    /** Flow control restart fan-in CT. */
    ptl_handle_ct_t fanin_ct_h;
    /** Flow control restart fan-out ME. */
    ptl_handle_me_t fanout_me_h;
    /** Flow control restart fan-out CT. */
    ptl_handle_ct_t fanout_ct_h;
    /** Flow control restart fan-out ME for a local put to generate an
        event */
    ptl_handle_me_t fanout_event_me_h;

    size_t num_procs;
    size_t num_children;
    ptl_process_t children[2];
    ptl_process_t parent;
    ptl_process_t me;
    int i_am_root;
};
typedef struct ompi_mtl_portals4_flowctl_t ompi_mtl_portals4_flowctl_t;

/**
 * Initialize flow control code
 *
 * Initialize flow control code.  This includes initializing epoch
 * counter and creating necessary MEs and CTs.
 */
int ompi_mtl_portals4_flowctl_init(void);

int ompi_mtl_portals4_flowctl_fini(void);

int ompi_mtl_portals4_flowctl_add_procs(size_t me,
                                        size_t npeers,
                                        struct mca_mtl_base_endpoint_t **peers);
int ompi_mtl_portals4_flowctl_setup_comm(void);
int ompi_mtl_portals4_flowctl_start_recover(void);

#endif
