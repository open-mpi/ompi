/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mtl_portals4.h"
#include "mtl_portals4_flowctl.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_recv_short.h"


OBJ_CLASS_INSTANCE(ompi_mtl_portals4_pending_request_t, opal_free_list_item_t,
                   NULL, NULL);

static int
flowctl_alert_callback(ptl_event_t *ev,
                       ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    return ompi_mtl_portals4_flowctl_start_recover();
}


static int
flowctl_fanout_callback(ptl_event_t *ev,
                        ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    /* woo, we're recovered! */
    ompi_mtl_portals4.flowctl.flowctl_active = false;
    ompi_mtl_portals4_pending_list_progress();

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_flowctl_init(void)
{
    ptl_me_t me;
    int ret;

    ompi_mtl_portals4.flowctl.flowctl_active = false;

    OBJ_CONSTRUCT(&ompi_mtl_portals4.flowctl.active_sends, opal_list_t);

    OBJ_CONSTRUCT(&ompi_mtl_portals4.flowctl.pending_sends, opal_list_t);

    OBJ_CONSTRUCT(&ompi_mtl_portals4.flowctl.pending_fl, opal_free_list_t);
    opal_free_list_init(&ompi_mtl_portals4.flowctl.pending_fl,
                        sizeof(ompi_mtl_portals4_pending_request_t),
                        OBJ_CLASS(ompi_mtl_portals4_pending_request_t),
                        1, -1, 1);

    OBJ_CONSTRUCT(&ompi_mtl_portals4.flowctl.mutex, opal_mutex_t);

    ompi_mtl_portals4.flowctl.slots = (ompi_mtl_portals4.queue_size - 3) / 3;

    ompi_mtl_portals4.flowctl.alert_req.type = portals4_req_flowctl;
    ompi_mtl_portals4.flowctl.alert_req.event_callback = flowctl_alert_callback;

    ompi_mtl_portals4.flowctl.fanout_req.type = portals4_req_flowctl;
    ompi_mtl_portals4.flowctl.fanout_req.event_callback = flowctl_fanout_callback;

    ompi_mtl_portals4.flowctl.epoch_counter = 0;

    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_ONLY_TRUNCATE,
                     ompi_mtl_portals4.send_eq_h,
                     REQ_FLOWCTL_TABLE_ID,
                     &ompi_mtl_portals4.flowctl_idx);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.trigger_ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    me.start = NULL;
    me.length = 0;
    me.min_free = 0;
    me.uid = PTL_UID_ANY;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.ignore_bits = 0;

    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_COMM_DISABLE |
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.trigger_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_TRIGGER;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &ompi_mtl_portals4.flowctl.trigger_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.alert_ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_COMM_DISABLE |
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.alert_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_ALERT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &ompi_mtl_portals4.flowctl.alert_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE;
    me.ct_handle = PTL_CT_NONE;
    me.match_bits = MTL_PORTALS4_FLOWCTL_ALERT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &ompi_mtl_portals4.flowctl.alert_event_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.fanin_ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_COMM_DISABLE |
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.fanin_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_FANIN;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &ompi_mtl_portals4.flowctl.fanin_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.fanout_ct_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_COMM_DISABLE |
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.fanout_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_FANOUT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &ompi_mtl_portals4.flowctl.fanout_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE;
    me.ct_handle = PTL_CT_NONE;
    me.match_bits = MTL_PORTALS4_FLOWCTL_FANOUT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &ompi_mtl_portals4.flowctl.fanout_event_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ompi_mtl_portals4.flowctl.num_children = 0;

    ret = OMPI_SUCCESS;

 error:
    return ret;
}


int
ompi_mtl_portals4_flowctl_fini(void)
{
    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_flowctl_add_procs(size_t me,
                                    size_t npeers,
                                    struct mca_mtl_base_endpoint_t **peers)
{
    int i;

    /* if epoch isn't 0, that means setup trees has been called, which
       means that this add_procs is a dynamic process, which we don't
       support */
    if (ompi_mtl_portals4.flowctl.epoch_counter != 1) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    ompi_mtl_portals4.flowctl.num_procs = npeers;
    if (0 == me) {
        ompi_mtl_portals4.flowctl.i_am_root = 1;
    } else {
        ompi_mtl_portals4.flowctl.parent = 
            peers[(me - 1) / 2]->ptl_proc;
    }
    ompi_mtl_portals4.flowctl.me = peers[me]->ptl_proc;

    for (i = 0 ; i < 2 ; ++i) {
        size_t tmp = (2 * me) + i + 1;
        if (tmp < npeers) {
            ompi_mtl_portals4.flowctl.num_children++;
            ompi_mtl_portals4.flowctl.children[i] = peers[tmp]->ptl_proc;
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_flowctl_setup_comm(void)
{
    int ret;
    size_t i;

    /* increase the flow control epoch count */
    ompi_mtl_portals4.flowctl.epoch_counter++;

    ret = PtlStartBundle(ompi_mtl_portals4.ni_h);
    if (PTL_OK != ret) {
        return ret;
    }

    /* setup the trigger to start the alert broadcast */
    if (0 != ompi_mtl_portals4.flowctl.i_am_root) {
        int counter = 
            ompi_mtl_portals4.flowctl.epoch_counter *
            (ompi_mtl_portals4.flowctl.num_procs + 1);

        ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_mtl_portals4.flowctl.me,
                              ompi_mtl_portals4.flowctl_idx,
                              MTL_PORTALS4_FLOWCTL_ALERT,
                              0,
                              NULL,
                              0,
                              ompi_mtl_portals4.flowctl.trigger_ct_h,
                              counter);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto error;
        }
    }

    /* setup the alert broadcast tree */
    for (i = 0 ; i < ompi_mtl_portals4.flowctl.num_children ; ++i) {
        ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_mtl_portals4.flowctl.children[i],
                              ompi_mtl_portals4.flowctl_idx,
                              MTL_PORTALS4_FLOWCTL_ALERT,
                              0,
                              NULL,
                              0,
                              ompi_mtl_portals4.flowctl.trigger_ct_h,
                              ompi_mtl_portals4.flowctl.epoch_counter);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto error;
        }
    }

    /* setup the trigger to generate a full event on alert */
    ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                          0,
                          0,
                          PTL_NO_ACK_REQ,
                          ompi_mtl_portals4.flowctl.me,
                          ompi_mtl_portals4.flowctl_idx,
                          MTL_PORTALS4_FLOWCTL_ALERT,
                          0,
                          NULL,
                          0,
                          ompi_mtl_portals4.flowctl.alert_ct_h,
                          ompi_mtl_portals4.flowctl.epoch_counter);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlTriggeredPut failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* setup the restart barrier */
    if (0 != ompi_mtl_portals4.flowctl.i_am_root) {
        ptl_ct_event_t new_ct;

        /* turn to fanout part of restart */
        for (i = 0 ; i < ompi_mtl_portals4.flowctl.num_children ; ++i) {
            ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                                  0,
                                  0,
                                  PTL_NO_ACK_REQ,
                                  ompi_mtl_portals4.flowctl.children[i],
                                  ompi_mtl_portals4.flowctl_idx,
                                  MTL_PORTALS4_FLOWCTL_FANOUT,
                                  0,
                                  NULL,
                                  0,
                                  ompi_mtl_portals4.flowctl.fanin_ct_h,
                                  ompi_mtl_portals4.flowctl.epoch_counter *
                                  (ompi_mtl_portals4.flowctl.num_children + 1));
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_mtl_base_output,
                                    "%s:%d: PtlTriggeredPut failed: %d\n",
                                    __FILE__, __LINE__, ret);
                goto error;
            }
        }
        
        /* reset trigger ME */
        new_ct.success = ompi_mtl_portals4.flowctl.epoch_counter *
            (ompi_mtl_portals4.flowctl.num_children + 1);
        ret = PtlTriggeredCTSet(ompi_mtl_portals4.flowctl.trigger_ct_h,
                                new_ct,
                                ompi_mtl_portals4.flowctl.fanin_ct_h,
                                ompi_mtl_portals4.flowctl.epoch_counter *
                                (ompi_mtl_portals4.flowctl.num_children + 1));

        /* setup the trigger to generate a full event on fan-out */
        ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_mtl_portals4.flowctl.me,
                              ompi_mtl_portals4.flowctl_idx,
                              MTL_PORTALS4_FLOWCTL_FANOUT,
                              0,
                              NULL,
                              0,
                              ompi_mtl_portals4.flowctl.fanin_ct_h,
                              ompi_mtl_portals4.flowctl.epoch_counter *
                              (ompi_mtl_portals4.flowctl.num_children + 1));
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto error;
        }

    } else {
        /* fan-in */
        ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_mtl_portals4.flowctl.parent,
                              ompi_mtl_portals4.flowctl_idx,
                              MTL_PORTALS4_FLOWCTL_FANIN,
                              0,
                              NULL,
                              0,
                              ompi_mtl_portals4.flowctl.fanin_ct_h,
                              ompi_mtl_portals4.flowctl.epoch_counter *
                              (ompi_mtl_portals4.flowctl.num_children + 1));
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto error;
        }

        /* fan-out */
        for (i = 0 ; i < ompi_mtl_portals4.flowctl.num_children ; ++i) {
            ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                                  0,
                                  0,
                                  PTL_NO_ACK_REQ,
                                  ompi_mtl_portals4.flowctl.children[i],
                                  ompi_mtl_portals4.flowctl_idx,
                                  MTL_PORTALS4_FLOWCTL_FANOUT,
                                  0,
                                  NULL,
                                  0,
                                  ompi_mtl_portals4.flowctl.fanout_ct_h,
                                  ompi_mtl_portals4.flowctl.epoch_counter);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_mtl_base_output,
                                    "%s:%d: PtlTriggeredPut failed: %d\n",
                                    __FILE__, __LINE__, ret);
                goto error;
            }
        }

        /* setup the trigger to generate a full event on fan-out */
        ret = PtlTriggeredPut(ompi_mtl_portals4.zero_md_h,
                              0,
                              0,
                              PTL_NO_ACK_REQ,
                              ompi_mtl_portals4.flowctl.me,
                              ompi_mtl_portals4.flowctl_idx,
                              MTL_PORTALS4_FLOWCTL_FANOUT,
                              0,
                              NULL,
                              0,
                              ompi_mtl_portals4.flowctl.fanout_ct_h,
                              ompi_mtl_portals4.flowctl.epoch_counter);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto error;
        }
    }

 error:
    ret = PtlEndBundle(ompi_mtl_portals4.ni_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlEndBundle failed: %d\n",
                            __FILE__, __LINE__, ret);
    }

    return ret;
}


int
ompi_mtl_portals4_flowctl_start_recover(void)
{
    int ret;

    if (ompi_mtl_portals4.flowctl.flowctl_active) {
        return OMPI_SUCCESS;
    } else {
        ompi_mtl_portals4.flowctl.flowctl_active = true;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "entering flowctl_start_recover"));

    ret = ompi_mtl_portals4_flowctl_setup_comm();
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: flowctl_setup_comm failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* drain all pending sends */
    while (0 != opal_list_get_size(&ompi_mtl_portals4.flowctl.active_sends)) {
        opal_progress();
    }

    /* drain event queue */
    while (0 != ompi_mtl_portals4_progress()) { ; }

    /* check short block active count */
    ret = ompi_mtl_portals4_recv_short_link(1);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: recv_short_link failed: %d\n",
                            __FILE__, __LINE__, ret);
    }

    /* send barrier entry message */
    ret = PtlPut(ompi_mtl_portals4.zero_md_h,
                 0,
                 0,
                 PTL_NO_ACK_REQ,
                 ompi_mtl_portals4.flowctl.me,
                 ompi_mtl_portals4.flowctl_idx,
                 MTL_PORTALS4_FLOWCTL_FANIN,
                 0,
                 NULL,
                 0);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPut failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* recovery complete when fan-out event arrives, async event, so
       we're done now */
    
    ret = OMPI_SUCCESS;
 error:
    return ret;
}
