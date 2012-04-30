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

static int flowctl_alert_callback(ptl_event_t *ev,
                                  ompi_mtl_portals4_base_request_t *ptl_base_request);
static int flowctl_fanin_callback(ptl_event_t *ev,
                                  ompi_mtl_portals4_base_request_t *ptl_base_request);
static int flowctl_fanout_callback(ptl_event_t *ev,
                                   ompi_mtl_portals4_base_request_t *ptl_base_request);

static int start_recover(void);
static int setup_alarm(uint32_t epoch);

int
ompi_mtl_portals4_flowctl_init(void)
{
    ptl_me_t me;
    int ret;

    ompi_mtl_portals4.flowctl.flowctl_active = false;
    ompi_mtl_portals4.flowctl.send_alert = true;

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

    ompi_mtl_portals4.flowctl.fanin_req.type = portals4_req_flowctl;
    ompi_mtl_portals4.flowctl.fanin_req.event_callback = flowctl_fanin_callback;

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

    /* everyone creates the trigger ME, even if the root may be the
       only to use it */
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


    /* Alert CT/ME for broadcasting out alert when root receives a
       trigger */
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
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.alert_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_ALERT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      &ompi_mtl_portals4.flowctl.alert_req,
                      &ompi_mtl_portals4.flowctl.alert_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Fanin CT/ME for receiving fan-in for restart */
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
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.fanin_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_FANIN;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      &ompi_mtl_portals4.flowctl.fanin_req,
                      &ompi_mtl_portals4.flowctl.fanin_me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Fan-out CT/ME for sending restart messages after fan-in */
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
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.fanout_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_FANOUT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      &ompi_mtl_portals4.flowctl.fanout_req,
                      &ompi_mtl_portals4.flowctl.fanout_me_h);
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
    /* BWB: FIX ME */
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
    if (ompi_mtl_portals4.flowctl.epoch_counter != 0) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    ompi_mtl_portals4.flowctl.num_procs = npeers;
    ompi_mtl_portals4.flowctl.root = peers[0]->ptl_proc;
    if (0 == me) {
        ompi_mtl_portals4.flowctl.i_am_root = true;
    } else {
        ompi_mtl_portals4.flowctl.i_am_root = false;
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

    return setup_alarm(ompi_mtl_portals4.flowctl.epoch_counter);
}


int
ompi_mtl_portals4_flowctl_trigger(void)
{
    int ret;

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "Enter flowctl_trigger"));

    if (false == ompi_mtl_portals4.flowctl.flowctl_active) {
        ompi_mtl_portals4.flowctl.flowctl_active = true;

        /* send trigger to root */
        OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                             "Sending flow control trigger"));
        ret = PtlPut(ompi_mtl_portals4.zero_md_h,
                     0,
                     0,
                     PTL_NO_ACK_REQ,
                     ompi_mtl_portals4.flowctl.root,
                     ompi_mtl_portals4.flowctl_idx,
                     MTL_PORTALS4_FLOWCTL_TRIGGER,
                     0,
                     NULL,
                     0);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


static int
start_recover(void)
{
    int ret;

    ompi_mtl_portals4.flowctl.flowctl_active = true;

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "Entering flowctl_start_recover %d",
                         ompi_mtl_portals4.flowctl.epoch_counter));

    /* re-arm trigger/alarm for next time */
    ret = setup_alarm(ompi_mtl_portals4.flowctl.epoch_counter + 1);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d setup_alarm failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "flowctl_start_recover %d: draining active sends",
                         ompi_mtl_portals4.flowctl.epoch_counter));

    /* drain all pending sends */
    while (0 != opal_list_get_size(&ompi_mtl_portals4.flowctl.active_sends)) {
        opal_progress();
    }

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "flowctl_start_recover %d: draining event queue",
                         ompi_mtl_portals4.flowctl.epoch_counter));

    /* drain event queue */
    while (0 != ompi_mtl_portals4_progress()) { ; }

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "flowctl_start_recover %d: checking short blocks",
                         ompi_mtl_portals4.flowctl.epoch_counter));

    /* check short block active count */
    ret = ompi_mtl_portals4_recv_short_link(1);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: recv_short_link failed: %d",
                            __FILE__, __LINE__, ret);
    }

    /* drain event queue */
    while (0 != ompi_mtl_portals4_progress()) { ; }

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "flowctl_start_recover %d: starting barrier.  Async time!",
                         ompi_mtl_portals4.flowctl.epoch_counter));

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
    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "Exiting flowctl_start_recover %d",
                         ompi_mtl_portals4.flowctl.epoch_counter));

    return ret;
}


static int
setup_alarm(uint32_t epoch)
{
    int ret = OMPI_SUCCESS;
    size_t i;

    /* setup trigger */
    if (ompi_mtl_portals4.flowctl.i_am_root) {
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
                              (epoch * ompi_mtl_portals4.flowctl.num_procs) + 1);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto cleanup;
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
                              ompi_mtl_portals4.flowctl.alert_ct_h,
                              epoch + 1);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto cleanup;
        }
    }
    
 cleanup:
    return ret;
}


static int
flowctl_alert_callback(ptl_event_t *ev,
                       ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    int ret = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "-----> flowctl_alert_callback <-----"));

    ret = start_recover();

    return ret;
}


static int
flowctl_fanin_callback(ptl_event_t *ev,
                       ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    int ret = OMPI_SUCCESS;

    ompi_mtl_portals4.flowctl.fanin_count++;

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "flowctl_fanin_callback: %d (%d of %d)", 
                         ompi_mtl_portals4.flowctl.epoch_counter,
                         (int) ompi_mtl_portals4.flowctl.fanin_count,
                         (int) ompi_mtl_portals4.flowctl.num_children + 1));

    if (ompi_mtl_portals4.flowctl.fanin_count == 
        ompi_mtl_portals4.flowctl.num_children + 1) {

        if (ompi_mtl_portals4.flowctl.i_am_root) {
            /* drain event queue */
            while (0 != ompi_mtl_portals4_progress()) { ; }

            ompi_mtl_portals4.flowctl.send_alert = true;

            ret = PtlPTEnable(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.recv_idx);
            if (PTL_OK != ret) abort();

            ret = PtlPut(ompi_mtl_portals4.zero_md_h,
                         0,
                         0,
                         PTL_NO_ACK_REQ,
                         ompi_mtl_portals4.flowctl.me,
                         ompi_mtl_portals4.flowctl_idx,
                         MTL_PORTALS4_FLOWCTL_FANOUT,
                         0,
                         NULL,
                         0);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_mtl_base_output,
                                    "%s:%d: PtlTriggeredPut failed: %d\n",
                                    __FILE__, __LINE__, ret);
                goto cleanup;
            }
        } else {
            ret = PtlPut(ompi_mtl_portals4.zero_md_h,
                         0,
                         0,
                         PTL_NO_ACK_REQ,
                         ompi_mtl_portals4.flowctl.parent,
                         ompi_mtl_portals4.flowctl_idx,
                         MTL_PORTALS4_FLOWCTL_FANIN,
                         0,
                         NULL,
                         0);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_mtl_base_output,
                                    "%s:%d: PtlTriggeredPut failed: %d\n",
                                    __FILE__, __LINE__, ret);
                goto cleanup;
            }
        }

        ompi_mtl_portals4.flowctl.fanin_count = 0;
    }

 cleanup:
    return ret;
}


static int
flowctl_fanout_callback(ptl_event_t *ev,
                        ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    int ret;
    int tmp = ompi_mtl_portals4.flowctl.epoch_counter;
    size_t i;
    ptl_ct_event_t ct;

    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "Enter flowctl_fanout_callback: %d", tmp));

    /* setup the alert broadcast tree */
    for (i = 0 ; i < ompi_mtl_portals4.flowctl.num_children ; ++i) {
        ret = PtlPut(ompi_mtl_portals4.zero_md_h,
                     0,
                     0,
                     PTL_NO_ACK_REQ,
                     ompi_mtl_portals4.flowctl.children[i],
                     ompi_mtl_portals4.flowctl_idx,
                     MTL_PORTALS4_FLOWCTL_FANOUT,
                     0,
                     NULL,
                     0);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }
    }

    /* woo, we're recovered! */
    ompi_mtl_portals4.flowctl.epoch_counter++;
    ompi_mtl_portals4.flowctl.flowctl_active = false;
    if (ompi_mtl_portals4.flowctl.i_am_root) {
        ct.success = ompi_mtl_portals4.flowctl.epoch_counter *
                       ompi_mtl_portals4.flowctl.num_procs;
        ct.failure = 0;
        ret = PtlCTSet(ompi_mtl_portals4.flowctl.trigger_ct_h, ct);
        if (PTL_OK != ret) abort();
    }
    ret = PtlPTEnable(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.recv_idx);
    if (PTL_OK != ret) abort();
    ompi_mtl_portals4_pending_list_progress();
    OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_output,
                         "Exit flowctl_fanout_callback: %d", tmp));

    return OMPI_SUCCESS;
}
