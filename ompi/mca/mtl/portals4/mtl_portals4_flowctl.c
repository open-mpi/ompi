/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012      Sandia National Laboratories.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/proc/proc.h"

#include "mtl_portals4.h"
#include "mtl_portals4_flowctl.h"
#include "mtl_portals4_recv_short.h"

OBJ_CLASS_INSTANCE(ompi_mtl_portals4_pending_request_t, opal_free_list_item_t,
                   NULL, NULL);

static int flowctl_alert_callback(ptl_event_t *ev,
                                  ompi_mtl_portals4_base_request_t *ptl_base_request);
static int flowctl_fanout_callback(ptl_event_t *ev,
                                   ompi_mtl_portals4_base_request_t *ptl_base_request);

static int start_recover(void);
static int setup_alarm(uint32_t epoch);
static int setup_barrier(uint32_t epoch);

int
ompi_mtl_portals4_flowctl_init(void)
{
    ptl_me_t me;
    int ret;

    ompi_mtl_portals4.flowctl.flowctl_active = false;

    OBJ_CONSTRUCT(&ompi_mtl_portals4.flowctl.pending_sends, opal_list_t);

    OBJ_CONSTRUCT(&ompi_mtl_portals4.flowctl.pending_fl, opal_free_list_t);
    opal_free_list_init(&ompi_mtl_portals4.flowctl.pending_fl,
                        sizeof(ompi_mtl_portals4_pending_request_t),
                        opal_cache_line_size,
                        OBJ_CLASS(ompi_mtl_portals4_pending_request_t),
                        0, 0, 1, -1, 1, NULL, 0, NULL, NULL, NULL);

    ompi_mtl_portals4.flowctl.max_send_slots = (ompi_mtl_portals4.send_queue_size - 3) / 3;
    ompi_mtl_portals4.flowctl.send_slots = ompi_mtl_portals4.flowctl.max_send_slots;

    ompi_mtl_portals4.flowctl.alert_req.type = portals4_req_flowctl;
    ompi_mtl_portals4.flowctl.alert_req.event_callback = flowctl_alert_callback;

    ompi_mtl_portals4.flowctl.fanout_req.type = portals4_req_flowctl;
    ompi_mtl_portals4.flowctl.fanout_req.event_callback = flowctl_fanout_callback;

    ompi_mtl_portals4.flowctl.epoch_counter = -1;

    ret = PtlPTAlloc(ompi_mtl_portals4.ni_h,
                     PTL_PT_ONLY_TRUNCATE,
                     ompi_mtl_portals4.send_eq_h,
                     REQ_FLOWCTL_TABLE_ID,
                     &ompi_mtl_portals4.flowctl_idx);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.trigger_ct_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* everyone creates the trigger ME, even if the root may be the
       only to use it */
    me.start = NULL;
    me.length = 0;
    me.min_free = 0;
    me.uid = ompi_mtl_portals4.uid;
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
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }


    /* Alert CT/ME for broadcasting out alert when root receives a
       trigger */
    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.alert_ct_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
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
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Fanin CT/ME for receiving fan-in for restart */
    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.fanin_ct_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlCTAlloc failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }
    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_COMM_DISABLE |
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE |
        PTL_ME_EVENT_CT_COMM;
    me.ct_handle = ompi_mtl_portals4.flowctl.fanin_ct_h;
    me.match_bits = MTL_PORTALS4_FLOWCTL_FANIN;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &ompi_mtl_portals4.flowctl.fanin_me_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* Fan-out CT/ME for sending restart messages after fan-in */
    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h,
                     &ompi_mtl_portals4.flowctl.fanout_ct_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
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
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    ompi_mtl_portals4.flowctl.num_children = 0;

    gettimeofday(&ompi_mtl_portals4.flowctl.tv, NULL);
    ompi_mtl_portals4.flowctl.backoff_count = 0;

    ret = OMPI_SUCCESS;

 error:
    return ret;
}


int
ompi_mtl_portals4_flowctl_fini(void)
{
    PtlPTFree(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.flowctl_idx); 
    PtlCTFree(ompi_mtl_portals4.flowctl.trigger_ct_h); 
    PtlMEUnlink(ompi_mtl_portals4.flowctl.trigger_me_h); 
    PtlCTFree(ompi_mtl_portals4.flowctl.alert_ct_h); 
    PtlMEUnlink(ompi_mtl_portals4.flowctl.alert_me_h); 
    PtlCTFree(ompi_mtl_portals4.flowctl.fanin_ct_h); 
    PtlMEUnlink(ompi_mtl_portals4.flowctl.fanin_me_h); 
    PtlCTFree(ompi_mtl_portals4.flowctl.fanout_ct_h); 
    PtlMEUnlink(ompi_mtl_portals4.flowctl.fanout_me_h); 

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_flowctl_add_procs(size_t me,
                                    size_t npeers,
                                    struct ompi_proc_t **procs)
{
    int i;

    /* if epoch isn't 0, that means setup trees has been called, which
       means that this add_procs is a dynamic process, which we don't
       support */
    if (ompi_mtl_portals4.flowctl.epoch_counter != -1) {
        return OMPI_ERR_NOT_SUPPORTED;
    }
    ompi_mtl_portals4.flowctl.epoch_counter = 0;

    ompi_mtl_portals4.flowctl.num_procs = npeers;
    ompi_mtl_portals4.flowctl.root =
        *((ptl_process_t*) procs[0]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
    if (0 == me) {
        ompi_mtl_portals4.flowctl.i_am_root = true;
    } else {
        ompi_mtl_portals4.flowctl.i_am_root = false;
        ompi_mtl_portals4.flowctl.parent = 
            *((ptl_process_t*) procs[(me - 1) / 2]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
    }
    ompi_mtl_portals4.flowctl.me =
        *((ptl_process_t*) procs[me]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);

    for (i = 0 ; i < 2 ; ++i) {
        size_t tmp = (2 * me) + i + 1;
        if (tmp < npeers) {
            ompi_mtl_portals4.flowctl.num_children++;
            ompi_mtl_portals4.flowctl.children[i] = 
                *((ptl_process_t*) procs[tmp]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PORTALS4]);
        }
    }

    return setup_alarm(ompi_mtl_portals4.flowctl.epoch_counter);
}


int
ompi_mtl_portals4_flowctl_trigger(void)
{
    int ret;

    if (false == ompi_mtl_portals4.flowctl.flowctl_active) {
        ompi_mtl_portals4.flowctl.flowctl_active = true;

        /* send trigger to root */
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
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }
    }

    return OMPI_SUCCESS;
}


static int
seqnum_compare(opal_list_item_t **ap, opal_list_item_t **bp)
{
    ompi_mtl_portals4_pending_request_t *a = 
        (ompi_mtl_portals4_pending_request_t*) *ap;
    ompi_mtl_portals4_pending_request_t *b = 
        (ompi_mtl_portals4_pending_request_t*) *bp;

    if (a->ptl_request->opcount > b->ptl_request->opcount) {
        return 1;
    } else if (a->ptl_request->opcount == b->ptl_request->opcount) {
        return 0;
    } else {
        return -1;
    }
}

static int
start_recover(void)
{
    int ret;
    int64_t epoch_counter;

    ompi_mtl_portals4.flowctl.flowctl_active = true;
    epoch_counter = opal_atomic_add_64(&ompi_mtl_portals4.flowctl.epoch_counter, 1);

    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "Entering flowctl_start_recover %ld",
                        epoch_counter);

    /* re-arm trigger/alarm for next time */
    ret = setup_alarm(epoch_counter);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d setup_alarm failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    /* setup barrier tree for getting us out of flow control */
    ret = setup_barrier(epoch_counter);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d setup_barrier failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    /* drain all pending sends */
    while (ompi_mtl_portals4.flowctl.send_slots != 
           ompi_mtl_portals4.flowctl.max_send_slots) {
        opal_progress();
    }

    /* drain event queue */
    while (0 != ompi_mtl_portals4_progress()) { ; }

    /* check short block active count */
    ret = ompi_mtl_portals4_recv_short_link(1);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: recv_short_link failed: %d",
                            __FILE__, __LINE__, ret);
    }

    /* reorder the pending sends by operation count */
    ret = opal_list_sort(&ompi_mtl_portals4.flowctl.pending_sends, seqnum_compare);
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d opal_list_sort failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    /* drain event queue again, just to make sure */
    while (0 != ompi_mtl_portals4_progress()) { ; }

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
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPut failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* recovery complete when fan-out event arrives, async event, so
       we're done now */
    ret = OMPI_SUCCESS;

 error:
    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "Exiting flowctl_start_recover %ld",
                         epoch_counter));

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
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
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
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto cleanup;
        }
    }
    
 cleanup:
    return ret;
}


static int
setup_barrier(uint32_t epoch)
{
    int ret = OMPI_SUCCESS;
    size_t i;
    ptl_ct_event_t ct;

    if (ompi_mtl_portals4.flowctl.i_am_root) {
        ct.success = ompi_mtl_portals4.flowctl.epoch_counter *
            ompi_mtl_portals4.flowctl.num_procs;
        ct.failure = 0;
        ret = PtlTriggeredCTSet(ompi_mtl_portals4.flowctl.trigger_ct_h, 
                                ct,
                                ompi_mtl_portals4.flowctl.fanin_ct_h,
                                epoch * (ompi_mtl_portals4.flowctl.num_children + 1));
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlTriggeredCTSet failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto cleanup;
        }

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
                              epoch * (ompi_mtl_portals4.flowctl.num_children + 1));
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto cleanup;
        }
    } else {
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
                              epoch * (ompi_mtl_portals4.flowctl.num_children + 1));
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto cleanup;
        }
    }

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
                              epoch);
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlTriggeredPut failed: %d\n",
                                __FILE__, __LINE__, ret);
            return ret;
        }
    }

 cleanup:
    return ret;
}


static int
flowctl_alert_callback(ptl_event_t *ev,
                       ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    return start_recover();
}


static int
flowctl_fanout_callback(ptl_event_t *ev,
                        ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    int ret;
    struct timeval tv;

    ompi_mtl_portals4.flowctl.flowctl_active = false;
    ret = PtlPTEnable(ompi_mtl_portals4.ni_h, ompi_mtl_portals4.recv_idx);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPTEnabled failed: %d\n",
                            __FILE__, __LINE__, ret);
        return ret;
    }

    gettimeofday(&tv, NULL);
    if (((tv.tv_sec * 1000000 + tv.tv_usec) - 
         (ompi_mtl_portals4.flowctl.tv.tv_sec * 1000000 + ompi_mtl_portals4.flowctl.tv.tv_usec)) 
        < 1000000 * ompi_mtl_portals4.flowctl.backoff_count) {
        usleep(++ompi_mtl_portals4.flowctl.backoff_count);
    } else {
        ompi_mtl_portals4.flowctl.backoff_count = 0;
    }
    ompi_mtl_portals4.flowctl.tv = tv;
         
    ompi_mtl_portals4_pending_list_progress();

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "Exiting flowctl_fanout_callback %ld",
                         ompi_mtl_portals4.flowctl.epoch_counter));

    return OMPI_SUCCESS;
}
