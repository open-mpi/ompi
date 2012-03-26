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

int
ompi_mtl_portals4_flowctl_init(void)
{
    ptl_me_t me;
    int ret;

    ompi_mtl_portals4.flowctl.epoch_counter = 0;

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
    me.ct_handle = ompi_mtl_portals4.flowctl.trigger_ct_h;
    me.min_free = 0;
    me.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | 
        PTL_ME_ACK_DISABLE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE;
    me.match_id.phys.nid = PTL_NID_ANY;
    me.match_id.phys.pid = PTL_PID_ANY;
    me.match_bits = MTL_PORTALS4_FLOWCTL_TRIGGER;
    me.ignore_bits = 0;
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

    me.ct_handle = PTL_CT_NONE;
    me.match_bits = MTL_PORTALS4_FLOWCTL_ALERT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_event_idx,
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

    me.ct_handle = PTL_CT_NONE;
    me.match_bits = MTL_PORTALS4_FLOWCTL_FANOUT;
    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.flowctl_event_idx,
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
    if (ompi_mtl_portals4.flowctl.epoch_counter != 0) {
        return OMPI_ERR_NOT_SUPPORTED;
    }

    ompi_mtl_portals4.flowctl.num_procs = npeers;
    if (0 != me) {
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
                          ompi_mtl_portals4.flowctl_event_idx,
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
                              ompi_mtl_portals4.flowctl_event_idx,
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
                              ompi_mtl_portals4.flowctl_event_idx,
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

    ret = ompi_mtl_portals4_flowctl_setup_comm();
    if (OMPI_SUCCESS != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: flowctl_setup_comm failed: %d\n",
                            __FILE__, __LINE__, ret);
        goto error;
    }

    /* drain all pending sends */
    while (0 != opal_list_get_size(&ompi_mtl_portals4.active_sends)) {
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
