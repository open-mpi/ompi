/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *               2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"
#include "opal/align.h"
#include "opal/util/sys_limits.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"

#include "oob_ud_component.h"

#include "opal/mca/common/verbs/common_verbs.h"

static int   mca_oob_ud_component_open (void);
static int   mca_oob_ud_component_close (void);
static int   mca_oob_ud_component_register (void);
static int   mca_oob_ud_component_available(void);
static int   mca_oob_ud_component_startup(void);
static int   mca_oob_ud_component_send_nb(orte_rml_send_t *msg);
static void  mca_oob_ud_component_shutdown(void);
static char* mca_oob_ud_component_get_addr(void);
static int   mca_oob_ud_component_set_addr(orte_process_name_t *peer, char **uris);
static bool  mca_oob_ud_component_is_reachable(orte_process_name_t *peer);
#if OPAL_ENABLE_FT_CR == 1
static int   mca_oob_ud_component_ft_event(int state);
#endif // OPAL_ENABLE_FT_CR

static int mca_oob_ud_listen_create (mca_oob_ud_port_t *port);
static int mca_oob_ud_listen_destroy (mca_oob_ud_port_t *port);
static int mca_oob_ud_port_alloc_buffers (mca_oob_ud_port_t *port);
static inline int mca_oob_ud_port_recv_start (mca_oob_ud_port_t *port);
static inline int mca_oob_ud_alloc_reg_mem (struct ibv_pd *pd, mca_oob_ud_reg_mem_t *reg_mem,
                                            const int buffer_len);
static inline void mca_oob_ud_free_reg_mem (mca_oob_ud_reg_mem_t *reg_mem);
static void mca_oob_ud_cancel_all_in_list (opal_list_t *list);
static void mca_oob_ud_empty_list (opal_list_t *list);
static void mca_oob_ud_port_construct (mca_oob_ud_port_t *port);
static void mca_oob_ud_port_destruct (mca_oob_ud_port_t *port);
static void mca_oob_ud_device_construct (mca_oob_ud_device_t *device);
static void mca_oob_ud_device_destruct (mca_oob_ud_device_t *device);

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_oob_ud_component_t mca_oob_ud_component = {
    {
        .oob_base = {
            MCA_OOB_BASE_VERSION_2_0_0,
            .mca_component_name = "ud",
            MCA_BASE_MAKE_VERSION(component, ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION,
                                  ORTE_RELEASE_VERSION),
            .mca_open_component = mca_oob_ud_component_open,
            .mca_close_component = mca_oob_ud_component_close,
            .mca_register_component_params = mca_oob_ud_component_register,
        },
        .oob_data = {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        .priority = 0,  //set the priority so that we will select this component only if someone directs to do so
        .available = mca_oob_ud_component_available, //available
        .startup = mca_oob_ud_component_startup, //startup
        .shutdown = mca_oob_ud_component_shutdown, //shutdown
        .send_nb = mca_oob_ud_component_send_nb, //send_nb
        .get_addr = mca_oob_ud_component_get_addr,
        .set_addr = mca_oob_ud_component_set_addr,
        .is_reachable = mca_oob_ud_component_is_reachable, //is_reachable
#if OPAL_ENABLE_FT_CR == 1
        .ft_event = mca_oob_ud_component_ft_event,
#endif // OPAL_ENABLE_FT_CR
    },
};

static int mca_oob_ud_component_open (void)
{
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_devices, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_active_sends, opal_list_t);

    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_active_recvs, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_event_queued_reqs, opal_list_t);

    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_event_processing_msgs, opal_list_t);

    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_match_lock, opal_mutex_t);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_component_close (void)
{
    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                        "%s oob:ud:component_close entering",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_devices);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_active_sends);

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_active_recvs);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_event_queued_reqs);

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_lock);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_match_lock);

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_event_processing_msgs);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_component_register (void)
{
    mca_base_component_t *component = &mca_oob_ud_component.super.oob_base;

    mca_oob_ud_component.ud_min_qp = 8;

    (void) mca_base_component_var_register (component, "min_qp", "Minimum number of UD queue pairs "
                                            "to allocate (default: 8)", MCA_BASE_VAR_TYPE_INT, NULL,
                                            0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_LOCAL, &mca_oob_ud_component.ud_min_qp);

    mca_oob_ud_component.ud_max_qp = 32;
    (void) mca_base_component_var_register (component, "max_qp", "Maximum number of UD queue pairs "
                                            "to allocate (default: 32)", MCA_BASE_VAR_TYPE_INT, NULL,
                                            0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_LOCAL, &mca_oob_ud_component.ud_max_qp);

    mca_oob_ud_component.ud_recv_buffer_count = 512;
    (void) mca_base_component_var_register (component, "recv_buffers", "Number of MTU sized recv "
                                            "buffers to post (default: 512)", MCA_BASE_VAR_TYPE_INT, NULL,
                                            0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_LOCAL, &mca_oob_ud_component.ud_recv_buffer_count);

    mca_oob_ud_component.ud_send_buffer_count = 512;
    (void) mca_base_component_var_register (component, "send_buffers", "Number of MTU sized send "
                                            "buffers to allocate (default: 512)", MCA_BASE_VAR_TYPE_INT, NULL,
                                            0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_5,
                                            MCA_BASE_VAR_SCOPE_LOCAL, &mca_oob_ud_component.ud_send_buffer_count);

    mca_oob_ud_component.ud_max_retries = 5;
    (void)mca_base_component_var_register(component, "peer_retries",
                                          "Number of times to try shutting down a connection before giving up",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_ud_component.ud_max_retries);

    mca_oob_ud_component.ud_timeout_usec = 800000;
    (void)mca_base_component_var_register(component, "peer_timeout",
                                          "Timeout in microseconds between retransmission of data",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_ud_component.ud_timeout_usec);


    mca_oob_ud_component.ud_qp_max_send_sge = 1;
    (void)mca_base_component_var_register(component, "max_send_sge",
                                          "Requested max number of outstanding WRs in the SQ",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_ud_component.ud_qp_max_send_sge);

    mca_oob_ud_component.ud_qp_max_recv_sge = 2;
    (void)mca_base_component_var_register(component, "max_recv_sge",
                                          "Requested max number of outstanding WRs in the RQ",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_ud_component.ud_qp_max_recv_sge);


    mca_oob_ud_component.ud_qp_max_send_wr = 4096;
    (void)mca_base_component_var_register(component, "max_send_wr",
                                          "Requested max number of scatter/gather (s/g) elements in a WR in the SQ",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_ud_component.ud_qp_max_send_wr);

    mca_oob_ud_component.ud_qp_max_recv_wr = 4096;
    (void)mca_base_component_var_register(component, "max_recv_wr",
                                          "Requested max number of scatter/gather (s/g) elements in a WR in the RQ",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_ud_component.ud_qp_max_recv_wr);

    mca_oob_ud_component.ud_qp_max_inline_data = 0;
    (void)mca_base_component_var_register(component, "max_inline_data",
                                          "Requested max number of data (bytes) that can be posted inline to the SQ",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_LOCAL,
                                          &mca_oob_ud_component.ud_qp_max_inline_data);
    return ORTE_SUCCESS;
}

static int  mca_oob_ud_component_available(void) {

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                    "oob:ud: component_available called");

    /* set the module event base - this is where we would spin off a separate
     * progress thread if so desired */
    mca_oob_ud_module.ev_base = orte_event_base;

    return ORTE_SUCCESS;
}

static int port_mtus[] = {0, 256, 512, 1024, 2048, 4096};

static inline int mca_oob_ud_port_setup (mca_oob_ud_port_t *port)
{
    int rc;
    struct ibv_port_attr port_attr;

    rc = ibv_query_port (port->device->ib_context, port->port_num, &port_attr);
    if (0 != rc || IBV_PORT_ACTIVE != port_attr.state || 0 == port_attr.lid) {
        /* skip this port */
        return ORTE_ERROR;
    }

    port->lid = port_attr.lid;
    port->mtu = port_attr.active_mtu > IBV_MTU_4096 ? 2048 : port_mtus[port_attr.active_mtu];

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:port_setup found port: num = %u, lid = %u, mtu = %u",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         port->port_num, port->lid, port->mtu);

    return rc;
}

static inline int mca_oob_ud_device_setup (mca_oob_ud_device_t *device,
                                           struct ibv_device *ib_device)
{
    int rc, port_num;

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:device_setup attempting to setup ib device %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) ib_device);


    device->ib_context = ibv_open_device (ib_device);
    if (NULL == device->ib_context) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                             "%s oob:ud:device_setup error opening device. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    rc = ibv_query_device (device->ib_context, &device->attr);
    if (0 != rc) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                             "%s oob:ud:device_setup error querying device. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    device->ib_channel = ibv_create_comp_channel (device->ib_context);
    if (NULL == device->ib_channel) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                             "%s oob:ud:device_setup error completing completion channel."
                             "errno = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    device->ib_pd = ibv_alloc_pd (device->ib_context);
    if (NULL == device->ib_pd) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                             "%s oob:ud:device_setup error allocating protection domain."
                             "errno = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    for (port_num = 1 ; port_num <= device->attr.phys_port_cnt ; ++port_num) {
        mca_oob_ud_port_t *port = OBJ_NEW(mca_oob_ud_port_t);

        if (NULL == port) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        port->device = device;
        port->port_num = port_num;

        rc = mca_oob_ud_port_setup (port);
        if (ORTE_SUCCESS != rc) {
            OBJ_RELEASE(port);
            continue;
        }

        opal_list_append (&device->ports, (opal_list_item_t *) port);

	break;
    }

    if (0 == opal_list_get_size(&device->ports)) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                             "%s oob:ud:device_setup could not init device. no usable "
                             "ports present", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static int mca_oob_ud_component_startup(void)
{
    struct ibv_device **devices;
    int num_devices, i, rc;
    opal_list_item_t *item, *item2;
    bool found_one = false;

    /* If fork support is requested, try to enable it */
    rc = opal_common_verbs_fork_test();
    if (OPAL_SUCCESS != rc) {
        opal_output_verbose(5, orte_oob_base_framework.framework_output,
                            "%s oob:ud:device_setup failed in ibv_fork_init. errno = %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno);
        return ORTE_ERROR;
    }

    /* If there are no devices, it is not an error; we just won't use
       this component. */
    devices = ibv_get_device_list (&num_devices);
    if (NULL == devices) {
        return ORTE_ERR_NOT_FOUND;
    }
    if (0 == num_devices) {
        ibv_free_device_list(devices);
        return ORTE_ERR_NOT_FOUND;
    }

    for (i = 0 ; i < num_devices ; ++i) {
        mca_oob_ud_device_t *device = OBJ_NEW(mca_oob_ud_device_t);

        if (NULL == device) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERROR;
        }

        rc = mca_oob_ud_device_setup (device, devices[i]);
        if (ORTE_SUCCESS != rc) {
            OBJ_RELEASE(device);
            continue;
        }

        opal_list_append (&mca_oob_ud_component.ud_devices,
                          (opal_list_item_t *) device);

        /* NTH: support only 1 device for now */
        break;
    }

    ibv_free_device_list (devices);

    /* If no usable devices are found, then just ignore this component
       in this run */
    if (0 == opal_list_get_size (&mca_oob_ud_component.ud_devices)) {
        return ORTE_ERR_NOT_FOUND;
    }

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                        "%s oob:ud:init initializing oob/openib. # of devices = %u",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (unsigned int) opal_list_get_size (&mca_oob_ud_component.ud_devices));

    for (item = opal_list_get_first (&mca_oob_ud_component.ud_devices);
         item != opal_list_get_end (&mca_oob_ud_component.ud_devices);
         item = opal_list_get_next (item)) {
        mca_oob_ud_device_t *device = (mca_oob_ud_device_t *) item;

        /* start monitoring the device for completions */
        for (item2 = opal_list_get_first (&device->ports) ;
             item2 != opal_list_get_end (&device->ports) ;
             item2 = opal_list_get_next (item2)) {
            mca_oob_ud_port_t *port = (mca_oob_ud_port_t *) item2;

            rc = mca_oob_ud_listen_create (port);
            if (0 != rc) {
                continue;
            }

            rc = mca_oob_ud_port_alloc_buffers (port);
            if (ORTE_SUCCESS != rc) {
                mca_oob_ud_listen_destroy (port);
                continue;
            }

            rc = opal_free_list_init (&port->data_qps,
                                      sizeof (mca_oob_ud_qp_t), 8,
                                      OBJ_CLASS(mca_oob_ud_qp_t), 0, 0,
                                      mca_oob_ud_component.ud_min_qp,
                                      mca_oob_ud_component.ud_max_qp,
                                      2, NULL, 0, NULL, NULL, NULL);
            if (OPAL_SUCCESS != rc) {
                mca_oob_ud_listen_destroy (port);
                continue;
            }

            rc = mca_oob_ud_port_recv_start (port);
            if (ORTE_SUCCESS != rc) {
                mca_oob_ud_listen_destroy (port);
                continue;
            }

            /* NTH: only supports one port for now */
            found_one = true;

            /* NTH: since we only support one port start monitoring now */
            mca_oob_ud_event_start_monitor (device);

            break;
        }
    }

    if (!found_one) {
        orte_show_help("help-oob-ud.txt", "no-ports-usable", true,
                       orte_process_info.nodename);
        return ORTE_ERR_NOT_FOUND;
    }

    /* have to call the module init here so we can test for available qpair */
    if ((NULL != mca_oob_ud_module.api.init) && (ORTE_SUCCESS != (rc = mca_oob_ud_module.api.init()))){
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static void  mca_oob_ud_component_shutdown(void)
{
    mca_oob_ud_peer_t *peer;
    opal_list_item_t *item;

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:fini entering",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_lock);
    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_match_lock);

    if (ORTE_VPID_INVALID != ORTE_PROC_MY_PARENT->vpid) {
        if (ORTE_SUCCESS == mca_oob_ud_peer_lookup (ORTE_PROC_MY_PARENT, &peer) && NULL != peer) {
            mca_oob_ud_peer_handle_end (peer);
        }
    }

    /* abort active receives */
    mca_oob_ud_cancel_all_in_list (&mca_oob_ud_component.ud_active_recvs);
    mca_oob_ud_cancel_all_in_list (&mca_oob_ud_component.ud_active_sends);

    mca_oob_ud_empty_list (&mca_oob_ud_component.ud_event_queued_reqs);

    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_match_lock);

    if (NULL != mca_oob_ud_module.api.finalize) {
        mca_oob_ud_module.api.finalize(&peer);
    }

    for (item = opal_list_get_first (&mca_oob_ud_component.ud_devices);
         item != opal_list_get_end (&mca_oob_ud_component.ud_devices);
         item = opal_list_get_next (item)) {
        mca_oob_ud_event_stop_monitor ((mca_oob_ud_device_t *) item);
    }

    mca_oob_ud_empty_list (&mca_oob_ud_component.ud_devices);
    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);
}

static char* mca_oob_ud_component_get_addr(void) {
    /* NTH: qp_num - 32 bits (10), lid - 16 bits (5), port - 8 bits (3) + ud:// + 3 .'s + \0 = 27 chars */
    char *contact_info = (char *) calloc(opal_list_get_size(&mca_oob_ud_component.ud_devices) * 27, 1);
    char *ptr = contact_info;
    opal_list_item_t *item, *port_item;
    *ptr = 0;

    for (item = opal_list_get_first (&mca_oob_ud_component.ud_devices) ;
         item != opal_list_get_end (&mca_oob_ud_component.ud_devices) ;
         item = opal_list_get_next (item)) {

        mca_oob_ud_device_t *device = (mca_oob_ud_device_t *) item;

        for (port_item = opal_list_get_first (&device->ports);
             port_item != opal_list_get_end (&device->ports);
             port_item = opal_list_get_next (port_item)) {

            if (ptr != contact_info) {
                ptr += sprintf (ptr, ";");
            }

            mca_oob_ud_port_get_uri ((mca_oob_ud_port_t *) port_item, ptr);
            ptr += strlen (ptr);
        }
    }

    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:get_addr contact information: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), contact_info);

    return contact_info;
}

static int mca_oob_ud_component_send_nb(orte_rml_send_t *msg) {
    if (NULL != mca_oob_ud_module.api.send_nb) {
        mca_oob_ud_module.api.send_nb(msg);
        return ORTE_SUCCESS;
    }
    return ORTE_ERROR;
}

static int mca_oob_ud_component_set_addr(orte_process_name_t *peer, char **uris)
{
    int rc;

    OPAL_THREAD_LOCK(&mca_oob_ud_component.ud_lock);

    for (int i = 0; NULL != uris[i]; i++) {
        if (0 == strncmp(uris[i], "ud:", 3)) {
            if (NULL != mca_oob_ud_module.api.set_addr) {
                if (ORTE_SUCCESS != (rc = mca_oob_ud_module.api.set_addr(peer, uris[i]))) {
                    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);
                    return rc;
                }
            }
        }
    }

    OPAL_THREAD_UNLOCK(&mca_oob_ud_component.ud_lock);

    return ORTE_SUCCESS;
}

#if OPAL_ENABLE_FT_CR == 1
static int   mca_oob_ud_component_ft_event(int state) {
    (void) state;
    return ORTE_SUCCESS;
}
#endif // OPAL_ENABLE_FT_CR

static int mca_oob_ud_port_alloc_buffers (mca_oob_ud_port_t *port) {
    int total_buffer_count = mca_oob_ud_component.ud_recv_buffer_count +
        mca_oob_ud_component.ud_send_buffer_count;
    int rc;

    rc = mca_oob_ud_alloc_reg_mem (port->device->ib_pd, &port->grh_buf,
                                   mca_oob_ud_component.ud_recv_buffer_count * sizeof (struct ibv_grh));
    if (ORTE_SUCCESS != rc) {
        return rc;
    }


    rc = mca_oob_ud_alloc_reg_mem (port->device->ib_pd, &port->msg_buf,
                                   total_buffer_count * port->mtu);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    port->send_buffer_index = 0;
    rc = opal_free_list_init (&port->free_msgs, sizeof (mca_oob_ud_msg_t), 8,
                              OBJ_CLASS(mca_oob_ud_msg_t), 0, 0, mca_oob_ud_component.ud_send_buffer_count,
                              mca_oob_ud_component.ud_send_buffer_count, 0, NULL, 0, NULL, mca_oob_ud_msg_init,
                              port);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }

    return rc;
}

static bool mca_oob_ud_component_is_reachable(orte_process_name_t *peer_name)
{
    orte_process_name_t hop;

    /* if we have a route to this peer, then we can reach it */
    hop = orte_routed.get_route(peer_name);
    if (ORTE_JOBID_INVALID == hop.jobid ||
        ORTE_VPID_INVALID == hop.vpid) {
        ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
        return false;
    }
    return true;
}

static void mca_oob_ud_port_construct (mca_oob_ud_port_t *port)
{
    memset((char *) port + sizeof (port->super), 0, sizeof (*port) - sizeof (port->super));

    OBJ_CONSTRUCT(&port->data_qps, opal_free_list_t);
    OBJ_CONSTRUCT(&port->free_msgs, opal_free_list_t);
    OBJ_CONSTRUCT(&port->listen_qp, opal_free_list_item_t);
}

static void mca_oob_ud_port_destruct (mca_oob_ud_port_t *port)
{
    (void) mca_oob_ud_listen_destroy (port);
    OBJ_DESTRUCT(&port->data_qps);
    OBJ_DESTRUCT(&port->free_msgs);

    mca_oob_ud_free_reg_mem (&port->grh_buf);
    mca_oob_ud_free_reg_mem (&port->msg_buf);
}

OBJ_CLASS_INSTANCE(mca_oob_ud_port_t, opal_list_item_t,
                   mca_oob_ud_port_construct,
                   mca_oob_ud_port_destruct);

static int mca_oob_ud_listen_create (mca_oob_ud_port_t *port) {
    return mca_oob_ud_qp_init (&port->listen_qp, port, port->device->ib_channel, NULL, false);
}

/* mca_oob_ud_listen_destroy:
 *
 * Destory the listen queue pair associated with a port.
 */
static int mca_oob_ud_listen_destroy (mca_oob_ud_port_t *port)
{
    if (NULL == port || NULL == port->listen_qp.ib_qp) {
        return ORTE_SUCCESS;
    }

    OBJ_DESTRUCT(&port->listen_qp);

    return ORTE_SUCCESS;
}

static inline int mca_oob_ud_port_recv_start (mca_oob_ud_port_t *port)
{
    int i, rc;

    rc = mca_oob_ud_qp_to_rts (&port->listen_qp);
    if (ORTE_SUCCESS != rc) {
        return rc;
    }
    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                         "%s oob:ud:port_recv_start posting "
                         "%d message buffers", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         mca_oob_ud_component.ud_recv_buffer_count);

    for (i = 0 ; i < mca_oob_ud_component.ud_recv_buffer_count ; ++i) {
        rc = mca_oob_ud_port_post_one_recv (port, i);
        if (ORTE_SUCCESS != rc) {
            return rc;
        }
    }

    rc = ibv_req_notify_cq (port->listen_qp.ib_recv_cq, 0);
    if (0 != rc) {
        orte_show_help("help-oob-ud.txt", "notify-cq-failed", true,
                       orte_process_info.nodename, strerror(errno));
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static inline int mca_oob_ud_alloc_reg_mem (struct ibv_pd *pd, mca_oob_ud_reg_mem_t *reg_mem,
                                            const int buffer_len)
{
    size_t buffer_len_aligned, page_size;
    reg_mem->len = buffer_len;
    reg_mem->ptr = NULL;
    reg_mem->mr  = NULL;
    /* The allocated buffer should be a multiple of page size.
       If ibv_fork_init() has been invoked the pages are marked MADV_DONTFORK.
       If we only partially use a page, any data allocated on the remainder of
       the page will be inaccessible to the child process */
    page_size = opal_getpagesize();
    buffer_len_aligned = OPAL_ALIGN(buffer_len, page_size, size_t);
    opal_output_verbose(5, orte_oob_base_framework.framework_output,
                          "%s oob:ud:alloc_reg_mem allocing and registering %d bytes of memory with pd %p",
                          ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), buffer_len, (void *) pd);

    posix_memalign ((void **)&reg_mem->ptr, page_size, buffer_len_aligned);
    if (NULL == reg_mem->ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    memset (reg_mem->ptr, 0, buffer_len);

    reg_mem->mr = ibv_reg_mr (pd, reg_mem->ptr, buffer_len, IBV_ACCESS_LOCAL_WRITE);
    if (NULL == reg_mem->mr) {
        orte_show_help("help-oob-ud.txt", "reg-mr-failed", true,
                       orte_process_info.nodename, reg_mem->ptr, buffer_len, strerror(errno));
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static inline void mca_oob_ud_free_reg_mem (mca_oob_ud_reg_mem_t *reg_mem)
{
    if (reg_mem->mr) {
        (void) ibv_dereg_mr (reg_mem->mr);
    }

    if (reg_mem->ptr) {
        free (reg_mem->ptr);
    }

    memset (reg_mem, 0, sizeof (mca_oob_ud_reg_mem_t));
}

static void mca_oob_ud_cancel_all_in_list (opal_list_t *list)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (list))) {
        ((mca_oob_ud_req_t *)item)->req_list = NULL;
        mca_oob_ud_req_abort ((mca_oob_ud_req_t *) item);
    }
}

static void mca_oob_ud_empty_list (opal_list_t *list)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (list))) {
        OBJ_RELEASE(item);
    }
}

static void mca_oob_ud_device_construct (mca_oob_ud_device_t *device)
{
    memset((char *) device + sizeof (device->super), 0, sizeof (*device) - sizeof (device->super));

    OBJ_CONSTRUCT(&device->ports, opal_list_t);
}

static void mca_oob_ud_device_destruct (mca_oob_ud_device_t *device)
{
    opal_list_item_t *item;

    while (NULL != (item = opal_list_remove_first (&device->ports))) {
        OBJ_RELEASE(item);
    }

    if (device->ib_pd) {
        (void) ibv_dealloc_pd (device->ib_pd);
    }

    if (device->ib_channel) {
        (void) ibv_destroy_comp_channel (device->ib_channel);
    }

    if (device->ib_context) {
        (void) ibv_close_device (device->ib_context);
    }

    OBJ_DESTRUCT(&device->ports);

    memset (device, 0, sizeof (mca_oob_ud_device_t));
}

OBJ_CLASS_INSTANCE(mca_oob_ud_device_t, opal_list_item_t,
                   mca_oob_ud_device_construct,
                   mca_oob_ud_device_destruct);

OBJ_CLASS_INSTANCE(mca_oob_ud_msg_op_t,
                   opal_object_t,
                   NULL, NULL);

OBJ_CLASS_INSTANCE(mca_oob_ud_ping_t,
                   opal_object_t,
                   NULL, NULL);
