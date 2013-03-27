/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
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

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/proc_info.h"

#include "oob_ud.h"

static int        mca_oob_ud_component_open (void);
static int        mca_oob_ud_component_close (void);
static int        mca_oob_ud_component_register (void);
static mca_oob_t *mca_oob_ud_component_init (int *priority);

mca_oob_ud_component_t mca_oob_ud_component = {
  {
    {
        MCA_OOB_BASE_VERSION_2_0_0,
        "ud", /* MCA module name */
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        mca_oob_ud_component_open,
        mca_oob_ud_component_close,
	NULL, /* component query */
	mca_oob_ud_component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    mca_oob_ud_component_init
  }
};

static int mca_oob_ud_component_open (void)
{
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_devices, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_active_sends, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_pending_recvs, opal_list_t);

    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_active_recvs, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_event_queued_reqs, opal_list_t);

    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_unexpected_recvs, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_completed, opal_list_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_event_processing_msgs, opal_list_t);

    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_match_lock, opal_mutex_t);

    OBJ_CONSTRUCT(&mca_oob_ud_component.ud_peers, opal_hash_table_t);

    return ORTE_SUCCESS;
}

static int mca_oob_ud_component_close (void)
{
    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:component_close entering",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_devices);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_active_sends);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_pending_recvs);

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_active_recvs);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_event_queued_reqs);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_unexpected_recvs);

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_lock);
    OBJ_DESTRUCT(&mca_oob_ud_component.ud_match_lock);

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_peers);

    OBJ_DESTRUCT(&mca_oob_ud_component.ud_completed);
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

    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:port_setup found port: num = %u,"
                         "lid = %u, mtu = %u", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         port->port_num, port->lid, port->mtu));

    return rc;
}

static inline int mca_oob_ud_device_setup (mca_oob_ud_device_t *device,
                                           struct ibv_device *ib_device)
{
    int rc, port_num;
    struct ibv_device_attr dev_attr;

    OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:device_setup attempting to setup ib device %p",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (void *) ib_device));

    device->ib_context = ibv_open_device (ib_device);
    if (NULL == device->ib_context) {
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:device_setup error opening device. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno));
        return ORTE_ERROR;
    }

    rc = ibv_query_device (device->ib_context, &dev_attr); 
    if (0 != rc) {
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:device_setup error querying device. errno = %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno));
        return ORTE_ERROR;
    }

    device->ib_channel = ibv_create_comp_channel (device->ib_context);
    if (NULL == device->ib_channel) {
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:device_setup error completing completion channel."
                             "errno = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno));
        return ORTE_ERROR;
    }

    device->ib_pd = ibv_alloc_pd (device->ib_context);
    if (NULL == device->ib_pd) {
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:device_setup error allocating protection domain."
                             "errno = %d", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), errno));
        return ORTE_ERROR;
    }

    for (port_num = 1 ; port_num <= dev_attr.phys_port_cnt ; ++port_num) {
        mca_oob_ud_port_t *port = OBJ_NEW(mca_oob_ud_port_t);

        if (NULL == port) {
            opal_output (0, "oob:ud:device_setup malloc failure. errno = %d", errno);
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
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:device_setup could not init device. no usable "
                             "ports present", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static mca_oob_t *mca_oob_ud_component_init(int *priority)
{
    struct ibv_device **devices;
    int num_devices, i, rc;

    /* set the priority so that we will select this component
     * only if someone directs to do so
     */
    *priority = 0;

    opal_hash_table_init (&mca_oob_ud_component.ud_peers, 1024);

    devices = ibv_get_device_list (&num_devices);
    if (NULL == devices || 0 == num_devices) {
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:component_init no devices found",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return NULL;
    }

    for (i = 0 ; i < num_devices ; ++i) {
        mca_oob_ud_device_t *device = OBJ_NEW(mca_oob_ud_device_t);

        if (NULL == device) {
            opal_output (0, "oob:ud:component_init malloc failure. errno = %d",
                         errno);
            return NULL;
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

    if (0 == opal_list_get_size (&mca_oob_ud_component.ud_devices)) {
        OPAL_OUTPUT_VERBOSE((5, mca_oob_base_output, "%s oob:ud:component_init no usable devices found.",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return NULL;
    }

    /* have to call the module init here so we can test for available qpair */
    if (ORTE_SUCCESS != mca_oob_ud_module_init()) {
        return NULL;
    }

    return &mca_oob_ud_module;
}
