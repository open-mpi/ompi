/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "common_ugni.h"

#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"
#include "opal/mca/pmix/pmix.h"

/* NTH: we need some options from the btl */
#include "opal/mca/btl/ugni/btl_ugni.h"

static int opal_common_ugni_module_ref_count = 0;
opal_common_ugni_module_t opal_common_ugni_module;

mca_base_component_t opal_common_ugni_component = {
    MCA_BASE_VERSION_2_0_0,
    "common",
    MCA_BASE_VERSION_2_0_0,
    "ugni",
    MCA_BASE_VERSION_2_0_0,
    NULL,
    NULL
};

static inline int
get_ptag(uint8_t *out_ptag)
{
    /* TODO no need for tmp */
    char *ptr;
    uint8_t tmp_ptag;

    if (NULL == (ptr = getenv("PMI_GNI_PTAG"))) {
        /* TODO add err msg - better rc? */
        return OPAL_ERR_NOT_FOUND;
    }
    errno = 0;
    tmp_ptag = (uint8_t)strtoul (ptr, (char **)NULL, 10);
    if (0 != errno) {
        /* TODO add err msg - better rc? */
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }
    *out_ptag = tmp_ptag;
    return OPAL_SUCCESS;
}

static inline int get_cookie (uint32_t *out_cookie)
{
    /* TODO no need for tmp */
    char *ptr;
    uint32_t tmp_cookie;

    if (NULL == (ptr = getenv("PMI_GNI_COOKIE"))) {
        /* TODO add err msg - better rc? */
        return OPAL_ERR_NOT_FOUND;
    }
    errno = 0;
    tmp_cookie = (uint32_t) strtoul (ptr, NULL, 10);
    if (0 != errno) {
        /* TODO add err msg - better rc? */
        return OPAL_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *out_cookie = tmp_cookie;

    return OPAL_SUCCESS;
}

static unsigned int
opal_common_ugni_get_nic_address(int device_id)
{
    unsigned int address, cpu_id;
    gni_return_t status;
    int i, alps_dev_id = -1;
    char *token,*p_ptr;

    p_ptr = getenv("PMI_GNI_DEV_ID");
    if (!p_ptr) {
        status = GNI_CdmGetNicAddress(device_id, &address, &cpu_id);
        if(status != GNI_RC_SUCCESS) {
            opal_output (0, "FAILED:GNI_CdmGetNicAddress returned error %d", status);
            return (unsigned int)-1;
        }
        return address;
    }

    while (NULL != (token = strtok(p_ptr, ":"))) {
        alps_dev_id = atoi(token);
        if (alps_dev_id == device_id) {
            break;
        }
        p_ptr = NULL;
    }

    if (OPAL_UNLIKELY(-1 == alps_dev_id)) {
        return (unsigned int)-1;
    }

    p_ptr = getenv("PMI_GNI_LOC_ADDR");
    if (OPAL_UNLIKELY(NULL == p_ptr)) {
        return (unsigned int)-1;
    }

    i = 0;
    while (NULL != (token = strtok(p_ptr, ":"))) {
        if (i == alps_dev_id) {
            return strtoul (token, NULL, 10);
        }
        p_ptr = NULL;
        ++i;
    }

    return (unsigned int)-1;
}

static int opal_common_ugni_device_init (opal_common_ugni_device_t *device,
                                         int device_id)
{
    int rc;

    /* Create a NIC Adress */
    device->dev_id = device_id; /* Minor number of the Gemini NIC */

    device->dev_addr = opal_common_ugni_get_nic_address (device->dev_id);

    OPAL_OUTPUT((-1, "Got NIC Addr: 0x%08x, CPU ID: %d", device->dev_addr, device->dev_id));

    OBJ_CONSTRUCT(&device->dev_lock,opal_mutex_t);

    /* Attach device to the communication domain */
    rc = GNI_CdmAttach (opal_common_ugni_module.cd_handle, device->dev_id,
                        &device->dev_pe_addr, &device->dev_handle);
    if (GNI_RC_SUCCESS != rc) {
        OPAL_OUTPUT((0, "Error: Creating communication domain %d\n", rc));
        return opal_common_rc_ugni_to_opal (rc);
    }

    return OPAL_SUCCESS;
}

static int opal_common_ugni_device_fini (opal_common_ugni_device_t *dev)
{
    return OPAL_SUCCESS;
}

/*
 * Send local device information and other information
 * required for setup
 */
static int opal_common_ugni_send_modex (int my_cdm_id)
{
    uint32_t modex_size, total_msg_size, msg_offset;
    struct opal_common_ugni_modex_t modex;
    char *modex_msg;
    int rc, i;

    modex_size = sizeof (struct opal_common_ugni_modex_t);
    total_msg_size = opal_common_ugni_module.device_count * modex_size;

    modex_msg = (char *) malloc (total_msg_size);
    if (NULL == modex_msg) {
        OPAL_OUTPUT((-1, "Error allocating memory for modex @ %s:%d",
                     __FILE__, __LINE__));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* pack modex for all available devices */
    for (i = 0, msg_offset = 0; i < opal_common_ugni_module.device_count ; ++i) {
        opal_common_ugni_device_t *dev = opal_common_ugni_module.devices + i;

        modex.addr = dev->dev_addr;
        modex.id   = my_cdm_id;

        memcpy ((void *)((uintptr_t) modex_msg + msg_offset),
                (void *)&modex, modex_size);

        msg_offset += modex_size;
    }

    /*
     * need global for edge cases like MPI_Comm_spawn support with
     * new ranks started on the same nodes as the spawnee ranks, etc.
     */

    OPAL_MODEX_SEND(rc, PMIX_ASYNC_RDY, PMIX_GLOBAL,
                    &opal_common_ugni_component,
                    modex_msg, total_msg_size);

    free(modex_msg);

    return rc;
}

int opal_common_ugni_fini (void)
{
    int i, rc;

    if (0 == opal_common_ugni_module_ref_count) {
        return OPAL_SUCCESS;
    }

    if (1 == opal_common_ugni_module_ref_count) {
        /* tear down component */
        if (opal_common_ugni_module.devices) {
            /* finalize devices */
            for (i = 0 ; i < opal_common_ugni_module.device_count ; ++i) {
                opal_common_ugni_device_fini (opal_common_ugni_module.devices + i);
            }

            free (opal_common_ugni_module.devices);
            opal_common_ugni_module.devices = NULL;
        }

        /* finally, tear down the communication domain */
        rc = GNI_CdmDestroy (opal_common_ugni_module.cd_handle);
        if (GNI_RC_SUCCESS != rc) {
            OPAL_OUTPUT((-1, "error destroying cdm"));
        }
    }

    opal_common_ugni_module_ref_count--;

    return OPAL_SUCCESS;
}

int opal_common_ugni_init (void)
{
    int modes, rc, i;
    uint32_t my_cdm_id;

    opal_common_ugni_module_ref_count ++;

    if (opal_common_ugni_module_ref_count > 1) {
        return OPAL_SUCCESS;
    }

    /* use pid for my_cdm_id.  Although its not stated in the uGNI
       documentation, the cdm_id only needs to be unique
       within a node for a given ptag/cookie tuple */

    my_cdm_id = getpid();   /*TODO: eventually need something else for thread-hot support */

    /* pull settings from ugni btl */
    opal_common_ugni_module.rdma_max_retries =
        mca_btl_ugni_component.rdma_max_retries;

    /* Create a communication domain */

    modes = GNI_CDM_MODE_FORK_FULLCOPY | GNI_CDM_MODE_CACHED_AMO_ENABLED |
            GNI_CDM_MODE_ERR_NO_KILL | GNI_CDM_MODE_FAST_DATAGRAM_POLL;

    /* collect uGNI information */
    rc = get_ptag(&opal_common_ugni_module.ptag);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    rc = get_cookie(&opal_common_ugni_module.cookie);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    /* create a communication domain */
    rc = GNI_CdmCreate (my_cdm_id, opal_common_ugni_module.ptag,
                        opal_common_ugni_module.cookie, modes,
                        &opal_common_ugni_module.cd_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        OPAL_OUTPUT((0, "Error: Creating communication domain %d\n",rc));
        return opal_common_rc_ugni_to_opal (rc);
    }

    /* setup uGNI devices. we only support one device atm */
    opal_common_ugni_module.device_count = 1;
    opal_common_ugni_module.devices = calloc (opal_common_ugni_module.device_count,
                                              sizeof (opal_common_ugni_device_t));

    for (i = 0 ; i < opal_common_ugni_module.device_count ; ++i) {
        rc = opal_common_ugni_device_init (opal_common_ugni_module.devices + i, i);
        if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
            OPAL_OUTPUT((-1, "error initializing uGNI device"));
            return rc;
        }
    }

    /* send ugni modex */
    opal_common_ugni_send_modex (my_cdm_id);

    return OPAL_SUCCESS;
}
