/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "common_ugni.h"

#include "ompi/proc/proc.h"
#include "opal/mca/db/db.h"

/* NTH: we need some options from the btl */
#include "ompi/mca/btl/ugni/btl_ugni.h"

static int ompi_common_ugni_module_ref_count = 0;
ompi_common_ugni_module_t ompi_common_ugni_module;

mca_base_component_t ompi_common_ugni_component = {
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
        return OMPI_ERR_NOT_FOUND;
    }
    errno = 0;
    tmp_ptag = (uint8_t)strtoul (ptr, (char **)NULL, 10);
    if (0 != errno) {
        /* TODO add err msg - better rc? */
        return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
    }
    *out_ptag = tmp_ptag;
    return OMPI_SUCCESS;
}

static inline int get_cookie (uint32_t *out_cookie)
{
    /* TODO no need for tmp */
    char *ptr;
    uint32_t tmp_cookie;

    if (NULL == (ptr = getenv("PMI_GNI_COOKIE"))) {
        /* TODO add err msg - better rc? */
        return OMPI_ERR_NOT_FOUND;
    }
    errno = 0;
    tmp_cookie = (uint32_t) strtoul (ptr, NULL, 10);
    if (0 != errno) {
        /* TODO add err msg - better rc? */
        return OMPI_ERR_VALUE_OUT_OF_BOUNDS;
    }

    *out_cookie = tmp_cookie;

    return OMPI_SUCCESS;
}

static unsigned int
ompi_common_ugni_get_nic_address(int device_id)
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

static int ompi_common_ugni_device_init (ompi_common_ugni_device_t *device,
                                         int device_id)
{
    int rc;

    /* Create a NIC Adress */
    device->dev_id = device_id; /* Minor number of the Gemini NIC */

    device->dev_addr = ompi_common_ugni_get_nic_address (device->dev_id);

    OPAL_OUTPUT((-1, "Got NIC Addr: 0x%08x, CPU ID: %d", device->dev_addr, device->dev_id));

    /* Attach device to the communication domain */
    rc = GNI_CdmAttach (ompi_common_ugni_module.cd_handle, device->dev_id,
                        &device->dev_pe_addr, &device->dev_handle);
    if (GNI_RC_SUCCESS != rc) {
        OPAL_OUTPUT((0, "Error: Creating communication domain %d\n", rc));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    return OMPI_SUCCESS;
}

static int ompi_common_ugni_device_fini (ompi_common_ugni_device_t *dev)
{
    return OMPI_SUCCESS;
}

/*
 * Send local device information and other information
 * required for setup
 */
static int ompi_common_ugni_send_modex (int my_rank)
{
    uint32_t modex_size, total_msg_size, msg_offset;
    struct ompi_common_ugni_modex_t modex;
    char *modex_msg;
    int rc, i;

    modex_size = sizeof (struct ompi_common_ugni_modex_t);
    total_msg_size = ompi_common_ugni_module.device_count * modex_size;

    modex_msg = (char *) malloc (total_msg_size);
    if (NULL == modex_msg) {
        OPAL_OUTPUT((-1, "Error allocating memory for modex @ %s:%d",
                     __FILE__, __LINE__));
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* pack modex for all available devices */
    for (i = 0, msg_offset = 0; i < ompi_common_ugni_module.device_count ; ++i) {
        ompi_common_ugni_device_t *dev = ompi_common_ugni_module.devices + i;

        modex.addr = dev->dev_addr;
        modex.id   = my_rank;

        memcpy ((void *)((uintptr_t) modex_msg + msg_offset),
                (void *)&modex, modex_size);

        msg_offset += modex_size;
    }

    rc = ompi_modex_send(&ompi_common_ugni_component,
                         modex_msg, total_msg_size);

    free(modex_msg);

    return rc;
}

int ompi_common_ugni_fini (void)
{
    int i, rc;

    if (0 == ompi_common_ugni_module_ref_count) {
        return OMPI_SUCCESS;
    }

    if (1 == ompi_common_ugni_module_ref_count) {
        /* tear down component */
        if (ompi_common_ugni_module.devices) {
            /* finalize devices */
            for (i = 0 ; i < ompi_common_ugni_module.device_count ; ++i) {
                ompi_common_ugni_device_fini (ompi_common_ugni_module.devices + i);
            }

            free (ompi_common_ugni_module.devices);
            ompi_common_ugni_module.devices = NULL;
        }

        /* finally, tear down the communication domain */
        rc = GNI_CdmDestroy (ompi_common_ugni_module.cd_handle);
        if (GNI_RC_SUCCESS != rc) {
            OPAL_OUTPUT((-1, "error destroying cdm"));
        }
    }

    ompi_common_ugni_module_ref_count--;

    return OMPI_SUCCESS;
}

int ompi_common_ugni_init (void)
{
    ompi_proc_t *my_proc;
    int modes, rc, i;
    uint32_t my_rank, *ptr;

    ompi_common_ugni_module_ref_count ++;

    if (ompi_common_ugni_module_ref_count > 1) {
        return OMPI_SUCCESS;
    }

    my_proc = ompi_proc_local ();

    /* get a unique id from the runtime */
#if defined(OMPI_DB_GLOBAL_RANK)
    ptr = &my_rank;
    rc = opal_db.fetch ((opal_identifier_t *) &my_proc->proc_name, OMPI_DB_GLOBAL_RANK,
                        (void **) &ptr, OPAL_UINT32);
    if (OPAL_SUCCESS != rc) {
        my_rank = my_proc->proc_name.vpid;
    }
#else
    my_rank = my_proc->proc_name.vpid;
#endif

    /* pull settings from ugni btl */
    ompi_common_ugni_module.rdma_max_retries =
        mca_btl_ugni_component.rdma_max_retries;

    /* Create a communication domain */
    modes = GNI_CDM_MODE_FORK_FULLCOPY | GNI_CDM_MODE_CACHED_AMO_ENABLED |
            GNI_CDM_MODE_ERR_NO_KILL | GNI_CDM_MODE_FAST_DATAGRAM_POLL;

    /* collect uGNI information */
    rc = get_ptag(&ompi_common_ugni_module.ptag);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    rc = get_cookie(&ompi_common_ugni_module.cookie);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
        return rc;
    }

    /* create a communication domain */
    rc = GNI_CdmCreate (my_rank, ompi_common_ugni_module.ptag,
                        ompi_common_ugni_module.cookie, modes,
                        &ompi_common_ugni_module.cd_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        OPAL_OUTPUT((0, "Error: Creating communication domain %d\n",rc));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    /* setup uGNI devices. we only support one device atm */
    ompi_common_ugni_module.device_count = 1;
    ompi_common_ugni_module.devices = calloc (ompi_common_ugni_module.device_count,
                                              sizeof (ompi_common_ugni_device_t));

    for (i = 0 ; i < ompi_common_ugni_module.device_count ; ++i) {
        rc = ompi_common_ugni_device_init (ompi_common_ugni_module.devices + i, i);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != rc)) {
            OPAL_OUTPUT((-1, "error initializing uGNI device"));
            return rc;
        }
    }

    /* send ugni modex */
    ompi_common_ugni_send_modex (my_rank);

    return OMPI_SUCCESS;
}
