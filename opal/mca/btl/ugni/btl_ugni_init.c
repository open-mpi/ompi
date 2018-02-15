/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "btl_ugni.h"
#include "btl_ugni_endpoint.h"
#include "btl_ugni_frag.h"

#include "opal/class/opal_list.h"
#include "opal/dss/dss.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/bit_ops.h"
#include "opal/mca/hwloc/base/base.h"

static inline int get_ptag(uint8_t *out_ptag)
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

static unsigned int mca_btl_ugni_get_nic_address(int device_id)
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

int mca_btl_ugni_device_init (mca_btl_ugni_device_t *device, int virtual_device_id)
{
    uint32_t dev_pe_addr;
    int rc;

    OBJ_CONSTRUCT(&device->rdma_descs, opal_free_list_t);

    /* create a communication domain */
    rc = GNI_CdmCreate (mca_btl_ugni_component.cdm_id_base | virtual_device_id, mca_btl_ugni_component.ptag,
                        mca_btl_ugni_component.cookie, mca_btl_ugni_component.cdm_flags, &device->dev_cd_handle);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        /* this REALLY is an error but under alps + mapn we may not get any credentials */
        BTL_VERBOSE(("Error: Creating communication domain %d for virtual device %d", rc, virtual_device_id));
        return mca_btl_rc_ugni_to_opal (rc);
    }

    device->dev_index = virtual_device_id;

    /* Create a NIC Adress */
    OPAL_OUTPUT((-1, "Got NIC Addr: 0x%08x, CPU ID: %d", mca_btl_ugni_component.dev_addr, 0));

    /* Attach device to the communication domain */
    rc = GNI_CdmAttach (device->dev_cd_handle, 0, &dev_pe_addr, &device->dev_handle);
    if (GNI_RC_SUCCESS != rc) {
        BTL_VERBOSE(("Error: Attaching to communication domain. rc = %d, virtual device = %d", rc, virtual_device_id));
        return mca_btl_rc_ugni_to_opal (rc);
    }

    rc = opal_free_list_init (&device->rdma_descs, sizeof (mca_btl_ugni_rdma_desc_t),
                              64, OBJ_CLASS(mca_btl_ugni_rdma_desc_t), 0, 8, 0,
                              mca_btl_ugni_component.local_rdma_cq_size, 32,
                              NULL, 0, NULL, mca_btl_ugni_rdma_desc_init, (void *) device);
    if (OPAL_SUCCESS != rc) {
        OBJ_DESTRUCT(&device->rdma_descs);
        return rc;
    }

    device->lock = 0;
    device->dev_rdma_local_cq.gni_handle = 0;
    device->dev_rdma_local_cq.active_operations = 0;
    device->dev_rdma_local_irq_cq.gni_handle = 0;
    device->dev_rdma_local_irq_cq.active_operations = 0;
    device->dev_smsg_local_cq.gni_handle = 0;
    device->dev_smsg_local_cq.active_operations = 0;
    device->flushed = true;

    return OPAL_SUCCESS;
}

int mca_btl_ugni_device_fini (mca_btl_ugni_device_t *dev)
{
    int rc;

    OBJ_DESTRUCT(&dev->rdma_descs);

    if (0 != dev->dev_rdma_local_cq.gni_handle) {
        GNI_CqDestroy (dev->dev_rdma_local_cq.gni_handle);
        dev->dev_rdma_local_cq.gni_handle = 0;
    }

    if (0 != dev->dev_rdma_local_irq_cq.gni_handle) {
        GNI_CqDestroy (dev->dev_rdma_local_irq_cq.gni_handle);
        dev->dev_rdma_local_irq_cq.gni_handle = 0;
    }

    if (0 != dev->dev_smsg_local_cq.gni_handle) {
        GNI_CqDestroy (dev->dev_smsg_local_cq.gni_handle);
        dev->dev_smsg_local_cq.gni_handle = 0;
    }

    rc = GNI_CdmDestroy (dev->dev_cd_handle);
    if (GNI_RC_SUCCESS != rc) {
        BTL_VERBOSE(("error destroying cdm handle"));
    }

    return OPAL_SUCCESS;
}

/*
 * Send local device information and other information
 * required for setup
 */
static int mca_btl_ugni_send_modex (void)
{
    struct mca_btl_ugni_modex_t modex;
    uint32_t modex_size;
    char *modex_msg;
    int rc;

    modex_size = sizeof (struct mca_btl_ugni_modex_t);

    modex_msg = (char *) malloc (modex_size);
    if (NULL == modex_msg) {
        OPAL_OUTPUT((-1, "Error allocating memory for modex @ %s:%d",
                     __FILE__, __LINE__));
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    modex.addr = mca_btl_ugni_component.dev_addr;
    modex.id   = mca_btl_ugni_component.cdm_id_base;

    BTL_VERBOSE(("sending modex. addr: %d, id: %d", modex.addr, modex.id));

    memcpy ((void *) modex_msg, (void *) &modex, modex_size);

    /*
     * need global for edge cases like MPI_Comm_spawn support with
     * new ranks started on the same nodes as the spawnee ranks, etc.
     */

    OPAL_MODEX_SEND(rc, OPAL_PMIX_GLOBAL,
                    &mca_btl_ugni_component.super.btl_version,
                    modex_msg, modex_size);

    free (modex_msg);

    return rc;
}

int mca_btl_ugni_fini (void)
{
    return OPAL_SUCCESS;
}

int mca_btl_ugni_init (void)
{
    int32_t pid_max = 32768;
    int rc, bit;
    FILE *fh;

    if (0 == mca_btl_ugni_component.virtual_device_count) {
        int core_count;

        (void) opal_hwloc_base_get_topology ();
        core_count = hwloc_get_nbobjs_by_type (opal_hwloc_topology, HWLOC_OBJ_CORE);

        if (core_count <= opal_process_info.num_local_peers || !opal_using_threads()) {
            /* there is probably no benefit to using multiple device contexts when not
             * using threads. */
            mca_btl_ugni_component.virtual_device_count = 1;
        } else {
            mca_btl_ugni_component.virtual_device_count = core_count / (opal_process_info.num_local_peers + 1);
        }
    }

    if (MCA_BTL_UGNI_MAX_DEV_HANDLES < mca_btl_ugni_component.virtual_device_count) {
        mca_btl_ugni_component.virtual_device_count = MCA_BTL_UGNI_MAX_DEV_HANDLES;
    }

    if (0 == mca_btl_ugni_component.local_rdma_cq_size) {
        if (1 == mca_btl_ugni_component.virtual_device_count) {
            mca_btl_ugni_component.local_rdma_cq_size = 2048;
        } else {
            mca_btl_ugni_component.local_rdma_cq_size = 256;
        }
    }

    if ((mca_btl_ugni_component.virtual_device_count * (1 + opal_process_info.num_local_peers)) < 122) {
        /* if there are fewer total devices than FMA descriptors it makes sense to turn off FMA sharing.
         * *DO NOT* override a user requested flag. */
        mca_base_var_source_t source = MCA_BASE_VAR_SOURCE_DEFAULT;

        mca_base_var_get_value (mca_btl_ugni_component.cdm_flags_id, NULL, &source, NULL);
        if (MCA_BASE_VAR_SOURCE_DEFAULT == source) {
            BTL_VERBOSE(("disabling shared FMA sharing"));

            mca_btl_ugni_component.cdm_flags &= ~GNI_CDM_MODE_FMA_SHARED;
            mca_btl_ugni_component.cdm_flags |= GNI_CDM_MODE_FMA_DEDICATED;
        }
    }

    fh = fopen ("/proc/sys/kernel/pid_max", "r");
    if (NULL != fh) {
        fscanf (fh, "%d", &pid_max);
        fclose (fh);
    }

    /* Use pid to generate the cdm_id.  Although its not stated in the uGNI
     * documentation, the cdm_id only needs to be unique within a node for a
     * given ptag/cookie tuple */
    bit = opal_hibit (pid_max, 31);
    if (bit >= 31) {
        mca_btl_ugni_component.virtual_device_count = 1;
        mca_btl_ugni_component.cdm_id_base = getpid();
    } else if (bit >= 30 && mca_btl_ugni_component.virtual_device_count > 2) {
        mca_btl_ugni_component.virtual_device_count = 2;
        mca_btl_ugni_component.cdm_id_base = getpid() << 1;
    } else {
        mca_btl_ugni_component.cdm_id_base = getpid() << 8;
    }

    /* Create a communication domain */
    /* collect uGNI information */
    rc = get_ptag(&mca_btl_ugni_component.ptag);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    rc = get_cookie(&mca_btl_ugni_component.cookie);
    if (OPAL_UNLIKELY(OPAL_SUCCESS != rc)) {
        return rc;
    }

    /* get the device address of the NIC */
    mca_btl_ugni_component.dev_addr = mca_btl_ugni_get_nic_address (0);

    /* send ugni modex */
    mca_btl_ugni_send_modex ();

    return OPAL_SUCCESS;
}
