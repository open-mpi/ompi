/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "opal/constants.h"
#include "opal/util/sys_limits.h"
#include "opal/mca/common/verbs/common_verbs.h"

#include "oshmem/mca/sshmem/sshmem.h"
#include "oshmem/mca/sshmem/base/base.h"

#include "sshmem_verbs.h"

/**
 * public string showing the shmem ompi_mmap component version number
 */
const char *mca_sshmem_verbs_component_version_string =
    "OSHMEM mmap sshmem MCA component version " OSHMEM_VERSION;

int mca_sshmem_verbs_relocate_backing_file = 0;
char *mca_sshmem_verbs_backing_file_base_dir = NULL;
bool mca_sshmem_verbs_nfs_warning = true;

/**
 * local functions
 */
static int verbs_register(void);
static int verbs_open(void);
static int verbs_close(void);
static int verbs_query(mca_base_module_t **module, int *priority);
static int verbs_runtime_query(mca_base_module_t **module,
                               int *priority,
                               const char *hint);

/**
 * instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_sshmem_verbs_component_t mca_sshmem_verbs_component = {
    /* ////////////////////////////////////////////////////////////////////// */
    /* super */
    /* ////////////////////////////////////////////////////////////////////// */
    {
        /**
         * common MCA component data
         */
        {
            MCA_SSHMEM_BASE_VERSION_2_0_0,

            /* component name and version */
            "verbs",
            OSHMEM_MAJOR_VERSION,
            OSHMEM_MINOR_VERSION,
            OSHMEM_RELEASE_VERSION,

            /* component open */
            verbs_open,
            /* component close */
            verbs_close,
            /* component query */
            verbs_query,
            /* component register */
            verbs_register
        },
        /* MCA v2.0.0 component meta data */
        {
            /* the component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        },
        verbs_runtime_query,
    },
};

/* ////////////////////////////////////////////////////////////////////////// */
static int
verbs_runtime_query(mca_base_module_t **module,
                    int *priority,
                    const char *hint)
{
    int rc = OSHMEM_SUCCESS;
    openib_device_t my_device;
    openib_device_t *device = &my_device;
    int num_devs = 0;
    int i = 0;

    *priority = 0;
    *module = NULL;

    /* If fork support is requested, try to enable it */
    if (OSHMEM_SUCCESS != (rc = opal_common_verbs_fork_test())) {
        return OSHMEM_ERROR;
    }

    memset(device, 0, sizeof(*device));

#ifdef HAVE_IBV_GET_DEVICE_LIST
    device->ib_devs = ibv_get_device_list(&num_devs);
#else
    #error unsupported ibv_get_device_list in infiniband/verbs.h
#endif

    if (num_devs == 0 || !device->ib_devs) {
        return OSHMEM_ERR_NOT_SUPPORTED;
    }

    /* Open device */
    if (NULL != mca_sshmem_verbs_component.hca_name) {
        for (i = 0; i < num_devs; i++) {
            if (0 == strcmp(mca_sshmem_verbs_component.hca_name, ibv_get_device_name(device->ib_devs[i]))) {
                device->ib_dev = device->ib_devs[i];
                break;
            }
        }
    } else {
        device->ib_dev = device->ib_devs[0];
    }

    if (NULL == device->ib_dev) {
        rc = OSHMEM_ERR_NOT_FOUND;
        goto out;
    }

    if (NULL == (device->ib_dev_context = ibv_open_device(device->ib_dev))) {
        rc = OSHMEM_ERR_RESOURCE_BUSY;
        goto out;
    }

    /* Obtain device attributes */
    if (ibv_query_device(device->ib_dev_context, &device->ib_dev_attr)) {
        rc = OSHMEM_ERR_RESOURCE_BUSY;
        goto out;
    }

    /* Allocate the protection domain for the device */
    device->ib_pd = ibv_alloc_pd(device->ib_dev_context);
    if (NULL == device->ib_pd) {
        rc = OSHMEM_ERR_RESOURCE_BUSY;
        goto out;
    }

    /* Allocate memory */
    if (!rc) {
        void *addr = NULL;
        size_t size = (size_t)opal_getpagesize();
        struct ibv_mr *ib_mr = NULL;
        uint64_t access_flag = IBV_ACCESS_LOCAL_WRITE |
                          IBV_ACCESS_REMOTE_WRITE |
                          IBV_ACCESS_REMOTE_READ; 
        uint64_t exp_access_flag = 0;

        OBJ_CONSTRUCT(&device->ib_mr_array, opal_value_array_t);
        opal_value_array_init(&device->ib_mr_array, sizeof(struct ibv_mr *));

#if (MPAGE_ENABLE > 0)
        exp_access_flag = IBV_EXP_ACCESS_ALLOCATE_MR  |
                          IBV_EXP_ACCESS_SHARED_MR_USER_READ |
                          IBV_EXP_ACCESS_SHARED_MR_USER_WRITE; 
#endif /* MPAGE_ENABLE */

        struct ibv_exp_reg_mr_in in = {device->ib_pd, addr, size, access_flag|exp_access_flag, 0};
        ib_mr = ibv_exp_reg_mr(&in);
        if (NULL == ib_mr) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        } else {
            device->ib_mr_shared = ib_mr;
            opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
        }

#if (MPAGE_ENABLE > 0)
        if (!rc && (0 != mca_sshmem_verbs_component.has_shared_mr)) {
            struct ibv_exp_reg_shared_mr_in in_smr;

            access_flag = IBV_ACCESS_LOCAL_WRITE |
                          IBV_ACCESS_REMOTE_WRITE |
                          IBV_ACCESS_REMOTE_READ|
                          IBV_EXP_ACCESS_NO_RDMA;

            addr = (void *)mca_sshmem_base_start_address;
            mca_sshmem_verbs_fill_shared_mr(&in_smr, device->ib_pd, device->ib_mr_shared->handle,  addr, access_flag);
            ib_mr = ibv_exp_reg_shared_mr(&in_smr);
            if (NULL == ib_mr) {
                if (mca_sshmem_verbs_component.has_shared_mr == 1)
                    rc = OSHMEM_ERR_OUT_OF_RESOURCE;

                mca_sshmem_verbs_component.has_shared_mr = 0;
            } else {
                opal_value_array_append_item(&device->ib_mr_array, &ib_mr);
                mca_sshmem_verbs_component.has_shared_mr = 1;
            }
        }
#else
        if (!rc && mca_sshmem_verbs_component.has_shared_mr == 1) {
            rc = OSHMEM_ERR_OUT_OF_RESOURCE;
        }
        mca_sshmem_verbs_component.has_shared_mr = 0;
#endif /* MPAGE_ENABLE */
    }

#if !MPAGE_HAVE_IBV_EXP_REG_MR_CREATE_FLAGS
    /* disqualify ourselves if we can not alloc contig
     * pages at fixed address
     */
    if (mca_sshmem_verbs_component.has_shared_mr == 0)
        rc = OSHMEM_ERR_OUT_OF_RESOURCE;
#endif

    /* all is well - rainbows and butterflies */
    if (!rc) {
        *priority = mca_sshmem_verbs_component.priority;
        *module = (mca_base_module_t *)&mca_sshmem_verbs_module.super;
    }

out:
    if (device) {
        if (0 < (i = opal_value_array_get_size(&device->ib_mr_array))) {
            struct ibv_mr** array;
            struct ibv_mr* ib_mr = NULL;
            array = OPAL_VALUE_ARRAY_GET_BASE(&device->ib_mr_array, struct ibv_mr *);
            /* destruct shared_mr first in order to avoid proc fs race */
            for (i--;i >= 0; i--) {
                ib_mr = array[i];
                ibv_dereg_mr(ib_mr);
                opal_value_array_remove_item(&device->ib_mr_array, i);
            }

            if (device->ib_mr_shared) {
                device->ib_mr_shared = NULL;
            }
            OBJ_DESTRUCT(&device->ib_mr_array);
        }

        if (device->ib_pd) {
            ibv_dealloc_pd(device->ib_pd);
            device->ib_pd = NULL;
        }

        if(device->ib_dev_context) {
            ibv_close_device(device->ib_dev_context);
            device->ib_dev_context = NULL;
        }

        if(device->ib_devs) {
            ibv_free_device_list(device->ib_devs);
            device->ib_devs = NULL;
        }
    }

    return rc;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
verbs_register(void)
{
    int index;

    /* ////////////////////////////////////////////////////////////////////// */
    /* (default) priority - set high to make verbs the default */
    mca_sshmem_verbs_component.priority = 40;
    index = mca_base_component_var_register (&mca_sshmem_verbs_component.super.base_version,
                                           "priority", "Priority for sshmem verbs "
                                           "component (default: 40)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_sshmem_verbs_component.priority);

    mca_sshmem_verbs_component.hca_name = NULL;
    index = mca_base_component_var_register (&mca_sshmem_verbs_component.super.base_version,
                                           "hca_name", "Preferred hca (default: the first)", MCA_BASE_VAR_TYPE_STRING,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_sshmem_verbs_component.hca_name);
    if (index) {
        (void) mca_base_var_register_synonym(index, "oshmem", "memheap", "base",
                                         "hca_name",
                                         MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }
    /* allow user specify hca port, extract hca name
     * ex: mlx_4_0:1 is allowed
     */
    if (mca_sshmem_verbs_component.hca_name) {
        char *p;

        p = strchr(mca_sshmem_verbs_component.hca_name, ':');
        if (p)
            *p = 0;
    }


    mca_sshmem_verbs_component.mr_interleave_factor = 2;
    index = mca_base_component_var_register (&mca_sshmem_verbs_component.super.base_version,
                                           "mr_interleave_factor", "try to give at least N Gbytes spaces between mapped memheaps "
                                           "of other PEs that are local to me (default: 2)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_sshmem_verbs_component.mr_interleave_factor);
    if (index) {
        (void) mca_base_var_register_synonym(index, "oshmem", "memheap", "base",
                                         "mr_interleave_factor",
                                         MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    }

    mca_sshmem_verbs_component.has_shared_mr = -1;
    index = mca_base_component_var_register (&mca_sshmem_verbs_component.super.base_version,
                                           "shared_mr", "Shared memory region usage "
                                           "[0 - off, 1 - on, -1 - auto] (default: -1)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                           OPAL_INFO_LVL_4,
                                           MCA_BASE_VAR_SCOPE_ALL_EQ,
                                           &mca_sshmem_verbs_component.has_shared_mr);

    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
verbs_open(void)
{
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
verbs_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_sshmem_verbs_component.priority;
    *module = (mca_base_module_t *)&mca_sshmem_verbs_module.super;
    return OSHMEM_SUCCESS;
}

/* ////////////////////////////////////////////////////////////////////////// */
static int
verbs_close(void)
{
    return OSHMEM_SUCCESS;
}

