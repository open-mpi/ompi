/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Intel, Inc, All rights reserved
 *
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/util/printf.h"

#include "opal/mca/btl/btl.h"
#include "opal/mca/btl/base/base.h"
#include "opal/mca/hwloc/base/base.h"

#include <string.h>

#include "btl_ofi.h"
#include "btl_ofi_endpoint.h"
#include "btl_ofi_rdma.h"
#include "btl_ofi_frag.h"

#define MCA_BTL_OFI_ONE_SIDED_REQUIRED_CAPS       (FI_RMA | FI_ATOMIC)
#define MCA_BTL_OFI_TWO_SIDED_REQUIRED_CAPS       (FI_MSG)

#define MCA_BTL_OFI_REQUESTED_MR_MODE   (FI_MR_ALLOCATED | FI_MR_PROV_KEY | FI_MR_VIRT_ADDR)

static char *prov_include;
static char *ofi_progress_mode;
static bool disable_sep;
static int mca_btl_ofi_init_device(struct fi_info *info);

/* validate information returned from fi_getinfo().
 * return OPAL_ERROR if we dont have what we need. */
static int validate_info(struct fi_info *info, uint64_t required_caps)
{
    int mr_mode;

    BTL_VERBOSE(("validating device: %s", info->domain_attr->name));

    /* we need exactly all the required bits */
    if ((info->caps & required_caps) != required_caps) {
        BTL_VERBOSE(("unsupported caps"));
        return OPAL_ERROR;
    }

    /* we need FI_EP_RDM */
    if (info->ep_attr->type != FI_EP_RDM) {
        BTL_VERBOSE(("unsupported EP type"));
        return OPAL_ERROR;
    }

    mr_mode = info->domain_attr->mr_mode;

    if (!(mr_mode == FI_MR_BASIC || mr_mode == FI_MR_SCALABLE ||
         (mr_mode & ~(FI_MR_VIRT_ADDR | FI_MR_ALLOCATED | FI_MR_PROV_KEY)) == 0)) {
        BTL_VERBOSE(("unsupported MR mode"));
        return OPAL_ERROR;
    }

    if (!(info->tx_attr->op_flags | FI_DELIVERY_COMPLETE)) {
        BTL_VERBOSE(("the endpoint tx_ctx does not support FI_DELIVERY_COMPLETE"));
        return OPAL_ERROR;
    }

    BTL_VERBOSE(("device: %s is good to go.", info->domain_attr->name));
    return OPAL_SUCCESS;
}

/* Register the MCA parameters */
static int mca_btl_ofi_component_register(void)
{
    char *msg;
    mca_btl_ofi_module_t *module = &mca_btl_ofi_module_template;

    opal_asprintf(&msg, "BTL OFI mode of operation. Valid values are: %d = One-Sided only, %d=Two-Sided only, "
                   "%d = Both one and two sided. BTL OFI is only optimized for one-sided communication",
                   MCA_BTL_OFI_MODE_ONE_SIDED,
                   MCA_BTL_OFI_MODE_TWO_SIDED,
                   MCA_BTL_OFI_MODE_FULL_SUPPORT);
    if (NULL == msg) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    mca_btl_ofi_component.mode = MCA_BTL_OFI_MODE_ONE_SIDED;
    (void)mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "mode",
                                          msg,
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_ofi_component.mode);

    /* fi_getinfo with prov_name == NULL means ALL provider.
     * Since now we are using the first valid info returned, I'm not sure
     * if we need to provide the support for comma limited provider list. */
    prov_include = NULL;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "provider_include",
                                          "OFI provider that ofi btl will query for. This parameter only "
                                          "accept ONE provider name. "
                                          "(e.g., \"psm2\"; an empty value means that all providers will "
                                          "be considered.",
                                          MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                          OPAL_INFO_LVL_4,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &prov_include);

    mca_btl_ofi_component.num_cqe_read = MCA_BTL_OFI_NUM_CQE_READ;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "num_cq_read",
                                          "Number of completion entries to read from a single cq_read. ",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_ofi_component.num_cqe_read);

    ofi_progress_mode = "unspec";
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "progress_mode",
                                          "requested provider progress mode. [unspec, auto, manual]"
                                          "(default: unspec)",
                                          MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &ofi_progress_mode);

    mca_btl_ofi_component.num_contexts_per_module = 1;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "num_contexts_per_module",
                                          "number of communication context per module to create. "
                                          "This should increase multithreaded performance but it is "
                                          "advised that this number should be lower than total cores.",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_ofi_component.num_contexts_per_module);

    disable_sep = false;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "disable_sep",
                                          "force btl/ofi to never use scalable endpoint.",
                                          MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &disable_sep);

    mca_btl_ofi_component.progress_threshold = MCA_BTL_OFI_DEFAULT_PROGRESS_THRESHOLD;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "progress_threshold",
                                          "number of outstanding operation before btl will progress "
                                          "automatically. Tuning this might improve performance on "
                                          "certain type of application.",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_ofi_component.progress_threshold);

    mca_btl_ofi_component.rd_num = MCA_BTL_OFI_DEFAULT_RD_NUM;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                          "rd_num",
                                          "Number of receive descriptor posted per context.",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                          OPAL_INFO_LVL_5,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &mca_btl_ofi_component.rd_num);


    /* for now we want this component to lose to the MTL. */
    module->super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH - 50;

    return mca_btl_base_param_register (&mca_btl_ofi_component.super.btl_version,
                                        &module->super);
}

static int mca_btl_ofi_component_open(void)
{
    mca_btl_ofi_component.module_count = 0;
    return OPAL_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int mca_btl_ofi_component_close(void)
{
    /* If we don't sleep, sockets provider freaks out. */
    sleep(1);
    return OPAL_SUCCESS;
}

void mca_btl_ofi_exit(void)
{
    BTL_ERROR(("BTL OFI will now abort."));
    exit(1);
}

/*
 *  OFI component initialization:
 *   read interface list from kernel and compare against component parameters
 *   then create a BTL instance for selected interfaces
 */

static mca_btl_base_module_t **mca_btl_ofi_component_init (int *num_btl_modules, bool enable_progress_threads,
                                                           bool enable_mpi_threads)
{
    /* for this BTL to be useful the interface needs to support RDMA and certain atomic operations */
    int rc;
    uint64_t progress_mode;
    unsigned resource_count = 0;
    struct mca_btl_base_module_t **base_modules;

    BTL_VERBOSE(("initializing ofi btl"));

    /* Set up libfabric hints. */
    uint32_t libfabric_api;
    libfabric_api = fi_version();

    /* bail if OFI version is less than 1.5. */
    if (libfabric_api < FI_VERSION(1, 5)) {
        BTL_VERBOSE(("ofi btl disqualified because OFI version < 1.5."));
        return NULL;
    }

    struct fi_info *info, *info_list;
    struct fi_info hints = {0};
    struct fi_ep_attr ep_attr = {0};
    struct fi_rx_attr rx_attr = {0};
    struct fi_tx_attr tx_attr = {0};
    struct fi_fabric_attr fabric_attr = {0};
    struct fi_domain_attr domain_attr = {0};
    uint64_t required_caps;

    switch (mca_btl_ofi_component.mode) {

        case MCA_BTL_OFI_MODE_TWO_SIDED:
            mca_btl_ofi_component.two_sided_enabled = true;
            required_caps = MCA_BTL_OFI_TWO_SIDED_REQUIRED_CAPS;
            break;

        case MCA_BTL_OFI_MODE_FULL_SUPPORT:
            mca_btl_ofi_component.two_sided_enabled = true;
            required_caps = MCA_BTL_OFI_ONE_SIDED_REQUIRED_CAPS |
                            MCA_BTL_OFI_TWO_SIDED_REQUIRED_CAPS;
            break;

        default:
            /* default to only one sided. */
            required_caps = MCA_BTL_OFI_ONE_SIDED_REQUIRED_CAPS;
            break;
    }

    /* Select the provider */
    fabric_attr.prov_name = prov_include;

    domain_attr.mr_mode = MCA_BTL_OFI_REQUESTED_MR_MODE;

    /* message progression mode. */
    if (!strcmp(ofi_progress_mode, "auto")) {
        progress_mode = FI_PROGRESS_AUTO;
    } else if (!strcmp(ofi_progress_mode, "manual")) {
        progress_mode = FI_PROGRESS_MANUAL;
    } else {
        progress_mode = FI_PROGRESS_UNSPEC;
    }

    domain_attr.control_progress = progress_mode;
    domain_attr.data_progress = progress_mode;

    /* select endpoint type */
    ep_attr.type = FI_EP_RDM;

    /* ask for capabilities */
    /* TODO: catch the caps here. */
    hints.caps = required_caps;
    hints.mode = FI_CONTEXT;

    /* Ask for completion context */
    hints.mode = FI_CONTEXT;

    hints.fabric_attr = &fabric_attr;
    hints.domain_attr = &domain_attr;
    hints.ep_attr = &ep_attr;
    hints.tx_attr = &tx_attr;
    hints.rx_attr = &rx_attr;

    /* for now */
    tx_attr.iov_limit = 1;
    rx_attr.iov_limit = 1;

    tx_attr.op_flags = FI_DELIVERY_COMPLETE;

    mca_btl_ofi_component.module_count = 0;

    /* do the query. */
    rc = fi_getinfo(FI_VERSION(1, 5), NULL, NULL, 0, &hints, &info_list);
    if (0 != rc) {
        BTL_VERBOSE(("fi_getinfo failed with code %d: %s",rc, fi_strerror(-rc)));
        return NULL;
    }

    /* count the number of resources/ */
    info = info_list;
    while(info) {
        resource_count++;
        info = info->next;
    }
    BTL_VERBOSE(("ofi btl found %d possible resources.", resource_count));

    info = info_list;

    while(info) {
        rc = validate_info(info, required_caps);
        if (OPAL_SUCCESS == rc) {
            /* Device passed sanity check, let's make a module.
             * We only pick the first device we found valid */
            rc = mca_btl_ofi_init_device(info);
            if (OPAL_SUCCESS == rc)
                break;
        }
        info = info->next;
    }

    /* We are done with the returned info. */
    fi_freeinfo(info_list);

    /* pass module array back to caller */
    base_modules = calloc (mca_btl_ofi_component.module_count, sizeof (*base_modules));
    if (NULL == base_modules) {
        return NULL;
    }

    memcpy(base_modules, mca_btl_ofi_component.modules,
           mca_btl_ofi_component.module_count *sizeof (mca_btl_ofi_component.modules[0]));

    BTL_VERBOSE(("ofi btl initialization complete. found %d suitable transports",
                 mca_btl_ofi_component.module_count));

    *num_btl_modules = mca_btl_ofi_component.module_count;

    return base_modules;
}

static int mca_btl_ofi_init_device(struct fi_info *info)
{
    int rc;
    int *module_count = &mca_btl_ofi_component.module_count;
    size_t namelen;
    size_t num_contexts_to_create;

    char *linux_device_name;
    char ep_name[FI_NAME_MAX];

    struct fi_info *ofi_info;
    struct fi_ep_attr *ep_attr;
    struct fi_domain_attr *domain_attr;
    struct fi_av_attr av_attr = {0};
    struct fid_fabric *fabric = NULL;
    struct fid_domain *domain = NULL;
    struct fid_ep *ep = NULL;
    struct fid_av *av = NULL;

    mca_btl_ofi_module_t *module;

    module = mca_btl_ofi_module_alloc(mca_btl_ofi_component.mode);
    if (NULL == module) {
        BTL_VERBOSE(("failed allocating ofi module"));
        goto fail;
    }

    /* If the user ask for two sided support, something bad is happening
     * to the MTL, so we will take maximum priority to supersede the MTL. */
    module->super.btl_exclusivity    = MCA_BTL_EXCLUSIVITY_DEFAULT;

    /* make a copy of the given info to store on the module */
    ofi_info = fi_dupinfo(info);
    ep_attr = ofi_info->ep_attr;
    domain_attr = ofi_info->domain_attr;

    linux_device_name = info->domain_attr->name;
    BTL_VERBOSE(("initializing dev:%s provider:%s",
                    linux_device_name,
                    info->fabric_attr->prov_name));

    /* fabric */
    rc = fi_fabric(ofi_info->fabric_attr, &fabric, NULL);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_fabric with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto fail;
    }

    /* domain */
    rc = fi_domain(fabric, ofi_info, &domain, NULL);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_domain with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto fail;
    }

    /* AV */
    av_attr.type = FI_AV_MAP;
    rc = fi_av_open(domain, &av_attr, &av, NULL);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_av_open with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto fail;
    }

    num_contexts_to_create = mca_btl_ofi_component.num_contexts_per_module;

    /* If the domain support scalable endpoint. */
    if (domain_attr->max_ep_tx_ctx > 1 && !disable_sep) {

        BTL_VERBOSE(("btl/ofi using scalable endpoint."));

        if (num_contexts_to_create > domain_attr->max_ep_tx_ctx) {
            BTL_VERBOSE(("cannot create requested %u contexts. (node max=%zu)",
                            module->num_contexts,
                            domain_attr->max_ep_tx_ctx));
            goto fail;
         }

        /* modify the info to let the provider know we are creating x contexts */
        ep_attr->tx_ctx_cnt = num_contexts_to_create;
        ep_attr->rx_ctx_cnt = num_contexts_to_create;

        /* create scalable endpoint */
        rc = fi_scalable_ep(domain, ofi_info, &ep, NULL);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_scalable_ep with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto fail;
        }

        module->num_contexts = num_contexts_to_create;
        module->is_scalable_ep = true;

        /* create contexts */
        module->contexts = mca_btl_ofi_context_alloc_scalable(ofi_info,
                                domain, ep, av,
                                num_contexts_to_create);

   } else {
        /* warn the user if they want more than 1 context */
        if (num_contexts_to_create > 1) {
            BTL_ERROR(("cannot create %zu contexts as the provider does not support "
                        "scalable endpoint. Falling back to single context endpoint.",
                        num_contexts_to_create));
        }

        BTL_VERBOSE(("btl/ofi using normal endpoint."));

        rc = fi_endpoint(domain, ofi_info, &ep, NULL);
        if (0 != rc) {
            BTL_VERBOSE(("%s failed fi_endpoint with err=%s",
                            linux_device_name,
                            fi_strerror(-rc)
                            ));
            goto fail;
        }

        module->num_contexts = 1;
        module->is_scalable_ep = false;

        /* create contexts */
        module->contexts = mca_btl_ofi_context_alloc_normal(ofi_info,
                                                            domain, ep, av);
    }

    if (NULL == module->contexts) {
        /* error message is already printed */
        goto fail;
    }

    /* enable the endpoint for using */
    rc = fi_enable(ep);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_enable with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto fail;
    }

    /* Everything succeeded, lets create a module for this device. */
    /* store the information. */
    module->fabric_info = ofi_info;
    module->fabric = fabric;
    module->domain = domain;
    module->av = av;
    module->ofi_endpoint = ep;
    module->linux_device_name = linux_device_name;
    module->outstanding_rdma = 0;
    module->use_virt_addr = false;

    if (ofi_info->domain_attr->mr_mode == FI_MR_BASIC ||
        ofi_info->domain_attr->mr_mode & FI_MR_VIRT_ADDR) {
        module->use_virt_addr = true;
    }

    /* initialize the rcache */
    mca_btl_ofi_rcache_init(module);

    /* create endpoint list */
    OBJ_CONSTRUCT(&module->endpoints, opal_list_t);
    OBJ_CONSTRUCT(&module->module_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->id_to_endpoint, opal_hash_table_t);

    rc = opal_hash_table_init (&module->id_to_endpoint, 512);
    if (OPAL_SUCCESS != rc) {
        BTL_ERROR(("error initializing hash table."));
        goto fail;
    }

    /* create and send the modex for this device */
    namelen = sizeof(ep_name);
    rc = fi_getname((fid_t)ep, &ep_name[0], &namelen);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_getname with err=%s",
                        linux_device_name,
                        fi_strerror(-rc)
                        ));
        goto fail;
    }


    /* If we have two-sided support. */
    if (TWO_SIDED_ENABLED) {

        /* post wildcard recvs */
        for (int i=0; i < module->num_contexts; i++) {
            rc = mca_btl_ofi_post_recvs((mca_btl_base_module_t*) module,
                                        &module->contexts[i],
                                        mca_btl_ofi_component.rd_num);
            if (OPAL_SUCCESS != rc) {
                goto fail;
            }
        }
    }

    /* post our endpoint name so peer can use it to connect to us */
    OPAL_MODEX_SEND(rc,
                    OPAL_PMIX_GLOBAL,
                    &mca_btl_ofi_component.super.btl_version,
                    &ep_name,
                    namelen);
    mca_btl_ofi_component.namelen = namelen;

    /* add this module to the list */
    mca_btl_ofi_component.modules[(*module_count)++] = module;

    return OPAL_SUCCESS;

fail:
    /* clean up */

    /* if the contexts have not been initiated, num_contexts should
     * be zero and we skip this. */
    for (int i=0; i < module->num_contexts; i++) {
        mca_btl_ofi_context_finalize(&module->contexts[i], module->is_scalable_ep);
    }
    free(module->contexts);

    if (NULL != av) {
        fi_close(&av->fid);
    }

    if (NULL != ep) {
        fi_close(&ep->fid);
    }

    if (NULL != domain) {
        fi_close(&domain->fid);
    }

    if (NULL != fabric) {
        fi_close(&fabric->fid);
    }
    free(module);

    /* not really a failure. just skip this device. */
    return OPAL_ERR_OUT_OF_RESOURCE;
}

/**
 * @brief OFI BTL progress function
 *
 * This function explictly progresses all workers.
 */
static int mca_btl_ofi_component_progress (void)
{
    int events = 0;
    mca_btl_ofi_context_t *context;

    for (int i = 0 ; i < mca_btl_ofi_component.module_count ; ++i) {
        mca_btl_ofi_module_t *module = mca_btl_ofi_component.modules[i];

        /* progress context we own first. */
        context = get_ofi_context(module);

        if (mca_btl_ofi_context_trylock(context)) {
            events += mca_btl_ofi_context_progress(context);
            mca_btl_ofi_context_unlock(context);
        }

        /* if there is nothing to do, try progress other's. */
        if (events == 0) {
            for (int j = 0 ; j < module->num_contexts ; j++ ) {

                context = get_ofi_context_rr(module);

                if (mca_btl_ofi_context_trylock(context)) {
                    events += mca_btl_ofi_context_progress(context);
                    mca_btl_ofi_context_unlock(context);
                }

                /* If we did something, good enough. return now.
                 * This is crucial for performance/latency. */
                if (events > 0) {
                    break;
                }
            }
        }
    }

    return events;
}

/** OFI btl component */
mca_btl_ofi_component_t mca_btl_ofi_component = {
    .super = {
        .btl_version = {
            MCA_BTL_DEFAULT_VERSION("ofi"),
            .mca_open_component = mca_btl_ofi_component_open,
            .mca_close_component = mca_btl_ofi_component_close,
            .mca_register_component_params = mca_btl_ofi_component_register,
        },
        .btl_data = {
            /* The component is not checkpoint ready */
            .param_field = MCA_BASE_METADATA_PARAM_NONE
        },

        .btl_init = mca_btl_ofi_component_init,
        .btl_progress = mca_btl_ofi_component_progress,
    },
};
