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
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2018-2021 Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2020-2023 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/argv.h"
#include "opal/util/printf.h"

#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/common/ofi/common_ofi.h"
#include "opal/mca/hwloc/base/base.h"

#include <string.h>

#include "btl_ofi.h"
#include "btl_ofi_endpoint.h"
#include "btl_ofi_frag.h"
#include "btl_ofi_rdma.h"

#define MCA_BTL_OFI_ONE_SIDED_REQUIRED_CAPS (FI_RMA | FI_ATOMIC)
#define MCA_BTL_OFI_TWO_SIDED_REQUIRED_CAPS (FI_MSG)

#define MCA_BTL_OFI_REQUESTED_MR_MODE (FI_MR_ALLOCATED | FI_MR_PROV_KEY | FI_MR_VIRT_ADDR | FI_MR_ENDPOINT)

static char *ofi_progress_mode;
static bool disable_sep;
static int mca_btl_ofi_init_device(struct fi_info *info);

/* validate information returned from fi_getinfo().
 * return OPAL_ERROR if we dont have what we need. */
static int validate_info(struct fi_info *info, uint64_t required_caps, char **include_list,
                         char **exclude_list)
{
    int mr_mode;

    if (NULL != include_list
        && !opal_common_ofi_is_in_list(include_list, info->fabric_attr->prov_name)) {
        opal_output_verbose(1, opal_common_ofi.output,
                            "%s:%d: btl:ofi: \"%s\" not in include list\n", __FILE__, __LINE__,
                            info->fabric_attr->prov_name);
        return OPAL_ERROR;
    } else if (NULL != exclude_list
               && opal_common_ofi_is_in_list(exclude_list, info->fabric_attr->prov_name)) {
        opal_output_verbose(1, opal_common_ofi.output, "%s:%d: btl:ofi: \"%s\" in exclude list\n",
                            __FILE__, __LINE__, info->fabric_attr->prov_name);
        return OPAL_ERROR;
    }

    BTL_VERBOSE(("validating device: %s", info->domain_attr->name));

    /* EFA does not fulfill FI_DELIVERY_COMPLETE requirements in prior libfabric
     * versions. The prov version is set as:
     * FI_VERSION(FI_MAJOR_VERSION * 100 + FI_MINOR_VERSION, FI_REVISION_VERSION * 10)
     * Thus, FI_VERSION(112,0) corresponds to libfabric 1.12.0
     */
    if (!strncasecmp(info->fabric_attr->prov_name, "efa", 3)
        && FI_VERSION_LT(info->fabric_attr->prov_version, FI_VERSION(112, 0))) {
        BTL_VERBOSE(("unsupported libfabric efa version"));
        return OPAL_ERROR;
    }

    /* ofi_rxm does not fulfill FI_DELIVERY_COMPLETE requirements. Thus we
     * exclude it if it's detected.
     */
    if (strstr(info->fabric_attr->prov_name, "ofi_rxm")) {
        BTL_VERBOSE(("ofi_rxm does not support FI_DELIVERY_COMPLETE"));
        return OPAL_ERROR;
    }

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

    if (!(mr_mode == FI_MR_BASIC || mr_mode == FI_MR_SCALABLE
#if defined(FI_MR_HMEM)
          || (mr_mode & ~(FI_MR_VIRT_ADDR | FI_MR_ALLOCATED | FI_MR_PROV_KEY | FI_MR_ENDPOINT | FI_MR_HMEM)) == 0)) {
#else
          || (mr_mode & ~(FI_MR_VIRT_ADDR | FI_MR_ALLOCATED | FI_MR_PROV_KEY | FI_MR_ENDPOINT)) == 0)) {
#endif
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
    int ret;
    char *msg;
    mca_btl_ofi_module_t *module = &mca_btl_ofi_module_template;

    opal_asprintf(
        &msg,
        "BTL OFI mode of operation. Valid values are: %d = One-Sided only, %d=Two-Sided only, "
        "%d = Both one and two sided. BTL OFI is only optimized for one-sided communication",
        MCA_BTL_OFI_MODE_ONE_SIDED, MCA_BTL_OFI_MODE_TWO_SIDED, MCA_BTL_OFI_MODE_FULL_SUPPORT);
    if (NULL == msg) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    mca_btl_ofi_component.mode = MCA_BTL_OFI_MODE_ONE_SIDED;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version, "mode", msg,
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_btl_ofi_component.mode);

    mca_btl_ofi_component.num_cqe_read = MCA_BTL_OFI_NUM_CQE_READ;
    (void) mca_base_component_var_register(
        &mca_btl_ofi_component.super.btl_version, "num_cq_read",
        "Number of completion entries to read from a single cq_read. ", MCA_BASE_VAR_TYPE_INT, NULL,
        0, 0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY, &mca_btl_ofi_component.num_cqe_read);

    ofi_progress_mode = "unspec";
    (void)
        mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version, "progress_mode",
                                        "requested provider progress mode. [unspec, auto, manual]"
                                        "(default: unspec)",
                                        MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY, &ofi_progress_mode);

    mca_btl_ofi_component.num_contexts_per_module = 1;
    (void) mca_base_component_var_register(
        &mca_btl_ofi_component.super.btl_version, "num_contexts_per_module",
        "number of communication context per module to create. "
        "This should increase multithreaded performance but it is "
        "advised that this number should be lower than total cores.",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
        &mca_btl_ofi_component.num_contexts_per_module);

    disable_sep = false;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version, "disable_sep",
                                           "force btl/ofi to never use scalable endpoint.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY, &disable_sep);

    mca_btl_ofi_component.progress_threshold = MCA_BTL_OFI_DEFAULT_PROGRESS_THRESHOLD;
    (void)
        mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                        "progress_threshold",
                                        "number of outstanding operation before btl will progress "
                                        "automatically. Tuning this might improve performance on "
                                        "certain type of application.",
                                        MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
                                        MCA_BASE_VAR_SCOPE_READONLY,
                                        &mca_btl_ofi_component.progress_threshold);

    mca_btl_ofi_component.rd_num = MCA_BTL_OFI_DEFAULT_RD_NUM;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version, "rd_num",
                                           "Number of receive descriptor posted per context.",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_btl_ofi_component.rd_num);

    mca_btl_ofi_component.disable_inject = false;
    (void) mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version, "disable_inject",
                                           "disable use of fi_inject for short messages.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_5,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_btl_ofi_component.disable_inject);

    mca_btl_ofi_component.disable_hmem = false;
    mca_base_component_var_register(&mca_btl_ofi_component.super.btl_version,
                                    "disable_hmem",
                                    "Disable HMEM usage",
                                    MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                    OPAL_INFO_LVL_5,
                                    MCA_BASE_VAR_SCOPE_READONLY,
                                    &mca_btl_ofi_component.disable_hmem);


    /* for now we want this component to lose to the MTL. */
    module->super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH - 50;

    ret = opal_common_ofi_mca_register(&mca_btl_ofi_component.super.btl_version);
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    return mca_btl_base_param_register(&mca_btl_ofi_component.super.btl_version, &module->super);
}

static int mca_btl_ofi_component_open(void)
{
    mca_btl_ofi_component.module_count = 0;
    return opal_common_ofi_open();
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int mca_btl_ofi_component_close(void)
{
    int ret;
    ret = opal_common_ofi_close();
    /* If we don't sleep, sockets provider freaks out. Ummm this is a scary comment */
    sleep(1);
    return ret;
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

static mca_btl_base_module_t **mca_btl_ofi_component_init(int *num_btl_modules,
                                                          bool enable_progress_threads,
                                                          bool enable_mpi_threads)
{
    /* for this BTL to be useful the interface needs to support RDMA and certain atomic operations
     */
    int rc;
    uint64_t progress_mode;
    unsigned resource_count = 0;
    struct mca_btl_base_module_t **base_modules;
    char **include_list = NULL, **exclude_list = NULL;

    BTL_VERBOSE(("initializing ofi btl"));

    /* Set up libfabric hints. */
    uint32_t libfabric_api;
    libfabric_api = fi_version();

    /* bail if OFI version is less than 1.5. */
    if (libfabric_api < FI_VERSION(1, 9)) {
        BTL_VERBOSE(("ofi btl disqualified because OFI version < 1.9."));
        return NULL;
    }

    struct fi_info *info, *info_list, *selected_info;
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
        required_caps = MCA_BTL_OFI_ONE_SIDED_REQUIRED_CAPS | MCA_BTL_OFI_TWO_SIDED_REQUIRED_CAPS;
        break;

    default:
        /* default to only one sided. */
        required_caps = MCA_BTL_OFI_ONE_SIDED_REQUIRED_CAPS;
        break;
    }

    fabric_attr.prov_name = NULL;

    opal_output_verbose(1, opal_common_ofi.output, "%s:%d: btl:ofi:provider_include = \"%s\"\n",
                        __FILE__, __LINE__, *opal_common_ofi.prov_include);
    opal_output_verbose(1, opal_common_ofi.output, "%s:%d: btl:ofi:provider_exclude = \"%s\"\n",
                        __FILE__, __LINE__, *opal_common_ofi.prov_exclude);

    if (NULL != *opal_common_ofi.prov_include) {
        include_list = opal_argv_split(*opal_common_ofi.prov_include, ',');
    } else if (NULL != *opal_common_ofi.prov_exclude) {
        exclude_list = opal_argv_split(*opal_common_ofi.prov_exclude, ',');
    }

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

    /* Ask for completion context */
    hints.mode = FI_CONTEXT | FI_CONTEXT2;

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

#if defined(FI_HMEM)
    /* Request device transfer capabilities, separate from required_caps */
    if (false == mca_btl_ofi_component.disable_hmem) {
        hints.caps |= FI_HMEM;
        hints.domain_attr->mr_mode |= FI_MR_HMEM;
    }
no_hmem:
#endif

    /* Do the query. The earliest version that supports FI_HMEM hints is 1.9.
     * The earliest version the explictly allow provider to call CUDA API is 1.18  */
    rc = fi_getinfo(FI_VERSION(1, 18), NULL, NULL, 0, &hints, &info_list);
    if (FI_ENOSYS == -rc) {
	rc = fi_getinfo(FI_VERSION(1, 9), NULL, NULL, 0, &hints, &info_list);
    }
    if (0 != rc) {
#if defined(FI_HMEM)
        if (hints.caps & FI_HMEM) {
            /* Try again without FI_HMEM hints */
            hints.caps &= ~FI_HMEM;
            hints.domain_attr->mr_mode &= ~FI_MR_HMEM;
            goto no_hmem;
        }
#endif
        BTL_VERBOSE(("fi_getinfo failed with code %d: %s", rc, fi_strerror(-rc)));
        if (NULL != include_list) {
            opal_argv_free(include_list);
        }
        return NULL;
    }

#if defined(FI_HMEM)
    /* If we get to this point with FI_HMEM hint set, we want it to be a
     * required capability
     */
    if (hints.caps & FI_HMEM) {
        /* The EFA provider has a bug where it incorrectly advertises FI_HMEM +
         * FI_ATOMIC capability without being able to provide that support in
         * versions before libfabric 1.18.0
         */
        if (libfabric_api < FI_VERSION(1, 18) && !strncasecmp(info_list->fabric_attr->prov_name, "efa", 3)) {
            hints.caps &= ~FI_HMEM;
            hints.domain_attr->mr_mode &= ~FI_MR_HMEM;
            goto no_hmem;
        }
        required_caps |= FI_HMEM;
    }
#endif

    /* count the number of resources/ */
    info = info_list;
    while (info) {
        resource_count++;
        info = info->next;
    }
    BTL_VERBOSE(("ofi btl found %d possible resources.", resource_count));

    info = info_list;

    while (info) {
        rc = validate_info(info, required_caps, include_list, exclude_list);
        if (OPAL_SUCCESS == rc) {
            /* Device passed sanity check, let's make a module.
             *
             * The initial fi_getinfo() call will return a list of providers
             * available for this process. once a provider is selected from the
             * list, we will cycle through the remaining list to identify NICs
             * serviced by this provider, and try to pick one on the same NUMA
             * node as this process. If there are no NICs on the same NUMA node,
             * we pick one in a manner which allows all ranks to make balanced
             * use of available NICs on the system.
             *
             * Most providers give a separate fi_info object for each NIC,
             * however some may have multiple info objects with different
             * attributes for the same NIC. The initial provider attributes
             * are used to ensure that all NICs we return provide the same
             * capabilities as the initial one.
             */
            selected_info = opal_common_ofi_select_provider(info, &opal_process_info);
            rc = mca_btl_ofi_init_device(selected_info);
            if (OPAL_SUCCESS == rc) {
                info = selected_info;
                break;
            }
        }
        info = info->next;
    }

    /* We are done with the returned info. */
    fi_freeinfo(info_list);
    if (NULL != include_list) {
        opal_argv_free(include_list);
    }

    /* pass module array back to caller */
    base_modules = calloc(mca_btl_ofi_component.module_count, sizeof(*base_modules));
    if (NULL == base_modules) {
        return NULL;
    }

    memcpy(base_modules, mca_btl_ofi_component.modules,
           mca_btl_ofi_component.module_count * sizeof(mca_btl_ofi_component.modules[0]));

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
    void *ep_name = NULL;

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
    module->super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_DEFAULT;

    /* make a copy of the given info to store on the module */
    ofi_info = fi_dupinfo(info);
    ep_attr = ofi_info->ep_attr;
    domain_attr = ofi_info->domain_attr;

    /* mtl_btl_ofi_rcache_init() initializes patcher which should only
     * take place things are single threaded.  OFI providers may start
     * spawn threads, so initialize the rcache before creating OFI objects
     * to prevent races. */
    mca_btl_ofi_rcache_init(module);

    /* for similar reasons to the rcache call, this must be called
     * during single threaded part of the code and before Libfabric
     * configures its memory monitors.  Easiest to do that before
     * domain open.  Silently ignore not-supported errors, as they
     * are not critical to program correctness, but only indicate
     * that LIbfabric will have to pick a different, possibly less
     * optimal, monitor. */
    rc = opal_common_ofi_export_memory_monitor();
    if (0 != rc && -FI_ENOSYS != rc) {
        BTL_VERBOSE(("Failed to inject Libfabric memory monitor: %s",
                     fi_strerror(-rc)));
    }

    linux_device_name = info->domain_attr->name;
    BTL_VERBOSE(
        ("initializing dev:%s provider:%s", linux_device_name, info->fabric_attr->prov_name));

    /* fabric */
    rc = fi_fabric(ofi_info->fabric_attr, &fabric, NULL);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_fabric with err=%s", linux_device_name, fi_strerror(-rc)));
        goto fail;
    }

    /* domain */
    rc = fi_domain(fabric, ofi_info, &domain, NULL);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_domain with err=%s", linux_device_name, fi_strerror(-rc)));
        goto fail;
    }

    /**
     * Save the maximum sizes.
     */
    mca_btl_ofi_component.max_inject_size = ofi_info->tx_attr->inject_size;

    /* AV */
    av_attr.type = FI_AV_MAP;
    rc = fi_av_open(domain, &av_attr, &av, NULL);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_av_open with err=%s", linux_device_name, fi_strerror(-rc)));
        goto fail;
    }

    num_contexts_to_create = mca_btl_ofi_component.num_contexts_per_module;

    /* If the domain support scalable endpoint. */
    if (domain_attr->max_ep_tx_ctx > 1 && !disable_sep) {

        BTL_VERBOSE(("btl/ofi using scalable endpoint."));

        if (num_contexts_to_create > domain_attr->max_ep_tx_ctx) {
            BTL_VERBOSE(("cannot create requested %u contexts. (node max=%zu)",
                         module->num_contexts, domain_attr->max_ep_tx_ctx));
            goto fail;
        }

        /* modify the info to let the provider know we are creating x contexts */
        ep_attr->tx_ctx_cnt = num_contexts_to_create;
        ep_attr->rx_ctx_cnt = num_contexts_to_create;

        /* create scalable endpoint */
        rc = fi_scalable_ep(domain, ofi_info, &ep, NULL);
        if (0 != rc) {
            BTL_VERBOSE(
                ("%s failed fi_scalable_ep with err=%s", linux_device_name, fi_strerror(-rc)));
            goto fail;
        }

        module->num_contexts = num_contexts_to_create;
        module->is_scalable_ep = true;

        /* create contexts */
        module->contexts = mca_btl_ofi_context_alloc_scalable(ofi_info, domain, ep, av,
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
            BTL_VERBOSE(("%s failed fi_endpoint with err=%s", linux_device_name, fi_strerror(-rc)));
            goto fail;
        }

        module->num_contexts = 1;
        module->is_scalable_ep = false;

        /* create contexts */
        module->contexts = mca_btl_ofi_context_alloc_normal(ofi_info, domain, ep, av);
    }

    if (NULL == module->contexts) {
        /* error message is already printed */
        goto fail;
    }

    /* enable the endpoint for using */
    rc = fi_enable(ep);
    if (0 != rc) {
        BTL_VERBOSE(("%s failed fi_enable with err=%s", linux_device_name, fi_strerror(-rc)));
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
    module->use_fi_mr_bind = false;
    module->bypass_cache = false;

#if defined(FI_HMEM)
    if (ofi_info->caps & FI_HMEM) {
        module->super.btl_flags |= MCA_BTL_FLAGS_ACCELERATOR_RDMA;
    }
#endif

    if (ofi_info->domain_attr->mr_mode == FI_MR_BASIC
        || ofi_info->domain_attr->mr_mode & FI_MR_VIRT_ADDR) {
        module->use_virt_addr = true;
    }

    if (ofi_info->domain_attr->mr_mode & FI_MR_ENDPOINT) {
        module->use_fi_mr_bind = true;
    }

    /* Currently there is no API to query whether the libfabric provider
     * uses an underlying registration cache. For now, just check for known
     * providers that use registration caching. */
    if (!strncasecmp(info->fabric_attr->prov_name, "efa", 3)) {
        module->bypass_cache = true;
    }

    /* create endpoint list */
    OBJ_CONSTRUCT(&module->endpoints, opal_list_t);
    OBJ_CONSTRUCT(&module->module_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->id_to_endpoint, opal_hash_table_t);

    rc = opal_hash_table_init(&module->id_to_endpoint, 512);
    if (OPAL_SUCCESS != rc) {
        BTL_ERROR(("error initializing hash table."));
        goto fail;
    }

    rc = opal_common_ofi_fi_getname((fid_t)ep,
                                     &ep_name,
                                     &namelen);
    if (OPAL_SUCCESS != rc) {
        BTL_VERBOSE(("%s failed opal_common_ofi_fi_getname  with err=%d", linux_device_name, rc));
        goto fail;
    }

    if (TWO_SIDED_ENABLED) {

        /* post wildcard recvs */
        for (int i = 0; i < module->num_contexts; i++) {
            rc = mca_btl_ofi_post_recvs((mca_btl_base_module_t *) module, &module->contexts[i],
                                        mca_btl_ofi_component.rd_num);
            if (OPAL_SUCCESS != rc) {
                goto fail;
            }
        }
    }

    /* post our endpoint name so peer can use it to connect to us */
    OPAL_MODEX_SEND(rc, PMIX_GLOBAL, &mca_btl_ofi_component.super.btl_version, ep_name, namelen);
    mca_btl_ofi_component.namelen = namelen;
    free(ep_name);

    /* add this module to the list */
    mca_btl_ofi_component.modules[(*module_count)++] = module;

    return OPAL_SUCCESS;

fail:
    /* clean up */

    /* close basic ep before closing av */
    if (NULL != ep && !module->is_scalable_ep) {
        fi_close(&ep->fid);
        ep = NULL;
    }

    /* if the contexts have not been initiated, num_contexts should
     * be zero and we skip this. */
    if (NULL != module->contexts) {
        for (int i = 0; i < module->num_contexts; i++) {
            mca_btl_ofi_context_finalize(&module->contexts[i], module->is_scalable_ep);
        }
    }
    free(module->contexts);

    /* check for NULL ep to avoid double-close */
    if (NULL != ep) {
        fi_close(&ep->fid);
    }

    /* close av after closing basic ep */
    if (NULL != av) {
        fi_close(&av->fid);
    }

    if (NULL != domain) {
        fi_close(&domain->fid);
    }

    if (NULL != fabric) {
        fi_close(&fabric->fid);
    }
    free(module);

    if (NULL != ep_name) {
        free(ep_name);
    }

    /* not really a failure. just skip this device. */
    return OPAL_ERR_OUT_OF_RESOURCE;
}

/**
 * @brief OFI BTL progress function
 *
 * This function explicitly progresses all workers.
 */
static int mca_btl_ofi_component_progress(void)
{
    int events = 0;
    mca_btl_ofi_context_t *context;

    for (int i = 0; i < mca_btl_ofi_component.module_count; ++i) {
        mca_btl_ofi_module_t *module = mca_btl_ofi_component.modules[i];

        /* progress context we own first. */
        context = get_ofi_context(module);

        if (mca_btl_ofi_context_trylock(context)) {
            events += mca_btl_ofi_context_progress(context);
            mca_btl_ofi_context_unlock(context);
        }

        /* if there is nothing to do, try progress other's. */
        if (events == 0) {
            for (int j = 0; j < module->num_contexts; j++) {

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
    .super =
        {
            .btl_version =
                {
                    MCA_BTL_DEFAULT_VERSION("ofi"),
                    .mca_open_component = mca_btl_ofi_component_open,
                    .mca_close_component = mca_btl_ofi_component_close,
                    .mca_register_component_params = mca_btl_ofi_component_register,
                },
            .btl_data =
                {/* The component is not checkpoint ready */
                 .param_field = MCA_BASE_METADATA_PARAM_NONE},

            .btl_init = mca_btl_ofi_component_init,
            .btl_progress = mca_btl_ofi_component_progress,
        },
};
