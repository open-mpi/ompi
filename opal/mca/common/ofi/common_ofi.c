/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2020      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include <errno.h>
#include <unistd.h>

#include "common_ofi.h"
#include "opal_config.h"
#include "opal/constants.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/hwloc/base/base.h"

OPAL_DECLSPEC opal_common_ofi_module_t opal_common_ofi = {
    .prov_include = NULL,
    .prov_exclude = NULL,
    .registered = 0,
    .verbose = 0
};

static const char default_prov_exclude_list[] = "shm,sockets,tcp,udp,rstream";

OPAL_DECLSPEC int opal_common_ofi_register_mca_variables(const mca_base_component_t *component)
{
    static int registered = 0;
    static int include_index;
    static int exclude_index;
    static int verbose_index;

    if (fi_version() < FI_VERSION(1,0)) {
        return OPAL_ERROR;
    }

    if (!registered) {
        /*
         * this monkey business is needed because of the way the MCA VARs stuff tries to handle pointers to strings when
         * when destructing the MCA var database.  If you don't do something like this,the MCA var framework will try
         * to dereference a pointer which itself is no longer a valid address owing to having been previously dlclosed.
         */
         opal_common_ofi.prov_include = (char **)malloc(sizeof(char *));
         *opal_common_ofi.prov_include = NULL;
         include_index = mca_base_var_register("opal", "opal_common", "ofi",
                               "provider_include",
                               "Comma-delimited list of OFI providers that are considered for use (e.g., \"psm,psm2\"; an empty value means that all providers will be considered). Mutually exclusive with mtl_ofi_provider_exclude.",
                               MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                               OPAL_INFO_LVL_1,
                               MCA_BASE_VAR_SCOPE_READONLY,
                               opal_common_ofi.prov_include);
        opal_common_ofi.prov_exclude = (char **)malloc(sizeof(char *));
        *opal_common_ofi.prov_exclude = strdup(default_prov_exclude_list);
        exclude_index = mca_base_var_register("opal", "opal_common", "ofi",
                              "provider_exclude",
                              "Comma-delimited list of OFI providers that are not considered for use (default: \"sockets,mxm\"; empty value means that all providers will be considered). Mutually exclusive with mtl_ofi_provider_include.",
                              MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                              OPAL_INFO_LVL_1,
                              MCA_BASE_VAR_SCOPE_READONLY,
                              opal_common_ofi.prov_exclude);
        verbose_index = mca_base_var_register("opal", "opal_common", "ofi", "verbose",
                                              "Verbose level of the OFI components",
                                              MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                              MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                              MCA_BASE_VAR_SCOPE_LOCAL,
                                              &opal_common_ofi.verbose);
        registered = 1;
    }

    if (component) {
        mca_base_var_register_synonym(include_index, component->mca_project_name,
                                      component->mca_type_name,
                                      component->mca_component_name,
                                      "provider_include", 0);
        mca_base_var_register_synonym(exclude_index, component->mca_project_name,
                                      component->mca_type_name,
                                      component->mca_component_name,
                                      "provider_exclude", 0);
        mca_base_var_register_synonym(verbose_index, component->mca_project_name,
                                      component->mca_type_name,
                                      component->mca_component_name,
                                      "verbose", 0);
    }

    return OPAL_SUCCESS;
}

OPAL_DECLSPEC void opal_common_ofi_mca_register(void)
{
    opal_common_ofi.registered++;
    if (opal_common_ofi.registered > 1) {
         opal_output_set_verbosity(opal_common_ofi.output, opal_common_ofi.verbose);
        return;
    }

    opal_common_ofi.output = opal_output_open(NULL);
    opal_output_set_verbosity(opal_common_ofi.output, opal_common_ofi.verbose);
}

OPAL_DECLSPEC void opal_common_ofi_mca_deregister(void)
{
    /* unregister only on last deregister */
    opal_common_ofi.registered--;
    assert(opal_common_ofi.registered >= 0);
    if (opal_common_ofi.registered) {
        return;
    }
    opal_output_close(opal_common_ofi.output);
}

/* check that the tx attributes match */
static int
check_tx_attr(struct fi_tx_attr *provider_info,
              struct fi_tx_attr *provider)
{
    if (!(provider->msg_order & ~(provider_info->msg_order)) &&
        !(provider->op_flags & ~(provider_info->op_flags)) &&
        (provider->inject_size == provider_info->inject_size)) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

/* check that the rx attributes match */
static int
check_rx_attr(struct fi_rx_attr *provider_info,
              struct fi_rx_attr *provider)
{
    if (!(provider->msg_order & ~(provider_info->msg_order)) &&
        !(provider->op_flags & ~(provider_info->op_flags))) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

/* check that the ep attributes match */
static int
check_ep_attr(struct fi_ep_attr *provider_info,
              struct fi_ep_attr *provider)
{
    if (!(provider->type & ~(provider_info->type)) &&
        !(provider->mem_tag_format & ~(provider_info->mem_tag_format)) &&
        (provider->max_msg_size == provider_info->max_msg_size) &&
        (provider->tx_ctx_cnt == provider_info->tx_ctx_cnt) &&
        (provider->rx_ctx_cnt == provider_info->rx_ctx_cnt)) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

/* check that the provider attributes match */
static int
check_provider_attr(struct fi_info *provider_info,
                    struct fi_info *provider)
{
    /* make sure both info are the same provider and provide the same attributes */
    if (0 == strcmp(provider_info->fabric_attr->prov_name, provider->fabric_attr->prov_name) &&
        !check_tx_attr(provider_info->tx_attr, provider->tx_attr) &&
        !check_rx_attr(provider_info->rx_attr, provider->rx_attr) &&
        !check_ep_attr(provider_info->ep_attr, provider->ep_attr) &&
        !(provider_info->caps & ~(provider->caps)) &&
        !(provider_info->mode & ~(provider->mode))) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

#if OPAL_OFI_PCI_DATA_AVAILABLE
/* Check if a process and a pci device share the same cpuset
 *     @param (IN) pci              struct fi_pci_attr pci device attributes,
 *                                  used to find hwloc object for device.
 *
 *     @param (IN) topology         hwloc_topology_t topology to get the cpusets
 *                                  from
 *
 *     @param (OUT)                 returns true if cpusets match and false if
 *                                  cpusets do not match or an error prevents comparison
 *
 *     Uses a pci device to find an ancestor that contains a cpuset, and
 *     determines if it intersects with the cpuset that the process is bound to.
 *     if the process is not bound, or if a cpuset is unavailable for whatever
 *     reason, returns false. Otherwise, returns the result of
 *     hwloc_cpuset_intersects()
 */
static bool
compare_cpusets(hwloc_topology_t topology, struct fi_pci_attr pci)
{
    bool result = false;
    int ret;
    hwloc_bitmap_t proc_cpuset;
    hwloc_obj_t obj = NULL;

    /* Cannot find topology info if no topology is found */
    if (NULL == topology) {
        return false;
    }

    /* Allocate memory for proc_cpuset */
    proc_cpuset = hwloc_bitmap_alloc();
    if (NULL == proc_cpuset) {
        return false;
    }

    /* Fill cpuset with the collection of cpu cores that the process runs on */
    ret = hwloc_get_cpubind(topology, proc_cpuset, HWLOC_CPUBIND_PROCESS);
    if (0 > ret) {
        goto error;
    }

    /* Get the pci device from bdf */
    obj = hwloc_get_pcidev_by_busid(topology, pci.domain_id, pci.bus_id,
                                    pci.device_id, pci.function_id);
    if (NULL == obj) {
        goto error;
    }

    /* pcidev objects don't have cpusets so find the first non-io object above */
    obj = hwloc_get_non_io_ancestor_obj(topology, obj);
    if (NULL != obj) {
        result = hwloc_bitmap_intersects(proc_cpuset, obj->cpuset);
    }

error:
    hwloc_bitmap_free(proc_cpuset);
    return result;
}
#endif

/* Count providers returns the number of providers present in an fi_info list
 *     @param (IN) provider_list    struct fi_info* list of providers available
 *
 *     @param (OUT)                 int number of providers present in the list
 *
 *     returns 0 if the list is NULL
 */
static int
count_providers(struct fi_info* provider_list)
{
    struct fi_info* dev = provider_list;
    int num_provider = 0;

    while (NULL != dev) {
        num_provider++;
        dev = dev->next;
    }

    return num_provider;
}

/* Selects a NIC based on hardware locality between process cpuset and device BDF.
 *
 * Initializes opal_hwloc_topology to access hardware topology if not previously
 * initialized
 *
 * There are 3 main cases that this covers:
 *
 *      1. If the first provider passed into this function is the only valid
 *      provider, this provider is returned.
 *
 *      2. If there is more than 1 provider that matches the type of the first
 *      provider in the list, and the BDF data
 *      is available then a provider is selected based on locality of device
 *      cpuset and process cpuset and tries to ensure that processes are distributed
 *      evenly across NICs. This has two separate cases:
 *
 *          i. There is one or more provider local to the process:
 *
 *              (local rank % number of providers of the same type that share the process cpuset)
 *              is used to select one of these providers.
 *
 *          ii. There is no provider that is local to the process:
 *
 *              (local rank % number of providers of the same type)
 *              is used to select one of these providers
 *
 *      3. If there is more than 1 providers of the same type in the list, and the BDF data
 *      is not available (the ofi version does not support fi_info.nic or the
 *      provider does not support BDF) then (local rank % number of providers of the same type)
 *      is used to select one of these providers
 *
 *      @param provider_list (IN)   struct fi_info* An initially selected
 *                                  provider NIC. The provider name and
 *                                  attributes are used to restrict NIC
 *                                  selection. This provider is returned if the
 *                                  NIC selection fails.
 *
 *      @param local_index (IN)     int The local rank of the process. Used to
 *                                  select one valid NIC if there is a case
 *                                  where more than one can be selected. This
 *                                  could occur when more than one provider
 *                                  shares the same cpuset as the process.
 *
 *      @param provider (OUT)       struct fi_info* object with the selected
 *                                  provider if the selection succeeds
 *                                  if the selection fails, returns the fi_info
 *                                  object that was initially provided.
 *
 * All errors should be recoverable and will return the initially provided
 * provider. However, if an error occurs we can no longer guarantee
 * that the provider returned is local to the process or that the processes will
 * balance across available NICs.
 */
struct fi_info*
opal_mca_common_ofi_select_provider(struct fi_info *provider_list, int local_index)
{
    struct fi_info *provider = provider_list, *current_provider = provider_list;
    struct fi_info **provider_table;
#if OPAL_OFI_PCI_DATA_AVAILABLE
    struct fi_pci_attr pci;
#endif
    int ret;
    unsigned int num_provider = 0, provider_limit = 0;
    bool provider_found = false, cpusets_match = false;

    /* Initialize opal_hwloc_topology if it is not already */
    ret = opal_hwloc_base_get_topology();
    if (0 > ret) {
        /* Provider selection can continue but there is no guarantee of locality */
        opal_output_verbose(1, opal_common_ofi.output,
                            "%s:%d:Failed to initialize topology\n",
                            __FILE__, __LINE__);
    }

    provider_limit = count_providers(provider_list);

    /* Allocate memory for provider table */
    provider_table = calloc(provider_limit, sizeof(struct fi_info*));
    if (NULL == provider_table) {
        opal_output_verbose(1, opal_common_ofi.output,
                            "%s:%d:Failed to allocate memory for provider table\n",
                            __FILE__, __LINE__);
        return provider_list;
    }

    current_provider = provider;

    /* Cycle through remaining fi_info objects, looking for alike providers */
    while (NULL != current_provider) {
        if (!check_provider_attr(provider, current_provider)) {
            cpusets_match = false;
#if OPAL_OFI_PCI_DATA_AVAILABLE
            if (NULL != current_provider->nic) {
                pci = current_provider->nic->bus_attr->attr.pci;
                cpusets_match = compare_cpusets(opal_hwloc_topology, pci);
            }
#endif

            /* Reset the list if the cpusets match and no other provider was
             * found on the same cpuset as the process.
             */
            if (cpusets_match && !provider_found) {
                provider_found = true;
                num_provider = 0;
            }

            /* Add the provider to the provider list if the cpusets match or if
             * no other provider was found on the same cpuset as the process.
             */
            if (cpusets_match || !provider_found) {
                provider_table[num_provider] = current_provider;
                num_provider++;
            }
        }
        current_provider = current_provider->next;
    }

    /* Select provider from local rank % number of providers */
    if (num_provider > 0) {
        provider = provider_table[local_index % num_provider];
    }

#if OPAL_OFI_PCI_DATA_AVAILABLE
    if (NULL != provider->nic) {
        pci = provider->nic->bus_attr->attr.pci;
        cpusets_match = compare_cpusets(opal_hwloc_topology, pci);
    }
#endif

#if OPAL_DEBUG_ENABLE
    opal_output_verbose(1, opal_common_ofi.output,
                        "local rank: %d device: %s cpusets match: %s\n",
                         local_index, provider->domain_attr->name,
                         cpusets_match ? "true" : "false");
#endif

    free(provider_table);
    return provider;
}
