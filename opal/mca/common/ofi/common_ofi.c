/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2020      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2021      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020-2021 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "opal_config.h"

#include <errno.h>
#include <unistd.h>
#include <rdma/fabric.h>
#include <rdma/fi_errno.h>
#ifdef HAVE_RDMA_FI_EXT_H
#include <rdma/fi_ext.h>
#endif

#include "common_ofi.h"
#include "opal_config.h"
#include "opal/constants.h"
#include "opal/util/argv.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/util/show_help.h"

opal_common_ofi_module_t opal_common_ofi = {.prov_include = NULL,
                                            .prov_exclude = NULL,
                                            .output = -1};
static const char default_prov_exclude_list[] = "shm,sockets,tcp,udp,rstream,usnic";
static opal_mutex_t opal_common_ofi_mutex = OPAL_MUTEX_STATIC_INIT;
static int opal_common_ofi_verbose_level = 0;
static int opal_common_ofi_init_ref_cnt = 0;
#ifdef HAVE_STRUCT_FI_OPS_MEM_MONITOR
static bool opal_common_ofi_installed_memory_monitor = false;
#endif

#ifdef HAVE_STRUCT_FI_OPS_MEM_MONITOR

/*
 * Monitor object to export into Libfabric to provide memory release
 * notifications using our own memory hooks framework.  Monitors may
 * use the subscribe/unsubscribe notifications to reduce unnecessary
 * notifications, but are not required to do so.  Because patcher
 * notifies about all releases, it is cheaper for us to not filter and
 * this monitor can safely ignore subscribe/unsubscribe notifications.
 *
 * Libfabric requires the object to be fully defined.  Unlike most of
 * Open MPI, it does not have NULL function pointer checks in calling
 * code.
 */
static int opal_common_ofi_monitor_start(struct fid_mem_monitor *monitor)
{
    return 0;
}

static void opal_common_ofi_monitor_stop(struct fid_mem_monitor *monitor)
{
    return;
}

static int opal_common_ofi_monitor_subscribe(struct fid_mem_monitor *monitor,
                                             const void *addr, size_t len)
{
    return 0;
}

static void opal_common_ofi_monitor_unsubscribe(struct fid_mem_monitor *monitor,
                                                const void *addr, size_t len)
{
    return;
}

static bool opal_common_ofi_monitor_valid(struct fid_mem_monitor *monitor,
                                     const void *addr, size_t len)
{
    return true;
}

static struct fid_mem_monitor *opal_common_ofi_monitor = NULL;
static struct fid *opal_common_ofi_cache_fid = NULL;
static struct fi_ops_mem_monitor opal_common_ofi_export_ops = {
    .size = sizeof(struct fi_ops_mem_monitor),
    .start = opal_common_ofi_monitor_start,
    .stop = opal_common_ofi_monitor_stop,
    .subscribe = opal_common_ofi_monitor_subscribe,
    .unsubscribe = opal_common_ofi_monitor_unsubscribe,
    .valid = opal_common_ofi_monitor_valid,
};

/**
 * Callback function from Open MPI memory monitor
 *
 * Translation function between the callback function from Open MPI's
 * memory notifier to the Libfabric memory monitor.
 */
static void opal_common_ofi_mem_release_cb(void *buf, size_t length,
                                           void *cbdata, bool from_alloc)
{
    opal_common_ofi_monitor->import_ops->notify(opal_common_ofi_monitor,
                                                buf, length);
}

#endif /* HAVE_STRUCT_FI_OPS_MEM_MONITOR */

int opal_common_ofi_export_memory_monitor(void)
{
    int ret = -FI_ENOSYS;

#ifdef HAVE_STRUCT_FI_OPS_MEM_MONITOR
    OPAL_THREAD_LOCK(&opal_common_ofi_mutex);

    if (NULL != opal_common_ofi_cache_fid) {
        return 0;
    }

    /*
     * While the memory import functionality was introduced in 1.13,
     * some deadlock bugs exist in the 1.13 series.  Require version
     * 1.14 before this code is activated.  Not activating the code
     * should not break any functionality directly, but may lead to
     * sub-optimal memory monitors being used in Libfabric, as Open
     * MPI will almost certainly install a patcher first.
     */
    if (FI_VERSION_LT(fi_version(), FI_VERSION(1, 14))) {
        ret = -FI_ENOSYS;
        goto err;
    }

    ret = mca_base_framework_open(&opal_memory_base_framework, 0);
    if (OPAL_SUCCESS != ret) {
        ret = -FI_ENOSYS;
        goto err;
    }
    if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
        != (((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT))
        & opal_mem_hooks_support_level())) {
        ret = -FI_ENOSYS;
        goto err;
    }

    /*
     * The monitor import object has the well known name "mr_cache"
     * and was introduced in Libfabric 1.13
     */
    ret = fi_open(FI_VERSION(1,13), "mr_cache", NULL, 0, 0,
                  &opal_common_ofi_cache_fid, NULL);
    if (0 != ret) {
        goto err;
    }

    opal_common_ofi_monitor = calloc(1, sizeof(*opal_common_ofi_monitor));
    if (NULL == opal_common_ofi_monitor) {
        ret = -FI_ENOMEM;
        goto err;
    }

    opal_common_ofi_monitor->fid.fclass = FI_CLASS_MEM_MONITOR;
    opal_common_ofi_monitor->export_ops = &opal_common_ofi_export_ops;
    ret = fi_import_fid(opal_common_ofi_cache_fid,
                        &opal_common_ofi_monitor->fid, 0);
    if (0 != ret) {
        goto err;
    }
    opal_mem_hooks_register_release(opal_common_ofi_mem_release_cb, NULL);
    opal_common_ofi_installed_memory_monitor = true;

    ret = 0;

err:
    if (0 != ret) {
        if (NULL != opal_common_ofi_cache_fid) {
            fi_close(opal_common_ofi_cache_fid);
        }
        if (NULL != opal_common_ofi_monitor) {
            free(opal_common_ofi_monitor);
        }

        opal_common_ofi_installed_memory_monitor = false;
    }

    OPAL_THREAD_UNLOCK(&opal_common_ofi_mutex);
#endif

    return ret;
}

static int opal_common_ofi_remove_memory_monitor(void)
{
#ifdef HAVE_STRUCT_FI_OPS_MEM_MONITOR
    if (opal_common_ofi_installed_memory_monitor) {
        opal_mem_hooks_unregister_release(opal_common_ofi_mem_release_cb);
        fi_close(opal_common_ofi_cache_fid);
        fi_close(&opal_common_ofi_monitor->fid);
        free(opal_common_ofi_monitor);
        opal_common_ofi_installed_memory_monitor = false;
    }
#endif

    return OPAL_SUCCESS;
}

int opal_common_ofi_open(void)
{
    if ((opal_common_ofi_init_ref_cnt++) > 0) {
        return OPAL_SUCCESS;
    }

    return OPAL_SUCCESS;
}

int opal_common_ofi_close(void)
{
    int ret;

    if ((--opal_common_ofi_init_ref_cnt) > 0) {
        return OPAL_SUCCESS;
    }

    ret = opal_common_ofi_remove_memory_monitor();
    if (OPAL_SUCCESS != ret) {
        return ret;
    }

    if (-1 != opal_common_ofi.output) {
        opal_output_close(opal_common_ofi.output);
        opal_common_ofi.output = -1;
        if (OPAL_SUCCESS != ret) {
            return ret;
        }
    }

    return OPAL_SUCCESS;
}

int opal_common_ofi_is_in_list(char **list, char *item)
{
    int i = 0;

    if ((NULL == list) || (NULL == item)) {
        return 0;
    }

    while (NULL != list[i]) {
        if (0 == strncasecmp(item, list[i], strlen(list[i]))) {
            return 1;
        } else {
            i++;
        }
    }

    return 0;
}

int opal_common_ofi_mca_register(const mca_base_component_t *component)
{
    static int include_index = -1;
    static int exclude_index = -1;
    static int verbose_index = -1;
    int ret;

    if (fi_version() < FI_VERSION(1,0)) {
        return OPAL_ERROR;
    }

    OPAL_THREAD_LOCK(&opal_common_ofi_mutex);

    if (0 > include_index) {
        /*
         * this monkey business is needed because of the way the MCA VARs stuff tries to handle pointers to strings when
         * when destructing the MCA var database.  If you don't do something like this,the MCA var framework will try
         * to dereference a pointer which itself is no longer a valid address owing to having been previously dlclosed.
         */
        if (NULL == opal_common_ofi.prov_include) {
            opal_common_ofi.prov_include = (char **) malloc(sizeof(char *));
            assert(NULL != opal_common_ofi.prov_include);
        }
        *opal_common_ofi.prov_include = NULL;
        include_index = mca_base_var_register(
            "opal", "opal_common", "ofi", "provider_include",
            "Comma-delimited list of OFI providers that are considered for use (e.g., "
            "\"psm,psm2\"; an empty value means that all providers will be considered). Mutually "
            "exclusive with mtl_ofi_provider_exclude.",
            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_1, MCA_BASE_VAR_SCOPE_READONLY,
            opal_common_ofi.prov_include);
        if (0 > include_index) {
            ret = include_index;
            goto err;
        }
    }

    if (0 > exclude_index) {
        if (NULL == opal_common_ofi.prov_exclude) {
            opal_common_ofi.prov_exclude = (char **) malloc(sizeof(char *));
            assert(NULL != opal_common_ofi.prov_exclude);
        }
        *opal_common_ofi.prov_exclude = strdup(default_prov_exclude_list);
        exclude_index = mca_base_var_register(
            "opal", "opal_common", "ofi", "provider_exclude",
            "Comma-delimited list of OFI providers that are not considered for use (default: "
            "\"sockets,mxm\"; empty value means that all providers will be considered). Mutually "
            "exclusive with mtl_ofi_provider_include.",
            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0, OPAL_INFO_LVL_1, MCA_BASE_VAR_SCOPE_READONLY,
            opal_common_ofi.prov_exclude);
        if (0 > exclude_index) {
            ret = exclude_index;
            goto err;
        }
    }

    if (0 > verbose_index) {
        verbose_index = mca_base_var_register("opal", "opal_common", "ofi", "verbose",
                                              "Verbose level of the OFI components",
                                              MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                              MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                              MCA_BASE_VAR_SCOPE_LOCAL,
                                              &opal_common_ofi_verbose_level);
        if (0 > verbose_index) {
            ret = verbose_index;
            goto err;
        }
    }

    if (component) {
        ret = mca_base_var_register_synonym(include_index,
                                            component->mca_project_name,
                                            component->mca_type_name,
                                            component->mca_component_name,
                                            "provider_include", 0);
        if (0 > ret) {
            goto err;
        }
        ret = mca_base_var_register_synonym(exclude_index,
                                            component->mca_project_name,
                                            component->mca_type_name,
                                            component->mca_component_name,
                                            "provider_exclude", 0);
        if (0 > ret) {
            goto err;
        }
        ret = mca_base_var_register_synonym(verbose_index,
                                            component->mca_project_name,
                                            component->mca_type_name,
                                            component->mca_component_name,
                                            "verbose", 0);
        if (0 > ret) {
            goto err;
        }
    }

    /* The frameworks initialize their output streams during
     * register(), so we similarly try to initialize the output stream
     * as early as possible.  Because we may register synonyms for
     * each dependent component, we don't necessarily have all the
     * data to set verbosity during the first call to
     * common_ofi_register().  The MCA infrastructure has rules on
     * synonym value evaluation, so our rubric is to re-set verbosity
     * after every call to register() (which has registered a new
     * synonym).  This is not perfect, but it's not horrible, either.
     */
    if (opal_common_ofi.output == -1) {
        opal_common_ofi.output = opal_output_open(NULL);
    }
    opal_output_set_verbosity(opal_common_ofi.output, opal_common_ofi_verbose_level);

    ret = OPAL_SUCCESS;

err:
    OPAL_THREAD_UNLOCK(&opal_common_ofi_mutex);

    return ret;
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
        0 == strcmp(provider_info->fabric_attr->name, provider->fabric_attr->name) &&
        !check_tx_attr(provider_info->tx_attr, provider->tx_attr) &&
        !check_rx_attr(provider_info->rx_attr, provider->rx_attr) &&
        !check_ep_attr(provider_info->ep_attr, provider->ep_attr) &&
        !(provider_info->caps & ~(provider->caps)) &&
        !(provider_info->mode & ~(provider->mode)) &&
        provider_info->addr_format == provider->addr_format) {
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

/* Calculate the currrent process package rank.
 *     @param (IN) process_info     struct opal_process_info_t information
 *                                  about the current process. used to get
 *                                  num_local_peers, myprocid.rank, and
 *                                  my_local_rank.
 *
 *     @param (OUT)                 uint32_t package rank or myprocid.rank
 *
 * If successful, returns PMIX_PACKAGE_RANK, or an
 * equivalent calculated package rank.
 * otherwise falls back to using opal_process_info.myprocid.rank
 * this can affect performance, but is unlikely to happen.
 */
static uint32_t get_package_rank(int32_t num_local_peers, uint16_t my_local_rank, char *cpuset, uint32_t pid)
{
    int i;
    uint16_t relative_locality;
    uint16_t current_package_rank = 0;
    uint16_t package_ranks[num_local_peers + 1];
    opal_process_name_t pname;
    opal_status_t rc;
    char **peers = NULL;
    char *local_peers = NULL;
    char *locality_string = NULL;
    char *mylocality = NULL;
    uint16_t *package_rank_ptr;

    pname.jobid = OPAL_PROC_MY_NAME.jobid;
    pname.vpid = OPAL_VPID_WILDCARD;

    // Try to get the PACKAGE_RANK from PMIx
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, OPAL_PMIX_PACKAGE_RANK,
                                   &pname, &package_rank_ptr, OPAL_UINT16);
    if (OPAL_SUCCESS == rc) {
        return (uint32_t)*package_rank_ptr;
    }

    // Get the local peers
    OPAL_MODEX_RECV_VALUE(rc, OPAL_PMIX_LOCAL_PEERS,
                          &pname, &local_peers, OPAL_STRING);
    if (OPAL_SUCCESS != rc || NULL == local_peers) {
        // We can't find package_rank, fall back to procid
        opal_show_help("help-common-ofi.txt", "package_rank failed", true);
        return pid;
    }
    peers = opal_argv_split(local_peers, ',');
    free(local_peers);

    // Get my locality
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, OPAL_PMIX_LOCALITY_STRING,
                                   &OPAL_PROC_MY_NAME, &mylocality, OPAL_STRING);
    if (OPAL_SUCCESS != rc || NULL == mylocality) {
        // can we fall back to cpuset?
        if (NULL != cpuset && NULL != opal_hwloc_topology) {
            mylocality = opal_hwloc_base_get_locality_string(opal_hwloc_topology, cpuset);
        } else {
            // We can't find package_rank, fall back to procid
            opal_show_help("help-common-ofi.txt", "package_rank failed", true);
            return pid;
        }
    }

    for (i = 0; NULL != peers[i]; i++) {
        pname.vpid = strtoul(peers[i], NULL, 10);
        locality_string = NULL;
        // Get the LOCALITY_STRING for process[i]
        OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, OPAL_PMIX_LOCALITY_STRING,
                                       &pname, &locality_string, OPAL_STRING);
        if (OPAL_SUCCESS != rc || NULL == locality_string) {
            // If we don't have information about locality, fall back to procid
            int level = 10;
            if (opal_output_get_verbosity(opal_common_ofi.output) >= level) {
                opal_show_help("help-common-ofi.txt", "package_rank failed", true, level);
            }
            return pid;
        }

        // compute relative locality
        relative_locality = opal_hwloc_compute_relative_locality(mylocality, locality_string);
        free(locality_string);

        if (relative_locality & OPAL_PROC_ON_SOCKET) {
            package_ranks[i] = current_package_rank;
            current_package_rank++;
        }
    }
    free(mylocality);

    return (uint32_t)package_ranks[my_local_rank];
}

struct fi_info*
opal_mca_common_ofi_select_provider(struct fi_info *provider_list, int32_t num_local_peers,
                                    uint16_t my_local_rank, char *cpuset, uint32_t pid)
{
    struct fi_info *provider = provider_list, *current_provider = provider_list;
    struct fi_info **provider_table;
#if OPAL_OFI_PCI_DATA_AVAILABLE
    struct fi_pci_attr pci;
#endif
    int ret;
    uint32_t package_rank;
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
            if (NULL != current_provider->nic
                && NULL != current_provider->nic->bus_attr
                && current_provider->nic->bus_attr->bus_type == FI_BUS_PCI) {
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
    if (num_provider >= 2) {
        // If there are multiple NICs "close" to the process, try to calculate package_rank
        package_rank = get_package_rank(num_local_peers, my_local_rank, cpuset, pid);
        provider = provider_table[package_rank % num_provider];
    } else if (num_provider == 1) {
        provider = provider_table[num_provider - 1];
    }

#if OPAL_OFI_PCI_DATA_AVAILABLE
    if (NULL != provider->nic
        && NULL != provider->nic->bus_attr
        && provider->nic->bus_attr->bus_type == FI_BUS_PCI) {
        pci = provider->nic->bus_attr->attr.pci;
        cpusets_match = compare_cpusets(opal_hwloc_topology, pci);
    }
#endif

#if OPAL_ENABLE_DEBUG
    opal_output_verbose(1, opal_common_ofi.output,
                        "package rank: %d device: %s cpusets match: %s\n",
                         package_rank, provider->domain_attr->name,
                         cpusets_match ? "true" : "false");
#endif

    free(provider_table);
    return provider;
}
