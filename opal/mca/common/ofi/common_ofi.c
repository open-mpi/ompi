/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2020-2022 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020-2021 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2021-2023 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates. All rights
 *                         reserved.
 * Copyright (c) 2023      UT-Battelle, LLC.  All rights reserved.
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
#include <rdma/fi_cm.h>
#ifdef HAVE_RDMA_FI_EXT_H
#include <rdma/fi_ext.h>
#endif

#include "common_ofi.h"
#include "opal/constants.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/pmix/base/base.h"
#include "opal/util/argv.h"
#include "opal/util/show_help.h"

opal_common_ofi_module_t opal_common_ofi = {.prov_include = NULL,
                                            .prov_exclude = NULL,
                                            .output = -1};
static const char default_prov_exclude_list[] = "shm,sockets,tcp,udp,rstream,usnic,net";
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
    bool memory_base_frame_open = false;
    OPAL_THREAD_LOCK(&opal_common_ofi_mutex);

    if (NULL != opal_common_ofi_cache_fid) {
        OPAL_THREAD_UNLOCK(&opal_common_ofi_mutex);
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
    } else {
       memory_base_frame_open = true;
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

        if (memory_base_frame_open) {
            mca_base_framework_close(&opal_memory_base_framework);
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
        mca_base_framework_close(&opal_memory_base_framework);
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

    if (fi_version() < FI_VERSION(1, 0)) {
        return OPAL_ERROR;
    }

    OPAL_THREAD_LOCK(&opal_common_ofi_mutex);

    if (0 > include_index) {
        /*
         * this monkey business is needed because of the way the MCA VARs stuff tries to handle
         * pointers to strings when when destructing the MCA var database.  If you don't do
         * something like this,the MCA var framework will try to dereference a pointer which itself
         * is no longer a valid address owing to having been previously dlclosed.
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
static int check_tx_attr(struct fi_tx_attr *provider_info, struct fi_tx_attr *provider)
{
    if (!(provider->msg_order & ~(provider_info->msg_order))
        && !(provider->op_flags & ~(provider_info->op_flags))
        && (provider->inject_size == provider_info->inject_size)) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

/* check that the rx attributes match */
static int check_rx_attr(struct fi_rx_attr *provider_info, struct fi_rx_attr *provider)
{
    if (!(provider->msg_order & ~(provider_info->msg_order))
        && !(provider->op_flags & ~(provider_info->op_flags))) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

/* check that the ep attributes match */
static int check_ep_attr(struct fi_ep_attr *provider_info, struct fi_ep_attr *provider)
{
    if (!(provider->type & ~(provider_info->type))
        && !(provider->mem_tag_format & ~(provider_info->mem_tag_format))
        && (provider->max_msg_size == provider_info->max_msg_size)
        && (provider->tx_ctx_cnt == provider_info->tx_ctx_cnt)
        && (provider->rx_ctx_cnt == provider_info->rx_ctx_cnt)) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

/* check that the provider attributes match */
static int check_provider_attr(struct fi_info *provider_info, struct fi_info *provider)
{
    /* make sure both info are the same provider and provide the same attributes */
    if (0 == strcmp(provider_info->fabric_attr->prov_name, provider->fabric_attr->prov_name)
        && 0 == strcmp(provider_info->fabric_attr->name, provider->fabric_attr->name)
        && !check_tx_attr(provider_info->tx_attr, provider->tx_attr)
        && !check_rx_attr(provider_info->rx_attr, provider->rx_attr)
        && !check_ep_attr(provider_info->ep_attr, provider->ep_attr)
        && !(provider_info->caps & ~(provider->caps)) && !(provider_info->mode & ~(provider->mode))
        && provider_info->addr_format == provider->addr_format) {
        return 0;
    } else {
        return OPAL_ERROR;
    }
}

/**
 * Calculate device distances
 *
 * Calculate the distances between the current thread and all devices of
 * type OPENFABRICS or NETWORK.
 *
 * The shortest distances are the nearest and therefore most efficient
 * devices to use.
 *
 * Return an array of all the distances. Each entry is of type
 * pmix_device_distance_t
 *
 * This function is used if there is no PMIx server running.
 *
 * @param distances (OUT)     distances array
 * @param ndist (OUT)    number of entries in the distances array
 *
 * @return   0 on success. Error otherwise.
 *
 */
static int compute_dev_distances(pmix_device_distance_t **distances,
                                  size_t *ndist)
{
    int ret = OPAL_SUCCESS;
    size_t ninfo;
    pmix_info_t *info;
    pmix_cpuset_t cpuset;
    pmix_topology_t pmix_topo = PMIX_TOPOLOGY_STATIC_INIT;
    pmix_device_type_t type = PMIX_DEVTYPE_OPENFABRICS |
      PMIX_DEVTYPE_NETWORK;

    PMIX_CPUSET_CONSTRUCT(&cpuset);
    ret = PMIx_Get_cpuset(&cpuset, PMIX_CPUBIND_THREAD);
    if (PMIX_SUCCESS != ret) {
        /* we are not bound */
        ret = OPAL_ERR_NOT_BOUND;
        goto out;
    }
    /* if we are not bound, then we cannot compute distances */
    if (hwloc_bitmap_iszero(cpuset.bitmap) ||
        hwloc_bitmap_isfull(cpuset.bitmap)) {
        return OPAL_ERR_NOT_BOUND;
    }

    /* load the PMIX topology - this just loads a pointer to
     * the local topology held in PMIx, so you must not
     * free it */
    ret = PMIx_Load_topology(&pmix_topo);
    if (PMIX_SUCCESS != ret) {
        goto out;
    }

    ninfo = 1;
    info = PMIx_Info_create(ninfo);
    PMIx_Info_load(&info[0], PMIX_DEVICE_TYPE, &type, PMIX_DEVTYPE);
    ret = PMIx_Compute_distances(&pmix_topo, &cpuset, info, ninfo, distances,
                                 ndist);
    PMIx_Info_free(info, ninfo);

out:
    return ret;
}

/**
 * Find the nearest devices to the current thread
 *
 * Use the PMIx server or calculate the device distances, then out of the set of
 * returned distances find the subset of the nearest devices. This can be
 * 1 or more.
 *
 * @param num_distances (OUT)     number of entries in the returned array
 *
 * @return   An array of device distances which are nearest this thread
 *           or NULL if we fail to get the distances. In this case we will just
 *           revert to round robin.
 *
 */
static pmix_device_distance_t *
get_nearest_nics(int *num_distances, pmix_value_t **valin)
{
    size_t ndist, i;
    int ret, idx = 0;
    pmix_data_array_t *dptr;
    uint16_t near = USHRT_MAX;
    pmix_info_t directive;
    pmix_value_t *val = NULL;
    pmix_device_distance_t *distances, *nearest = NULL;

    PMIx_Info_load(&directive, PMIX_OPTIONAL, NULL, PMIX_BOOL);
    ret = PMIx_Get(&opal_process_info.myprocid,
             PMIX_DEVICE_DISTANCES, &directive, 1, &val);
    PMIx_Info_destruct(&directive);
    if (ret != PMIX_SUCCESS || !val) {
        ret = compute_dev_distances(&distances, &ndist);
        if (ret) {
            goto out;
        }
        goto find_nearest;
    }

    if (PMIX_DATA_ARRAY != val->type) {
        goto out;
    }
    dptr = val->data.darray;
    if (NULL == dptr) {
        goto out;
    }
    if (PMIX_DEVICE_DIST != dptr->type) {
        goto out;
    }

    distances = (pmix_device_distance_t*)dptr->array;
    ndist = dptr->size;

find_nearest:
    nearest = calloc(sizeof(*distances), ndist);
    if (!nearest) {
        goto out;
    }

    for (i = 0; i < ndist; i++) {
        if (distances[i].type != PMIX_DEVTYPE_NETWORK &&
            distances[i].type != PMIX_DEVTYPE_OPENFABRICS)
            continue;
        if (distances[i].mindist < near) {
            idx = 0;
            near = distances[i].mindist;
            nearest[idx] = distances[i];
            idx++;
        } else if (distances[i].mindist == near) {
            nearest[idx] = distances[i];
            idx++;
        }
    }

    *num_distances = idx;

out:
    *valin = val;
    return nearest;
}

#if OPAL_OFI_PCI_DATA_AVAILABLE
/**
 * Determine if a device is nearest
 *
 * Given a device distances array of the nearest pci devices,
 * determine if one of these device distances refers to the pci
 * device passed in
 *
 * @param distances (IN)     distances array
 * @param num_distances (IN) number of entries in the distances array
 * @param topology (IN)      topology of the node
 * @param pci (IN)           PCI device being examined
 *
 * @return   true if the PCI device is in the distances array or if the
 *           distances array is not provided. False otherwise.
 *
 */
#if HWLOC_API_VERSION < 0x00020000
static bool is_near(pmix_device_distance_t *distances,
                    int num_distances,
                    hwloc_topology_t topology,
                    struct fi_pci_attr pci)
{
    return true;
}
#else
static bool is_near(pmix_device_distance_t *distances,
                    int num_distances,
                    hwloc_topology_t topology,
                    struct fi_pci_attr pci)
{
    hwloc_obj_t pcidev, osdev;

    /* if we failed to find any distances, then we consider all interfaces
     * to be of equal distances and let the caller decide how to handle
     * them
     */
    if (!distances)
        return true;

    pcidev = hwloc_get_pcidev_by_busid(topology, pci.domain_id,
                        pci.bus_id, pci.device_id,
                        pci.function_id);
    if (!pcidev)
        return false;

    for(osdev = pcidev->io_first_child; osdev != NULL; osdev = osdev->next_sibling) {
        int i;

        if (osdev->attr->osdev.type == HWLOC_OBJ_OSDEV_OPENFABRICS) {
            const char *nguid = hwloc_obj_get_info_by_name(osdev,"NodeGUID");
            const char *sguid = hwloc_obj_get_info_by_name(osdev, "SysImageGUID");

            if (!nguid && !sguid)
                continue;

            for (i = 0; i < num_distances; i++) {
                char lsguid[20], lnguid[20];
                int ret;

                if (!distances[i].osname || !osdev->name
                    || strcmp(distances[i].osname, osdev->name))
                    continue;

                ret = sscanf(distances[i].uuid, "fab://%19s::%19s", lnguid, lsguid);
                if (ret != 2)
                    continue;
                if (nguid && (0 == strcasecmp(lnguid, nguid))) {
                    return true;
                } else if (sguid && (0 == strcasecmp(lsguid, sguid))) {
                    return true;
                }
            }
        } else if (osdev->attr->osdev.type == HWLOC_OBJ_OSDEV_NETWORK) {
            const char *address = hwloc_obj_get_info_by_name(osdev, "Address");
            if (!address)
                continue;
            for (i = 0; i < num_distances; i++) {
                char *addr = strstr(distances[i].uuid, "://");
                if (!addr || addr + 3 > distances[i].uuid
                    + strlen(distances[i].uuid))
                    continue;
                if (!strcmp(addr+3, address)) {
                    return true;
                }
            }
        }
    }

    return false;
}
#endif
#endif  // OPAL_OFI_PCI_DATA_AVAILABLE

/* Count providers returns the number of providers present in an fi_info list
 *     @param (IN) provider_list    struct fi_info* list of providers available
 *
 *     @param (OUT)                 int number of providers present in the list
 *
 *     returns 0 if the list is NULL
 */
static int count_providers(struct fi_info *provider_list)
{
    struct fi_info *dev = provider_list;
    int num_provider = 0;

    while (NULL != dev) {
        num_provider++;
        dev = dev->next;
    }

    return num_provider;
}

/* Calculate the current process package rank.
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
static uint32_t get_package_rank(opal_process_info_t *process_info)
{
    int i, level = 10;
    uint16_t relative_locality, *package_rank_ptr;
    uint32_t ranks_on_package = 0;
    opal_process_name_t pname;
    pmix_status_t rc;
    char **peers = NULL;
    char *local_peers = NULL;
    char *locality_string = NULL;

    pname.jobid = OPAL_PROC_MY_NAME.jobid;
    pname.vpid = OPAL_VPID_WILDCARD;

    /*
     * if we are a singleton just return myprocid.rank
     * because we by definition don't know about any local peers
     */
    if (opal_process_info.is_singleton) {
        return (uint32_t) process_info->myprocid.rank;
    }

#if HAVE_DECL_PMIX_PACKAGE_RANK
    // Try to get the PACKAGE_RANK from PMIx
    OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_PACKAGE_RANK, &pname, &package_rank_ptr, PMIX_UINT16);
    if (PMIX_SUCCESS == rc) {
        return (uint32_t) *package_rank_ptr;
    }
#endif

    // Get the local peers
    OPAL_MODEX_RECV_VALUE(rc, PMIX_LOCAL_PEERS, &pname, &local_peers, PMIX_STRING);
    if (PMIX_SUCCESS != rc || NULL == local_peers) {
        goto err;
    }
    peers = opal_argv_split(local_peers, ',');
    free(local_peers);

    for (i = 0; NULL != peers[i]; i++) {
        pname.vpid = strtoul(peers[i], NULL, 10);

        locality_string = NULL;
        // Get the LOCALITY_STRING for process[i]
        OPAL_MODEX_RECV_VALUE_OPTIONAL(rc, PMIX_LOCALITY_STRING, &pname, &locality_string,
                                       PMIX_STRING);
        if (PMIX_SUCCESS != rc || NULL == locality_string) {
            goto err;
        }

        // compute relative locality
        relative_locality = opal_hwloc_compute_relative_locality(process_info->locality,
                                                                 locality_string);
        free(locality_string);

        if ((uint16_t) pname.vpid == process_info->my_local_rank) {
            return ranks_on_package;
        }

        if (relative_locality & OPAL_PROC_ON_SOCKET) {
            ranks_on_package++;
        }
    }
err:
    if (opal_output_get_verbosity(opal_common_ofi.output) >= level) {
        opal_show_help("help-common-ofi.txt", "package_rank failed", true, level);
    }

    if (locality_string)
        free(locality_string);

    return (uint32_t) process_info->myprocid.rank;
}

struct fi_info *opal_common_ofi_select_provider(struct fi_info *provider_list,
                                                    opal_process_info_t *process_info)
{
    struct fi_info *provider = provider_list, *current_provider = provider_list;
    struct fi_info **provider_table;
#if OPAL_OFI_PCI_DATA_AVAILABLE
    pmix_device_distance_t *distances = NULL;
    pmix_value_t *pmix_val;
    struct fi_pci_attr pci;
    int num_distances = 0;
#endif
    bool near = false;
    int ret;
    unsigned int num_provider = 0, provider_limit = 0;
    bool provider_found = false;
    uint32_t package_rank = 0;

    /* Initialize opal_hwloc_topology if it is not already */
    ret = opal_hwloc_base_get_topology();
    if (0 > ret) {
        /* Provider selection can continue but there is no guarantee of locality */
        opal_output_verbose(1, opal_common_ofi.output, "%s:%d:Failed to initialize topology\n",
                            __FILE__, __LINE__);
    }

    provider_limit = count_providers(provider_list);

    /* Allocate memory for provider table */
    provider_table = calloc(provider_limit, sizeof(struct fi_info *));
    if (NULL == provider_table) {
        opal_output_verbose(1, opal_common_ofi.output,
                            "%s:%d:Failed to allocate memory for provider table\n", __FILE__,
                            __LINE__);
        return provider_list;
    }

#if OPAL_OFI_PCI_DATA_AVAILABLE
    /* find all the nearest devices to this thread, then out of these
     * determine which device we should bind to.
     */
    distances = get_nearest_nics(&num_distances, &pmix_val);
#endif

    current_provider = provider;

    /* Cycle through remaining fi_info objects, looking for alike providers */
    while (NULL != current_provider) {
        if (!check_provider_attr(provider, current_provider)) {
            near = false;
#if OPAL_OFI_PCI_DATA_AVAILABLE
            if (NULL != current_provider->nic
                && NULL != current_provider->nic->bus_attr
                && current_provider->nic->bus_attr->bus_type == FI_BUS_PCI) {
                pci = current_provider->nic->bus_attr->attr.pci;
                near = is_near(distances, num_distances,
                               opal_hwloc_topology, pci);
            }
#endif
            /* We could have multiple near providers */
            if (near && !provider_found) {
                provider_found = true;
                num_provider = 0;
            }

            /* Add the provider to the provider list if the cpusets match or if
             * no other provider was found on the same cpuset as the process.
             */
            if (near || !provider_found) {
                provider_table[num_provider] = current_provider;
                num_provider++;
            }
        }
        current_provider = current_provider->next;
    }

    /* Select provider from local rank % number of providers */
    if (num_provider >= 2) {
        // If there are multiple NICs "close" to the process, try to calculate package_rank
        package_rank = get_package_rank(process_info);
        provider = provider_table[package_rank % num_provider];
    } else if (num_provider == 1) {
        provider = provider_table[num_provider - 1];
    }

#if OPAL_OFI_PCI_DATA_AVAILABLE
    if (NULL != provider->nic
        && NULL != provider->nic->bus_attr
        && provider->nic->bus_attr->bus_type == FI_BUS_PCI) {
        pci = provider->nic->bus_attr->attr.pci;
        near = is_near(distances, num_distances,
                       opal_hwloc_topology, pci);
    }
#endif

#if OPAL_ENABLE_DEBUG
    opal_output_verbose(1, opal_common_ofi.output,
                        "package rank: %d device: %s near: %s\n", package_rank,
                        provider->domain_attr->name, near ? "true" : "false");
#endif

    free(provider_table);
#if OPAL_OFI_PCI_DATA_AVAILABLE
    if (pmix_val)
        PMIx_Value_free(pmix_val, 1);
#endif
    return provider;
}

/**
 * Obtain EP endpoint name
 *
 * Obtain the EP endpoint name and length for the supplied endpoint fid.
 *
 * @param fid (IN)     fid of (S)EP endpoint
 * @param addr (OUT)   buffer containing endpoint name 
 * @param addrlen (OUT) length of allocated buffer in bytes
 *
 * @return             OPAL_SUCCESS or OPAL error code
 *
 * The caller is responsible for freeing the buffer allocated to
 * contain the endpoint name.
 *
 */
OPAL_DECLSPEC int opal_common_ofi_fi_getname(fid_t fid, void **addr, size_t *addrlen)
{
    int ret=OPAL_SUCCESS;
    size_t namelen = 0;
    char *ep_name = NULL;

    /**
     * Get our address and publish it with modex.
     * Use the two step process of first getting the required
     * buffer size, then allocating the memory and calling
     * fi_getname again.
     */
    namelen = 0;
    ret = fi_getname(fid,
                     NULL,
                     &namelen);
    if ((FI_SUCCESS != ret) && (-FI_ETOOSMALL != ret)) {
        opal_output_verbose(1, opal_common_ofi.output, "%s:%d:fi_endpoint (namelen) returned %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        ret = OPAL_ERROR;
        goto error;
    }

    ep_name = (char *)malloc(namelen);
    if (NULL == ep_name) {
        ret = OPAL_ERR_OUT_OF_RESOURCE;
        goto error;
    }

    ret = fi_getname(fid,
                     ep_name,
                     &namelen);
    if (ret) {
        opal_output_verbose(1, opal_common_ofi.output, "%s:%d:fi_endpoint (ep_name) returned %s\n",
                            __FILE__, __LINE__, fi_strerror(-ret));
        ret = OPAL_ERROR;
        goto error;
    }

    *addr = ep_name;
    *addrlen = namelen;

    return ret;

error:
    if (NULL != ep_name) {
       free(ep_name); 
    }
    return ret;
}


