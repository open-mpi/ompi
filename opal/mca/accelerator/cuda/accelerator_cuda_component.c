/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2024-2026 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2024      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal_config.h"

#include <cuda.h>
#include <nvml.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "accelerator_cuda.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/dl/base/base.h"
#include "opal/runtime/opal_params.h"
#include "opal/util/argv.h"
#include "opal/util/printf.h"
#include "opal/util/proc.h"
#include "opal/util/show_help.h"
#include "opal/sys/atomic.h"

/* Define global variables, used in accelerator_cuda.c */
opal_accelerator_cuda_stream_t opal_accelerator_cuda_memcpy_stream = {0};
int opal_accelerator_cuda_num_devices = 0;

/* Initialization lock for delayed cuda initialization */
static opal_mutex_t accelerator_cuda_init_lock;
bool mca_accelerator_cuda_init_complete = false;

float *opal_accelerator_cuda_mem_bw = NULL;

/*
 * CUDA owns the NVLink domain cache. The MCA variable below is a read-only
 * string mirror used by OMPI_COMM_TYPE_NVLINK, with
 * opal_accelerator_nvlink_domain registered as a synonym so the communicator
 * does not depend on a CUDA symbol.
 */
static opal_accelerator_cuda_nvlink_domain_t accelerator_cuda_nvlink_domain = {
    .cuda_device = MCA_ACCELERATOR_NO_DEVICE_ID,
};
static char *accelerator_cuda_nvlink_domain_mca_string =
    "cuda_device=-1,cluster_uuid=00000000000000000000000000000000,clique_id=0";
static bool accelerator_cuda_nvlink_domain_mca_string_owned = false;
bool opal_accelerator_cuda_enable_vmm_check = true;
bool opal_accelerator_cuda_enable_mpool_check = true;

#define STRINGIFY2(x) #x
#define STRINGIFY(x)  STRINGIFY2(x)

typedef nvmlReturn_t (*opal_cuda_nvmlInit_v2_fn_t)(void);
typedef nvmlReturn_t (*opal_cuda_nvmlShutdown_fn_t)(void);
typedef nvmlReturn_t (*opal_cuda_nvmlDeviceGetHandleByPciBusId_v2_fn_t)(
    const char *pciBusId, nvmlDevice_t *device);
typedef nvmlReturn_t (*opal_cuda_nvmlDeviceGetGpuFabricInfoV_fn_t)(
    nvmlDevice_t device, nvmlGpuFabricInfoV_t *gpuFabricInfo);
typedef nvmlReturn_t (*opal_cuda_nvmlDeviceGetNvLinkState_fn_t)(
    nvmlDevice_t device, unsigned int link, nvmlEnableState_t *isActive);

#define OPAL_CUDA_NVML_ASSIGN_FN(type, dst, sym) \
    do {                                         \
        union {                                  \
            void *object;                        \
            type function;                       \
        } converter;                             \
        converter.object = (sym);                \
        *(dst) = converter.function;             \
    } while (0)

/* Unused variable that we register at init time and unregister at fini time.
 * This is used to detect if user has done a device reset prior to MPI_Finalize.
 * This is a workaround to avoid SEGVs.
 */
static int checkmem;
static int ctx_ok = 1;

/*
 * Public string showing the accelerator cuda component version number
 */
const char *opal_accelerator_cuda_component_version_string
    = "OPAL cuda accelerator MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int accelerator_cuda_open(void);
static int accelerator_cuda_close(void);
static int accelerator_cuda_component_register(void);
static opal_accelerator_base_module_t* accelerator_cuda_init(void);
static void accelerator_cuda_finalize(opal_accelerator_base_module_t* module);
static int accelerator_cuda_cache_nvlink_domain(void);
/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_accelerator_cuda_component_t mca_accelerator_cuda_component = {{

    /* First, the mca_component_t struct containing meta information
     * about the component itself */

    .base_version =
        {
            /* Indicate that we are a accelerator v1.1.0 component (which also
             * implies a specific MCA version) */

            OPAL_ACCELERATOR_BASE_VERSION_1_0_0,

            /* Component name and version */

            .mca_component_name = "cuda",
            MCA_BASE_MAKE_VERSION(component, OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION,
                                  OPAL_RELEASE_VERSION),

            /* Component open and close functions */

            .mca_open_component = accelerator_cuda_open,
            .mca_close_component = accelerator_cuda_close,
            .mca_register_component_params = accelerator_cuda_component_register,

        },
    /* Next the MCA v1.0.0 component meta data */
    .base_data =
        { /* The component is checkpoint ready */
         MCA_BASE_METADATA_PARAM_CHECKPOINT},
    .accelerator_init = accelerator_cuda_init,
    .accelerator_finalize = accelerator_cuda_finalize,
}};
MCA_BASE_COMPONENT_INIT(opal, accelerator, cuda)

static int accelerator_cuda_open(void)
{
    /* construct the component fields */
    return OPAL_SUCCESS;
}

static int accelerator_cuda_close(void)
{
    return OPAL_SUCCESS;
}

static int accelerator_cuda_component_register(void)
{
    int ret;

    ret = mca_base_component_var_register(&mca_accelerator_cuda_component.super.base_version,
                                          "nvlink_domain",
                                          "Cached CUDA NVLink domain used by OMPI_COMM_TYPE_NVLINK",
                                          MCA_BASE_VAR_TYPE_STRING, NULL, 0,
                                          MCA_BASE_VAR_FLAG_DEFAULT_ONLY, OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &accelerator_cuda_nvlink_domain_mca_string);
    if (0 > ret) {
        return ret;
    }
    (void) mca_base_var_register_synonym(ret, "opal", "accelerator", NULL,
                                         "nvlink_domain", 0);
    accelerator_cuda_nvlink_domain_mca_string_owned = true;

    (void) mca_base_component_var_register(&mca_accelerator_cuda_component.super.base_version,
                                           "enable_vmm_check",
                                           "Enable fallback classification of pointers via "
                                           "cuMemRetainAllocationHandle for CUDA Virtual Memory "
                                           "Management allocations. Disable if the application "
                                           "does not use cuMemMap-based allocations to avoid the "
                                           "per-call cost in check_addr.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
                                           &opal_accelerator_cuda_enable_vmm_check);

    (void) mca_base_component_var_register(&mca_accelerator_cuda_component.super.base_version,
                                           "enable_mpool_check",
                                           "Enable fallback classification of pointers via "
                                           "cuPointerGetAttribute(MEMPOOL_HANDLE) for CUDA "
                                           "stream-ordered memory pool allocations. Disable if "
                                           "the application does not use cuMemAllocAsync to avoid "
                                           "the per-call cost in check_addr.",
                                           MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                           OPAL_INFO_LVL_5, MCA_BASE_VAR_SCOPE_READONLY,
                                           &opal_accelerator_cuda_enable_mpool_check);

    return OPAL_SUCCESS;
}

static int accelerator_cuda_load_nvml(opal_dl_handle_t **handle,
                                      opal_cuda_nvmlInit_v2_fn_t *dyn_nvml_init,
                                      opal_cuda_nvmlShutdown_fn_t *dyn_nvml_shutdown,
                                      opal_cuda_nvmlDeviceGetHandleByPciBusId_v2_fn_t
                                          *dyn_nvml_device_get_handle_by_pci_bus_id,
                                      opal_cuda_nvmlDeviceGetGpuFabricInfoV_fn_t
                                          *dyn_nvml_device_get_gpu_fabric_info_v,
                                      opal_cuda_nvmlDeviceGetNvLinkState_fn_t
                                          *dyn_nvml_device_get_nvlink_state)
{
    const char *nvml_libraries[] = {"libnvidia-ml.so.1", "libnvidia-ml.so", NULL};
    void *symbol;
    char *err_msg = NULL;
    int rc = OPAL_ERROR;

    *handle = NULL;

#if !OPAL_HAVE_DL_SUPPORT
    return OPAL_ERR_NOT_AVAILABLE;
#endif
    if (NULL == opal_dl) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    for (int i = 0; NULL != nvml_libraries[i]; ++i) {
        rc = opal_dl_open(nvml_libraries[i], false, true, handle, &err_msg);
        if (OPAL_SUCCESS == rc) {
            break;
        }
    }

    if (OPAL_SUCCESS != rc) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    rc = opal_dl_lookup(*handle, "nvmlInit_v2", &symbol, NULL);
    if (OPAL_SUCCESS != rc) {
        goto error;
    }
    OPAL_CUDA_NVML_ASSIGN_FN(opal_cuda_nvmlInit_v2_fn_t, dyn_nvml_init, symbol);

    rc = opal_dl_lookup(*handle, "nvmlShutdown", &symbol, NULL);
    if (OPAL_SUCCESS != rc) {
        goto error;
    }
    OPAL_CUDA_NVML_ASSIGN_FN(opal_cuda_nvmlShutdown_fn_t, dyn_nvml_shutdown, symbol);

    rc = opal_dl_lookup(*handle, "nvmlDeviceGetHandleByPciBusId_v2", &symbol, NULL);
    if (OPAL_SUCCESS != rc) {
        goto error;
    }
    OPAL_CUDA_NVML_ASSIGN_FN(opal_cuda_nvmlDeviceGetHandleByPciBusId_v2_fn_t,
                             dyn_nvml_device_get_handle_by_pci_bus_id, symbol);

    rc = opal_dl_lookup(*handle, "nvmlDeviceGetGpuFabricInfoV", &symbol, NULL);
    if (OPAL_SUCCESS != rc) {
        goto error;
    }
    OPAL_CUDA_NVML_ASSIGN_FN(opal_cuda_nvmlDeviceGetGpuFabricInfoV_fn_t,
                             dyn_nvml_device_get_gpu_fabric_info_v, symbol);

    rc = opal_dl_lookup(*handle, "nvmlDeviceGetNvLinkState", &symbol, NULL);
    if (OPAL_SUCCESS != rc) {
        goto error;
    }
    OPAL_CUDA_NVML_ASSIGN_FN(opal_cuda_nvmlDeviceGetNvLinkState_fn_t,
                             dyn_nvml_device_get_nvlink_state, symbol);

    return OPAL_SUCCESS;

error:
    opal_dl_close(*handle);
    *handle = NULL;
    return OPAL_ERR_NOT_AVAILABLE;
}

static bool accelerator_cuda_nvlink_fabric_info_has_domain(const nvmlGpuFabricInfoV_t *fabric_info)
{
    return NVML_GPU_FABRIC_STATE_COMPLETED == fabric_info->state &&
           NVML_SUCCESS == fabric_info->status;
}

static bool accelerator_cuda_nvlink_cluster_uuid_is_zero(const unsigned char *cluster_uuid)
{
    for (int i = 0; i < NVML_GPU_FABRIC_UUID_LEN; ++i) {
        if (0 != cluster_uuid[i]) {
            return false;
        }
    }

    return true;
}

static bool accelerator_cuda_nvlink_has_active_link(
    opal_cuda_nvmlDeviceGetNvLinkState_fn_t dyn_nvml_device_get_nvlink_state,
    nvmlDevice_t nvml_device)
{
    for (unsigned int link = 0; link < NVML_NVLINK_MAX_LINKS; ++link) {
        nvmlEnableState_t state;
        nvmlReturn_t rc;

        rc = dyn_nvml_device_get_nvlink_state(nvml_device, link, &state);
        if (NVML_SUCCESS == rc && NVML_FEATURE_ENABLED == state) {
            return true;
        }
    }

    return false;
}

static uint64_t accelerator_cuda_nvlink_hash_hostname(const char *hostname)
{
    uint64_t hash = 1469598103934665603ULL;

    if (NULL == hostname) {
        hostname = "unknown";
    }

    while ('\0' != *hostname) {
        hash ^= (uint8_t) *hostname++;
        hash *= 1099511628211ULL;
    }

    return hash;
}

static void accelerator_cuda_nvlink_make_single_node_uuid(unsigned char *uuid,
                                                          const char *hostname)
{
    /* Encode an active single-node NVLink domain reported by NVML with an
     * all-zero cluster UUID. OMPI_COMM_TYPE_NVLINK does not synthesize this
     * token; CUDA is the only producer of the node-local NVLink encoding. */
    static const unsigned char signature[8] = {'O', 'M', 'P', 'I',
                                               'N', 'V', 'L', '0'};
    uint64_t hash = accelerator_cuda_nvlink_hash_hostname(hostname);

    for (int i = 0; i < 8; ++i) {
        uuid[i] = (hash >> (56 - i * 8)) & 0xff;
    }
    memcpy(uuid + 8, signature, sizeof(signature));
}

static void accelerator_cuda_nvlink_domain_uuid_to_hex(
    const uint8_t uuid[OPAL_ACCELERATOR_NVLINK_CLUSTER_UUID_LEN],
    char hex_uuid[2 * OPAL_ACCELERATOR_NVLINK_CLUSTER_UUID_LEN + 1])
{
    static const char hex[] = "0123456789abcdef";

    for (int i = 0; i < OPAL_ACCELERATOR_NVLINK_CLUSTER_UUID_LEN; ++i) {
        hex_uuid[2 * i] = hex[uuid[i] >> 4];
        hex_uuid[2 * i + 1] = hex[uuid[i] & 0x0f];
    }
    hex_uuid[2 * OPAL_ACCELERATOR_NVLINK_CLUSTER_UUID_LEN] = '\0';
}

static int accelerator_cuda_update_nvlink_domain_mca_string(void)
{
    char hex_uuid[2 * OPAL_ACCELERATOR_NVLINK_CLUSTER_UUID_LEN + 1];
    char value[128];
    char *new_value;
    int written;

    accelerator_cuda_nvlink_domain_uuid_to_hex(accelerator_cuda_nvlink_domain.cluster_uuid,
                                               hex_uuid);
    written = snprintf(value, sizeof(value), "cuda_device=%d,cluster_uuid=%s,clique_id=%u",
                       accelerator_cuda_nvlink_domain.cuda_device, hex_uuid,
                       accelerator_cuda_nvlink_domain.clique_id);
    if (0 > written || sizeof(value) <= (size_t) written) {
        return OPAL_ERROR;
    }

    new_value = strdup(value);
    if (NULL == new_value) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    if (accelerator_cuda_nvlink_domain_mca_string_owned) {
        free(accelerator_cuda_nvlink_domain_mca_string);
    }
    accelerator_cuda_nvlink_domain_mca_string = new_value;
    accelerator_cuda_nvlink_domain_mca_string_owned = true;

    return OPAL_SUCCESS;
}

static nvmlReturn_t accelerator_cuda_get_gpu_fabric_info(
    opal_cuda_nvmlDeviceGetGpuFabricInfoV_fn_t dyn_nvml_device_get_gpu_fabric_info_v,
    nvmlDevice_t nvml_device, nvmlGpuFabricInfoV_t *fabric_info)
{
    nvmlReturn_t rc;

#if defined(nvmlGpuFabricInfo_v3)
    memset(fabric_info, 0, sizeof(*fabric_info));
    fabric_info->version = nvmlGpuFabricInfo_v3;
    rc = dyn_nvml_device_get_gpu_fabric_info_v(nvml_device, fabric_info);
    if (NVML_SUCCESS == rc && accelerator_cuda_nvlink_fabric_info_has_domain(fabric_info)) {
        return NVML_SUCCESS;
    }
#endif

    memset(fabric_info, 0, sizeof(*fabric_info));
    fabric_info->version = nvmlGpuFabricInfo_v2;
    return dyn_nvml_device_get_gpu_fabric_info_v(nvml_device, fabric_info);
}

static int accelerator_cuda_cache_nvlink_domain(void)
{
    opal_cuda_nvmlDeviceGetGpuFabricInfoV_fn_t dyn_nvml_device_get_gpu_fabric_info_v;
    opal_cuda_nvmlDeviceGetHandleByPciBusId_v2_fn_t dyn_nvml_device_get_handle_by_pci_bus_id;
    opal_cuda_nvmlDeviceGetNvLinkState_fn_t dyn_nvml_device_get_nvlink_state;
    nvmlGpuFabricInfoV_t fabric_info;
    opal_accelerator_cuda_nvlink_domain_t domain = {
        .cuda_device = MCA_ACCELERATOR_NO_DEVICE_ID,
    };
    opal_cuda_nvmlShutdown_fn_t dyn_nvml_shutdown;
    nvmlDevice_t nvml_device;
    opal_cuda_nvmlInit_v2_fn_t dyn_nvml_init;
    opal_dl_handle_t *nvml_handle = NULL;
    char pci_bus_id[16] = {0};
    CUdevice cuDevice;
    CUresult cu_rc;
    nvmlReturn_t nvml_rc;
    int rc;

    accelerator_cuda_nvlink_domain = domain;
    (void) accelerator_cuda_update_nvlink_domain_mca_string();

    cu_rc = cuCtxGetDevice(&cuDevice);
    if (CUDA_SUCCESS != cu_rc) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    cu_rc = cuDeviceGetPCIBusId(pci_bus_id, sizeof(pci_bus_id), cuDevice);
    if (CUDA_SUCCESS != cu_rc) {
        return OPAL_ERR_NOT_AVAILABLE;
    }

    rc = accelerator_cuda_load_nvml(&nvml_handle, &dyn_nvml_init, &dyn_nvml_shutdown,
                                    &dyn_nvml_device_get_handle_by_pci_bus_id,
                                    &dyn_nvml_device_get_gpu_fabric_info_v,
                                    &dyn_nvml_device_get_nvlink_state);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    nvml_rc = dyn_nvml_init();
    if (NVML_SUCCESS != nvml_rc) {
        goto out;
    }

    nvml_rc = dyn_nvml_device_get_handle_by_pci_bus_id(pci_bus_id, &nvml_device);
    if (NVML_SUCCESS != nvml_rc) {
        goto shutdown;
    }

    if (!accelerator_cuda_nvlink_has_active_link(dyn_nvml_device_get_nvlink_state,
                                                 nvml_device)) {
        goto shutdown;
    }

    nvml_rc = accelerator_cuda_get_gpu_fabric_info(dyn_nvml_device_get_gpu_fabric_info_v,
                                                   nvml_device, &fabric_info);
    if (NVML_SUCCESS != nvml_rc ||
        !accelerator_cuda_nvlink_fabric_info_has_domain(&fabric_info)) {
        goto shutdown;
    }

    domain.cuda_device = (int) cuDevice;
    if (accelerator_cuda_nvlink_cluster_uuid_is_zero(fabric_info.clusterUuid)) {
        /* NVML reports an all-zero cluster UUID for single-node NVLink domains.
         * Store a nonzero hostname-derived token only from the CUDA component,
         * so the MCA default can remain all zero without suggesting a
         * node-local NVLink domain. In this case, use the CUDA device ordinal
         * as the split key. */
        domain.clique_id = (uint32_t) cuDevice;
        accelerator_cuda_nvlink_make_single_node_uuid(domain.cluster_uuid,
                                                      OPAL_PROC_MY_HOSTNAME);
    } else {
        /* Multi-node/fabric domains provide a real nonzero clusterUuid and a
         * cliqueId, which become the split color token and key respectively. */
        domain.clique_id = fabric_info.cliqueId;
        memcpy(domain.cluster_uuid, fabric_info.clusterUuid, sizeof(domain.cluster_uuid));
    }

    accelerator_cuda_nvlink_domain = domain;
    (void) accelerator_cuda_update_nvlink_domain_mca_string();

shutdown:
    dyn_nvml_shutdown();
out:
    opal_dl_close(nvml_handle);
    return OPAL_SUCCESS;
}

int opal_accelerator_cuda_delayed_init()
{
    int result = OPAL_SUCCESS;
    CUcontext cuContext;

    /* Double checked locking to avoid having to
     * grab locks post lazy-initialization.  */
    opal_atomic_rmb();
    if (true == mca_accelerator_cuda_init_complete) {
        return OPAL_SUCCESS;
    }
    OPAL_THREAD_LOCK(&accelerator_cuda_init_lock);

    /* If already initialized, just exit */
    if (true == mca_accelerator_cuda_init_complete) {
        goto out;
    }

    cuDeviceGetCount(&opal_accelerator_cuda_num_devices);

    /* Check to see if this process is running in a CUDA context.  If
     * so, all is good.  If not, then disable registration of memory. */
    result = cuCtxGetCurrent(&cuContext);
    if (CUDA_SUCCESS != result) {
        result = OPAL_ERR_NOT_INITIALIZED;
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent failed");
        goto out;
    } else if ((CUDA_SUCCESS == result) && (NULL == cuContext)) {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent returned NULL context");

        /* create a context for each device */
        for (int i = 0; i < opal_accelerator_cuda_num_devices; ++i) {
            CUdevice dev;
            result = cuDeviceGet(&dev, i);
            if (CUDA_SUCCESS != result) {
                opal_output_verbose(20, opal_accelerator_base_framework.framework_output,
                                    "CUDA: cuDeviceGet failed");
                result = OPAL_ERROR;
                goto out;
            }
            result = cuDevicePrimaryCtxRetain(&cuContext, dev);
            if (CUDA_SUCCESS != result) {
                opal_output_verbose(20, opal_accelerator_base_framework.framework_output,
                                    "CUDA: cuDevicePrimaryCtxRetain failed");
                result = OPAL_ERROR;
                goto out;
            }
            if (0 == i) {
                result = cuCtxPushCurrent(cuContext);
                if (CUDA_SUCCESS != result) {
                    opal_output_verbose(20, opal_accelerator_base_framework.framework_output,
                                        "CUDA: cuCtxPushCurrent failed");
                    result = OPAL_ERROR;
                    goto out;
                }
            }
        }


    } else {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output, "CUDA: cuCtxGetCurrent succeeded");
        (void) accelerator_cuda_cache_nvlink_domain();
    }

    /* Create stream for use in cuMemcpyAsync synchronous copies */
    CUstream memcpy_stream;
    result = cuStreamCreate(&memcpy_stream, 0);
    if (OPAL_UNLIKELY(result != CUDA_SUCCESS)) {
        opal_show_help("help-accelerator-cuda.txt", "cuStreamCreate failed", true,
                       OPAL_PROC_MY_HOSTNAME, result);
        goto out;
    }
    opal_accelerator_cuda_memcpy_stream.base.stream = malloc(sizeof(CUstream));
    *(CUstream*)opal_accelerator_cuda_memcpy_stream.base.stream = memcpy_stream;

    result = cuMemHostRegister(&checkmem, sizeof(int), 0);
    if (result != CUDA_SUCCESS) {
        /* If registering the memory fails, print a message and continue.
         * This is not a fatal error. */
        opal_show_help("help-accelerator-cuda.txt", "cuMemHostRegister during init failed", true,
                       &checkmem, sizeof(int), OPAL_PROC_MY_HOSTNAME, result, "checkmem");
    } else {
        opal_output_verbose(20, opal_accelerator_base_framework.framework_output,
                            "CUDA: cuMemHostRegister OK on test region");
    }

    /* determine the memory bandwidth */
    opal_accelerator_cuda_mem_bw = malloc(sizeof(float)*opal_accelerator_cuda_num_devices);
    for (int i = 0; i < opal_accelerator_cuda_num_devices; ++i) {
        CUdevice dev;
        result = cuDeviceGet(&dev, i);
        if (CUDA_SUCCESS != result) {
            opal_output_verbose(20, opal_accelerator_base_framework.framework_output,
                                "CUDA: cuDeviceGet failed");
            goto out;
        }
        int mem_clock_rate; // kHz
        result = cuDeviceGetAttribute(&mem_clock_rate,
                                CU_DEVICE_ATTRIBUTE_MEMORY_CLOCK_RATE,
                                dev);
        int bus_width; // bit
        result = cuDeviceGetAttribute(&bus_width,
                                CU_DEVICE_ATTRIBUTE_GLOBAL_MEMORY_BUS_WIDTH,
                                dev);
        /* bw = clock_rate * bus width * 2bit multiplier
         * See https://forums.developer.nvidia.com/t/memory-clock-rate/107940
         */
        float bw = ((float)mem_clock_rate*(float)bus_width*2.0) / 1024 / 1024 / 8;
        opal_accelerator_cuda_mem_bw[i] = bw;
    }

    result = OPAL_SUCCESS;
    opal_atomic_wmb();
    mca_accelerator_cuda_init_complete = true;
out:
    OPAL_THREAD_UNLOCK(&accelerator_cuda_init_lock);
    return result;
}

static opal_accelerator_base_module_t* accelerator_cuda_init(void)
{
    OBJ_CONSTRUCT(&accelerator_cuda_init_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&opal_accelerator_cuda_memcpy_stream, opal_accelerator_cuda_stream_t);

    /* First check if the support is enabled.  In the case that the user has
     * turned it off, we do not need to continue with any CUDA specific
     * initialization.  Do this after MCA parameter registration. */
    if (!opal_cuda_support) {
        return NULL;
    }

    (void)opal_accelerator_cuda_delayed_init();
    return &opal_accelerator_cuda_module;
}

static void accelerator_cuda_finalize(opal_accelerator_base_module_t* module)
{
    CUresult result;
    /* This call is in here to make sure the context is still valid.
     * This was the one way of checking which did not cause problems
     * while calling into the CUDA library.  This check will detect if
     * a user has called cudaDeviceReset prior to MPI_Finalize. If so,
     * then this call will fail and we skip cleaning up CUDA resources. */
    result = cuMemHostUnregister(&checkmem);
    if (CUDA_SUCCESS != result) {
        ctx_ok = 0;
    }

    if ((NULL != opal_accelerator_cuda_memcpy_stream.base.stream) && ctx_ok) {
        OBJ_DESTRUCT(&opal_accelerator_cuda_memcpy_stream);
    }

    free(opal_accelerator_cuda_mem_bw);
    opal_accelerator_cuda_mem_bw = NULL;

    OBJ_DESTRUCT(&accelerator_cuda_init_lock);
    return;
}
