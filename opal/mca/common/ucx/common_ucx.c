/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (C) Mellanox Technologies Ltd. 2018. ALL RIGHTS RESERVED.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Google, LLC. All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "common_ucx.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/pmix/pmix-internal.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/argv.h"
#include "opal/util/printf.h"

#include <fnmatch.h>
#include <stdio.h>
#include <ucm/api/ucm.h>

/***********************************************************************/

extern mca_base_framework_t opal_memory_base_framework;

opal_common_ucx_module_t opal_common_ucx =
{
    .progress_iterations = 100,
    .opal_mem_hooks = 1,
    .tls = NULL,
    .devices = NULL,
};

static opal_mutex_t opal_common_ucx_mutex = OPAL_MUTEX_STATIC_INIT;

static void opal_common_ucx_mem_release_cb(void *buf, size_t length, void *cbdata, bool from_alloc)
{
    ucm_vm_munmap(buf, length);
}

OPAL_DECLSPEC void opal_common_ucx_mca_var_register(const mca_base_component_t *component)
{
    char *default_tls = "rc_verbs,ud_verbs,rc_mlx5,dc_mlx5,ud_mlx5,cuda_ipc,rocm_ipc";
    char *default_devices = "mlx*";
    int hook_index;
    int verbose_index;
    int progress_index;
    int tls_index;
    int devices_index;

    OPAL_THREAD_LOCK(&opal_common_ucx_mutex);

    /* It is harmless to re-register variables so go ahead an re-register. */
    verbose_index = mca_base_var_register("opal", "opal_common", "ucx", "verbose",
                                          "Verbose level of the UCX components",
                                          MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                          MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                          MCA_BASE_VAR_SCOPE_LOCAL, &opal_common_ucx.verbose);
    progress_index = mca_base_var_register("opal", "opal_common", "ucx", "progress_iterations",
                                           "Set number of calls of internal UCX progress "
                                           "calls per opal_progress call",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0,
                                           MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
                                           MCA_BASE_VAR_SCOPE_LOCAL,
                                           &opal_common_ucx.progress_iterations);
    hook_index = mca_base_var_register("opal", "opal_common", "ucx", "opal_mem_hooks",
                                       "Use OPAL memory hooks, instead of UCX internal "
                                       "memory hooks",
                                       MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_3,
                                       MCA_BASE_VAR_SCOPE_LOCAL,
                                       &opal_common_ucx.opal_mem_hooks);

    if (NULL == opal_common_ucx.tls) {
        // Extra level of string indirection needed to make ompi_info
        // happy since it will unload this library before the MCA base
        // cleans up the MCA vars. This will cause the string to go
        // out of scope unless we place the pointer to it on the heap.
        opal_common_ucx.tls = (char **) malloc(sizeof(char *));
        *opal_common_ucx.tls = strdup(default_tls);
    }

    tls_index = mca_base_component_var_register(
        component, "tls",
        "List of UCX transports which should be supported on the system, to enable "
        "selecting the UCX component. Special values: any (any available). "
        "A '^' prefix negates the list. "
        "For example, in order to exclude on shared memory and TCP transports, "
        "please set to '^posix,sysv,self,tcp,cma,knem,xpmem'.",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0,
        MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL,
        opal_common_ucx.tls);

    if (NULL == opal_common_ucx.devices) {
        opal_common_ucx.devices = (char **) malloc(sizeof(char *));
        *opal_common_ucx.devices = strdup(default_devices);
    }
    devices_index = mca_base_component_var_register(
        component, "devices",
        "List of device driver pattern names, which, if supported by UCX, will "
        "bump its priority above ob1. Special values: any (any available)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0,
        MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL,
        opal_common_ucx.devices);

    if (component) {
        mca_base_var_register_synonym(verbose_index, component->mca_project_name,
                                      component->mca_type_name, component->mca_component_name,
                                      "verbose", 0);
        mca_base_var_register_synonym(progress_index, component->mca_project_name,
                                      component->mca_type_name, component->mca_component_name,
                                      "progress_iterations", 0);
        mca_base_var_register_synonym(hook_index, component->mca_project_name,
                                      component->mca_type_name, component->mca_component_name,
                                      "opal_mem_hooks", 0);
        mca_base_var_register_synonym(tls_index, component->mca_project_name,
                                      component->mca_type_name, component->mca_component_name,
                                      "tls", 0);
        mca_base_var_register_synonym(devices_index, component->mca_project_name,
                                      component->mca_type_name, component->mca_component_name,
                                      "devices", 0);
    }

    OPAL_THREAD_UNLOCK(&opal_common_ucx_mutex);
}

OPAL_DECLSPEC void opal_common_ucx_mca_register(void)
{
    int ret;

    opal_common_ucx.registered++;
    if (opal_common_ucx.registered > 1) {
        /* process once */
        return;
    }

    opal_common_ucx.output = opal_output_open(NULL);
    opal_output_set_verbosity(opal_common_ucx.output, opal_common_ucx.verbose);

    /* Set memory hooks */
    if (opal_common_ucx.opal_mem_hooks) {
        ret = mca_base_framework_open(&opal_memory_base_framework, 0);
        if (OPAL_SUCCESS != ret) {
            /* failed to initialize memory framework - just exit */
            MCA_COMMON_UCX_VERBOSE(1,
                                   "failed to initialize memory base framework: %d, "
                                   "memory hooks will not be used",
                                   ret);
            return;
        }

        if ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
            == ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
                & opal_mem_hooks_support_level())) {
            MCA_COMMON_UCX_VERBOSE(1, "%s", "using OPAL memory hooks as external events");
            ucm_set_external_event(UCM_EVENT_VM_UNMAPPED);
            opal_mem_hooks_register_release(opal_common_ucx_mem_release_cb, NULL);
        }
    }
}

OPAL_DECLSPEC void opal_common_ucx_mca_deregister(void)
{
    /* unregister only on last deregister */
    opal_common_ucx.registered--;
    assert(opal_common_ucx.registered >= 0);
    if (opal_common_ucx.registered) {
        return;
    }
    opal_mem_hooks_unregister_release(opal_common_ucx_mem_release_cb);
    opal_output_close(opal_common_ucx.output);
}

#if HAVE_DECL_OPEN_MEMSTREAM
static bool opal_common_ucx_check_device(const char *device_name, char **device_list)
{
    char sysfs_driver_link[PATH_MAX];
    char driver_path[PATH_MAX];
    char ib_device_name[NAME_MAX];
    char *driver_name;
    char **list_item;
    ssize_t ret;
    char ib_device_name_fmt[NAME_MAX];

    /* mlx5_0:1 */
    opal_snprintf(ib_device_name_fmt, sizeof(ib_device_name_fmt),
                  "%%%u[^:]%%*d", NAME_MAX - 1);
    ret = sscanf(device_name, ib_device_name_fmt, &ib_device_name);
    if (ret != 1) {
        return false;
    }

    sysfs_driver_link[sizeof(sysfs_driver_link) - 1] = '\0';
    snprintf(sysfs_driver_link, sizeof(sysfs_driver_link) - 1,
             "/sys/class/infiniband/%s/device/driver", ib_device_name);

    ret = readlink(sysfs_driver_link, driver_path, sizeof(driver_path) - 1);
    if (ret < 0) {
        MCA_COMMON_UCX_VERBOSE(2, "readlink(%s) failed: %s", sysfs_driver_link, strerror(errno));
        return false;
    }
    driver_path[ret] = '\0'; /* readlink does not append \0 */

    driver_name = basename(driver_path);
    for (list_item = device_list; *list_item != NULL; ++list_item) {
        if (!fnmatch(*list_item, driver_name, 0)) {
            MCA_COMMON_UCX_VERBOSE(2, "driver '%s' matched by '%s'", driver_path, *list_item);
            return true;
        }
    }

    return false;
}
#endif

OPAL_DECLSPEC opal_common_ucx_support_level_t opal_common_ucx_support_level(ucp_context_h context)
{
    opal_common_ucx_support_level_t support_level = OPAL_COMMON_UCX_SUPPORT_NONE;
    static const char *support_level_names[]
        = {[OPAL_COMMON_UCX_SUPPORT_NONE] = "none",
           [OPAL_COMMON_UCX_SUPPORT_TRANSPORT] = "transports only",
           [OPAL_COMMON_UCX_SUPPORT_DEVICE] = "transports and devices"};
#if HAVE_DECL_OPEN_MEMSTREAM
    char rsc_tl_name[NAME_MAX], rsc_device_name[NAME_MAX];
    char rsc_name_fmt[NAME_MAX];
    char **tl_list, **device_list, **list_item;
    bool is_any_tl, is_any_device;
    bool found_tl, negate;
    char line[128];
    FILE *stream;
    char *buffer;
    size_t size;
    int ret;
#endif

    if ((*opal_common_ucx.tls == NULL) || (*opal_common_ucx.devices == NULL)) {
        opal_common_ucx_mca_var_register(NULL);
    }

    is_any_tl = !strcmp(*opal_common_ucx.tls, "any");
    is_any_device = !strcmp(*opal_common_ucx.devices, "any");

    /* Check for special value "any" */
    if (is_any_tl && is_any_device) {
        MCA_COMMON_UCX_VERBOSE(1, "ucx is enabled on any transport or device");
        support_level = OPAL_COMMON_UCX_SUPPORT_DEVICE;
        goto out;
    }

#if HAVE_DECL_OPEN_MEMSTREAM
    /* Split transports list */
    negate = ('^' == (*opal_common_ucx.tls)[0]);
    tl_list = opal_argv_split(*opal_common_ucx.tls + (negate ? 1 : 0), ',');
    if (tl_list == NULL) {
        MCA_COMMON_UCX_VERBOSE(1, "failed to split tl list '%s', ucx is disabled",
                               *opal_common_ucx.tls);
        goto out;
    }

    /* Split devices list */
    device_list = opal_argv_split(*opal_common_ucx.devices, ',');
    if (device_list == NULL) {
        MCA_COMMON_UCX_VERBOSE(1, "failed to split devices list '%s', ucx is disabled",
                               *opal_common_ucx.devices);
        goto out_free_tl_list;
    }

    /* Open memory stream to dump UCX information to */
    stream = open_memstream(&buffer, &size);
    if (stream == NULL) {
        MCA_COMMON_UCX_VERBOSE(1,
                               "failed to open memory stream for ucx info (%s), "
                               "ucx is disabled",
                               strerror(errno));
        goto out_free_device_list;
    }

    /* Print ucx transports information to the memory stream */
    ucp_context_print_info(context, stream);

    /* "# resource 6  :  md 5  dev 4  flags -- rc_verbs/mlx5_0:1" */
    opal_snprintf(rsc_name_fmt, sizeof(rsc_name_fmt),
        "# resource %%*d : md %%*d dev %%*d flags -- %%%u[^/ \n\r]/%%%u[^/ \n\r]",
        NAME_MAX - 1, NAME_MAX - 1);

    /* Rewind and read transports/devices list from the stream */
    fseek(stream, 0, SEEK_SET);
    while ((support_level != OPAL_COMMON_UCX_SUPPORT_DEVICE)
           && (fgets(line, sizeof(line), stream) != NULL)) {
        ret = sscanf(line, rsc_name_fmt, rsc_tl_name, rsc_device_name);
        if (ret != 2) {
            continue;
        }

        /* Check if 'rsc_tl_name' is found  provided list */
        found_tl = is_any_tl;
        for (list_item = tl_list; !found_tl && (*list_item != NULL); ++list_item) {
            found_tl = !strcmp(*list_item, rsc_tl_name);
        }

        /* Check if the transport has a match (either positive or negative) */
        assert(!(is_any_tl && negate));
        if (found_tl != negate) {
            if (is_any_device || opal_common_ucx_check_device(rsc_device_name, device_list)) {
                MCA_COMMON_UCX_VERBOSE(2, "%s/%s: matched both transport and device list",
                                       rsc_tl_name, rsc_device_name);
                support_level = OPAL_COMMON_UCX_SUPPORT_DEVICE;
            } else {
                MCA_COMMON_UCX_VERBOSE(2, "%s/%s: matched transport list but not device list",
                                       rsc_tl_name, rsc_device_name);
                support_level = OPAL_COMMON_UCX_SUPPORT_TRANSPORT;
            }
        } else {
            MCA_COMMON_UCX_VERBOSE(2, "%s/%s: did not match transport list", rsc_tl_name,
                                   rsc_device_name);
        }
    }

    MCA_COMMON_UCX_VERBOSE(2, "support level is %s", support_level_names[support_level]);
    fclose(stream);
    free(buffer);

out_free_device_list:
    opal_argv_free(device_list);
out_free_tl_list:
    opal_argv_free(tl_list);
out:
#else
    MCA_COMMON_UCX_VERBOSE(2, "open_memstream() was not found, ucx is disabled");
#endif
    return support_level;
}

void opal_common_ucx_empty_complete_cb(void *request, ucs_status_t status)
{
}

static void opal_common_ucx_mca_fence_complete_cb(int status, void *fenced)
{
    *(int *) fenced = 1;
}

#if HAVE_DECL_UCM_TEST_EVENTS
static ucs_status_t opal_common_ucx_mca_test_external_events(int events)
{
#    if HAVE_DECL_UCM_TEST_EXTERNAL_EVENTS
    return ucm_test_external_events(UCM_EVENT_VM_UNMAPPED);
#    else
    return ucm_test_events(UCM_EVENT_VM_UNMAPPED);
#    endif
}

static void opal_common_ucx_mca_test_events(void)
{
    static int warned = 0;
    const char *suggestion;
    ucs_status_t status;

    if (!warned) {
        if (opal_common_ucx.opal_mem_hooks) {
            suggestion = "Please check OPAL memory events infrastructure.";
            status = opal_common_ucx_mca_test_external_events(UCM_EVENT_VM_UNMAPPED);
        } else {
            suggestion = "Pls try adding --mca opal_common_ucx_opal_mem_hooks 1 "
                         "to mpirun/oshrun command line to resolve this issue.";
            status = ucm_test_events(UCM_EVENT_VM_UNMAPPED);
        }

        if (status != UCS_OK) {
            MCA_COMMON_UCX_WARN("UCX is unable to handle VM_UNMAP event. "
                                "This may cause performance degradation or data "
                                "corruption. %s",
                                suggestion);
            warned = 1;
        }
    }
}
#endif

void opal_common_ucx_mca_proc_added(void)
{
#if HAVE_DECL_UCM_TEST_EVENTS
    opal_common_ucx_mca_test_events();
#endif
}

OPAL_DECLSPEC int opal_common_ucx_mca_pmix_fence_nb(int *fenced)
{
    return PMIx_Fence_nb(NULL, 0, NULL, 0, opal_common_ucx_mca_fence_complete_cb, (void *) fenced);
}

OPAL_DECLSPEC int opal_common_ucx_mca_pmix_fence(ucp_worker_h worker)
{
    volatile int fenced = 0;
    int ret = OPAL_SUCCESS;

    if (OPAL_SUCCESS
        != (ret = PMIx_Fence_nb(NULL, 0, NULL, 0, opal_common_ucx_mca_fence_complete_cb,
                                (void *) &fenced))) {
        return ret;
    }

    MCA_COMMON_UCX_PROGRESS_LOOP(worker) {
        if(fenced) {
            break;
        }
    }

    return ret;
}

static void opal_common_ucx_wait_all_requests(void **reqs, int count, ucp_worker_h worker)
{
    int i;

    MCA_COMMON_UCX_VERBOSE(2, "waiting for %d disconnect requests", count);
    for (i = 0; i < count; ++i) {
        opal_common_ucx_wait_request(reqs[i], worker, "ucp_disconnect_nb");
        reqs[i] = NULL;
    }
}

OPAL_DECLSPEC int opal_common_ucx_del_procs_nofence(opal_common_ucx_del_proc_t *procs, size_t count,
                                                    size_t my_rank, size_t max_disconnect,
                                                    ucp_worker_h worker)
{
    size_t num_reqs;
    size_t max_reqs;
    void *dreq, **dreqs;
    size_t i;
    size_t n;

    MCA_COMMON_UCX_ASSERT(procs || !count);
    MCA_COMMON_UCX_ASSERT(max_disconnect > 0);

    max_reqs = (max_disconnect > count) ? count : max_disconnect;

    dreqs = malloc(sizeof(*dreqs) * max_reqs);
    if (dreqs == NULL) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    num_reqs = 0;

    for (i = 0; i < count; ++i) {
        n = (i + my_rank) % count;
        if (procs[n].ep == NULL) {
            continue;
        }

        MCA_COMMON_UCX_VERBOSE(2, "disconnecting from rank %zu", procs[n].vpid);
        dreq = ucp_disconnect_nb(procs[n].ep);
        if (dreq != NULL) {
            if (UCS_PTR_IS_ERR(dreq)) {
                MCA_COMMON_UCX_ERROR("ucp_disconnect_nb(%zu) failed: %s", procs[n].vpid,
                                     ucs_status_string(UCS_PTR_STATUS(dreq)));
                continue;
            } else {
                dreqs[num_reqs++] = dreq;
                if (num_reqs >= max_disconnect) {
                    opal_common_ucx_wait_all_requests(dreqs, num_reqs, worker);
                    num_reqs = 0;
                }
            }
        }
    }
    /* num_reqs == 0 is processed by opal_common_ucx_wait_all_requests routine,
     * so suppress coverity warning */
    /* coverity[uninit_use_in_call] */
    opal_common_ucx_wait_all_requests(dreqs, num_reqs, worker);
    free(dreqs);

    return OPAL_SUCCESS;
}

OPAL_DECLSPEC int opal_common_ucx_del_procs(opal_common_ucx_del_proc_t *procs, size_t count,
                                            size_t my_rank, size_t max_disconnect,
                                            ucp_worker_h worker)
{
    opal_common_ucx_del_procs_nofence(procs, count, my_rank, max_disconnect, worker);

    return opal_common_ucx_mca_pmix_fence(worker);
}

static void safety_valve(void) __opal_attribute_destructor__;
void safety_valve(void) {
    opal_mem_hooks_unregister_release(opal_common_ucx_mem_release_cb);
}
