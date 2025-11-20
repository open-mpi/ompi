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
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2018-2024 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019-2025 Google, LLC. All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <regex.h>

#include "opal_config.h"

#include "opal/mca/btl/base/base.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/memoryhooks/memory.h"
#include "opal/util/argv.h"
#include <ucm/api/ucm.h>

#include "opal/util/printf.h"

#include <string.h>

#include "btl_uct_am.h"
#include "btl_uct_device_context.h"

static int mca_btl_uct_component_register(void)
{
    mca_btl_uct_module_t *module = &mca_btl_uct_module_template;

    mca_btl_uct_component.memory_domains = "mlx5_0,mlx4_0,rocep0s4";
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "memory_domains",
        "Comma-delimited list of memory domains of the form "
        "to use for communication. Memory domains MUST provide transports that "
        "support put, get, and amos. Special values: all (all available), none."
        " (default: mlx5_0,mlx4_0,rocep0s4)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.memory_domains);

    mca_btl_uct_component.allowed_transports = "dc_mlx5,rc_mlx5,ud,ugni_rdma,ugni_smsg,any";
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "transports",
        "Comma-delimited list of transports to use sorted by increasing "
        "priority. The list of transports available can be queried using ucx_info. Special"
        "values: any (any available) (default: dc_mlx5,rc_mlx5,ud,any)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.allowed_transports);

    mca_btl_uct_component.connection_domains = "tcp";
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "connection_domains",
        "Comma-delimited list of connection-only domains to use sorted by increasing "
        "priority. The list of transports available can be queried using ucx_info. Special"
        "values: any (any available) (default: tcp)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.connection_domains);

    mca_btl_uct_component.num_contexts_per_module = 0;
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "num_contexts_per_module",
        "Number of UCT worker contexts "
        "to create for each BTL module. Larger numbers will improve "
        "multi-threaded performance but may increase memory usage. "
        "A good rule of thumb is one context per application thread "
        "that will be calling into MPI. (default: 0 -- autoselect "
        "based on the number of cores)",
        MCA_BASE_VAR_TYPE_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_ALL, &mca_btl_uct_component.num_contexts_per_module);

    mca_btl_uct_component.disable_ucx_memory_hooks = true;
    (void)
        mca_base_component_var_register(&mca_btl_uct_component.super.btl_version,
                                        "disable_ucx_memory_hooks",
                                        "Disable the munmap memory hook "
                                        "inside UCX. These hooks are not necessary when using the "
                                        "uct btl and tend to cause performance problems when using "
                                        "multiple threads (default: true)",
                                        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE,
                                        OPAL_INFO_LVL_3, MCA_BASE_VAR_SCOPE_ALL,
                                        &mca_btl_uct_component.disable_ucx_memory_hooks);

#if OPAL_C_HAVE__THREAD_LOCAL
    mca_btl_uct_component.bind_threads_to_contexts = true;
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "bind_threads_to_contexts",
        "Bind threads to device contexts. "
        "In general this should improve the multi-threaded performance "
        "when threads are used. (default: true)",
        MCA_BASE_VAR_TYPE_BOOL, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_ALL, &mca_btl_uct_component.bind_threads_to_contexts);
#endif

    /* timeout between connection message attempts in µs */
    mca_btl_uct_component.connection_retry_timeout = 2000;
    (void) mca_base_component_var_register(
        &mca_btl_uct_component.super.btl_version, "connection_retry_timeout",
        "Timeout between attempts to send connection messages for connect-to-"
        "endpoint connections. The timeout is measured in µs and is only"
        "necessary when using unreliable transports for connections (ex: UD). "
        "(default: 2000µs)",
        MCA_BASE_VAR_TYPE_UNSIGNED_INT, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_4,
        MCA_BASE_VAR_SCOPE_LOCAL, &mca_btl_uct_component.connection_retry_timeout);

    /* for now we want this component to lose to btl/ugni and btl/vader */
    module->super.btl_exclusivity = MCA_BTL_EXCLUSIVITY_HIGH - 1;

    return mca_btl_base_param_register(&mca_btl_uct_component.super.btl_version, &module->super);
}

static void mca_btl_uct_mem_release_cb(void *buf, size_t length, void *cbdata, bool from_alloc)
{
    ucm_vm_munmap(buf, length);
}

static void mca_btl_uct_component_parse_include_list (const char *value, mca_btl_uct_include_list_t *list) {
    list->list = NULL;
    list->include = true;

    if (value == NULL) {
        return;
    }

    if (value[0] == '^') {
        list->include = false;
        value++;
    }

    list->list = opal_argv_split(value, ',');
}

static void mca_btl_uct_include_list_free (mca_btl_uct_include_list_t *list) {
    opal_argv_free (list->list);
    list->list = NULL;
}

int mca_btl_uct_include_list_rank (const char *name, const mca_btl_uct_include_list_t *list) {
    if (list->list == NULL) {
        return -1;
    }

    for (int i = 0; list->list[i]; ++i) {
        regex_t preg;
 
        BTL_VERBOSE(("evaluating %s vs %s-list item %s", name, list->include ? "include" : "exclude", list->list[i]));
        int rc = regcomp(&preg, list->list[i], REG_ICASE);
        if (0 != rc) {
            char errbuf[256];
            regerror(rc, &preg, errbuf, sizeof(errbuf));
            BTL_ERROR(("when matching name, could not parse regular expression: %s, error: %s", list->list[i], errbuf));
            continue;
        }

        int result = regexec(&preg, name, /*nmatch=*/0, /*pmatch=*/NULL, /*eflags=*/0);
        regfree(&preg);
        if (0 == result) {
            return list->include ? i + 1 : -(i + 1);
        }
    }

    return list->include ? -1 : 1;
}

static int mca_btl_uct_component_open(void)
{
    if (0 == mca_btl_uct_component.num_contexts_per_module) {
        /* use the core count and the number of local processes to determine
         * how many UCT workers to create */
        int core_count = 36;

        (void) opal_hwloc_base_get_topology();
        if (0 > (core_count = hwloc_get_nbobjs_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE))) {
            return OPAL_ERROR;
        }

        if ((uint32_t)core_count <= opal_process_info.num_local_peers || !opal_using_threads()) {
            /* there is probably no benefit to using multiple device contexts when not
             * using threads or oversubscribing the node with mpi processes. */
            mca_btl_uct_component.num_contexts_per_module = 1;
        } else {
            mca_btl_uct_component.num_contexts_per_module = core_count
                                                            / (opal_process_info.num_local_peers
                                                               + 1);
        }
    }

    if (mca_btl_uct_component.num_contexts_per_module > MCA_BTL_UCT_MAX_WORKERS) {
        mca_btl_uct_component.num_contexts_per_module = MCA_BTL_UCT_MAX_WORKERS;
    }

    if (mca_btl_uct_component.disable_ucx_memory_hooks
        && ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
            == ((OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_MUNMAP_SUPPORT)
                & opal_mem_hooks_support_level()))) {
        ucm_set_external_event(UCM_EVENT_VM_UNMAPPED);
        opal_mem_hooks_register_release(mca_btl_uct_mem_release_cb, NULL);
    }

    return OPAL_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */
static int mca_btl_uct_component_close(void)
{
    if (NULL != mca_btl_uct_component.conn_module) {
        mca_btl_uct_finalize (&mca_btl_uct_component.conn_module->super);
        mca_btl_uct_component.conn_module = NULL;
    }

    if (mca_btl_uct_component.disable_ucx_memory_hooks) {
        opal_mem_hooks_unregister_release(mca_btl_uct_mem_release_cb);
    }

    mca_btl_uct_include_list_free (&mca_btl_uct_component.memory_domain_list);
    mca_btl_uct_include_list_free (&mca_btl_uct_component.allowed_transport_list);
    mca_btl_uct_include_list_free (&mca_btl_uct_component.connection_domain_list);

    return OPAL_SUCCESS;
}

static size_t mca_btl_uct_tl_modex_size(mca_btl_uct_tl_t *tl)
{
    const size_t size = strlen(tl->uct_tl_name) + 1;

    if (MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & UCT_IFACE_FLAG_CONNECT_TO_IFACE) {
        /* pad out to a multiple of 4 bytes */
        return (4 + 3 + size + MCA_BTL_UCT_TL_ATTR(tl, 0).device_addr_len
                + MCA_BTL_UCT_TL_ATTR(tl, 0).iface_addr_len)
               & ~3;
    }

    return (4 + 3 + size + MCA_BTL_UCT_TL_ATTR(tl, 0).device_addr_len) & ~3;
}

static size_t mca_btl_uct_module_modex_size(mca_btl_uct_module_t *module)
{
    size_t modex_size = 4 + strlen(module->md_name) + 1;

    if (module->rdma_tl) {
        modex_size += mca_btl_uct_tl_modex_size(module->rdma_tl);
    }

    if (module->am_tl && module->am_tl != module->rdma_tl) {
        modex_size += mca_btl_uct_tl_modex_size(module->am_tl);
    }

    if (module->conn_tl && module->conn_tl != module->rdma_tl && module->conn_tl != module->am_tl) {
        modex_size += mca_btl_uct_tl_modex_size(module->conn_tl);
    }

    return modex_size;
}

static size_t mca_btl_uct_tl_modex_pack(mca_btl_uct_tl_t *tl, uint8_t *modex_data)
{
    mca_btl_uct_device_context_t *dev_context = tl->uct_dev_contexts[0];
    size_t modex_size = mca_btl_uct_tl_modex_size(tl);

    *((uint32_t *) modex_data) = (uint32_t) modex_size;
    modex_data += 4;

    strcpy((char *) modex_data, tl->uct_tl_name);
    modex_data += strlen(tl->uct_tl_name) + 1;

    /* NTH: only the first context is available. i assume the device addresses of the
     * contexts will be the same but they will have different iface addresses. i also
     * am assuming that it doesn't really matter if all remote contexts connect to
     * the same endpoint since we are only doing RDMA. if any of these assumptions are
     * wrong then we can't delay creating the other contexts and must include their
     * information in the modex. */
    if (MCA_BTL_UCT_TL_ATTR(tl, 0).cap.flags & UCT_IFACE_FLAG_CONNECT_TO_IFACE) {
        uct_iface_get_address(dev_context->uct_iface, (uct_iface_addr_t *) modex_data);
        modex_data += MCA_BTL_UCT_TL_ATTR(tl, 0).iface_addr_len;
    }

    uct_iface_get_device_address(dev_context->uct_iface, (uct_device_addr_t *) modex_data);
    modex_data += MCA_BTL_UCT_TL_ATTR(tl, 0).device_addr_len;

    return modex_size;
}

static uint8_t *mca_btl_uct_modex_pack(mca_btl_uct_module_t *module, uint8_t *modex_data)
{
    size_t name_len = strlen(module->md_name);

    /* pack the size */
    *((uint32_t *) modex_data) = (uint32_t) mca_btl_uct_module_modex_size(module);

    modex_data += 4;

    strcpy((char *) modex_data, module->md_name);
    modex_data += name_len + 1;

    if (module->rdma_tl) {
        modex_data += mca_btl_uct_tl_modex_pack(module->rdma_tl, modex_data);
    }

    if (module->am_tl && module->am_tl != module->rdma_tl) {
        modex_data += mca_btl_uct_tl_modex_pack(module->am_tl, modex_data);
    }

    if (module->conn_tl && module->conn_tl != module->rdma_tl
        && module->conn_tl != module->am_tl) {
        modex_data += mca_btl_uct_tl_modex_pack(module->conn_tl, modex_data);
    }

    return modex_data;
}

static int mca_btl_uct_modex_send(void)
{
    size_t modex_size = sizeof(mca_btl_uct_modex_t);
    mca_btl_uct_modex_t *modex;
    uint8_t *modex_data;
    int rc;

    for (int i = 0; i < mca_btl_uct_component.module_count; ++i) {
        modex_size += mca_btl_uct_module_modex_size(mca_btl_uct_component.modules[i]);
    }

    if (mca_btl_uct_component.conn_module != NULL) {
        modex_size += mca_btl_uct_module_modex_size(mca_btl_uct_component.conn_module);
    }

    modex = alloca(modex_size);
    modex_data = modex->data;

    modex->module_count = mca_btl_uct_component.module_count;

    for (int i = 0; i < mca_btl_uct_component.module_count; ++i) {
        modex_data = mca_btl_uct_modex_pack (mca_btl_uct_component.modules[i], modex_data);
    }

    if (mca_btl_uct_component.conn_module != NULL) {
        ++modex->module_count;
        modex_data = mca_btl_uct_modex_pack (mca_btl_uct_component.conn_module, modex_data);
    }

    OPAL_MODEX_SEND(rc, PMIX_GLOBAL, &mca_btl_uct_component.super.btl_version, modex, modex_size);
    return rc;
}

static mca_btl_uct_module_t *mca_btl_uct_alloc_module(const char *md_name, mca_btl_uct_md_t *md,
                                                      size_t registration_size)
{
    mca_btl_uct_module_t *module;
    ucs_status_t ucs_status;

    module = malloc(sizeof(*module));
    if (NULL == module) {
        return NULL;
    }

    /* copy the module template */
    *module = mca_btl_uct_module_template;

    OBJ_CONSTRUCT(&module->id_to_endpoint, opal_hash_table_t);
    OBJ_CONSTRUCT(&module->endpoint_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&module->short_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&module->eager_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&module->max_frags, opal_free_list_t);
    OBJ_CONSTRUCT(&module->pending_frags, opal_list_t);
    OBJ_CONSTRUCT(&module->lock, opal_recursive_mutex_t);
    OBJ_CONSTRUCT(&module->pending_connection_reqs, opal_fifo_t);

    module->md = md;
    module->md_name = strdup(md_name);
    module->super.btl_registration_handle_size = registration_size;

    ucs_status = ucs_async_context_create(UCS_ASYNC_MODE_THREAD, &module->ucs_async);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("Could not create a UCT async context"));
        mca_btl_uct_finalize(&module->super);
        return NULL;
    }

    return module;
}

ucs_status_t mca_btl_uct_am_handler(void *arg, void *data, size_t length, unsigned flags)
{
    mca_btl_uct_device_context_t *tl_context = (mca_btl_uct_device_context_t *) arg;
    mca_btl_uct_module_t *uct_btl = tl_context->uct_btl;
    mca_btl_uct_am_header_t *header = (mca_btl_uct_am_header_t *) data;
    if (header->data.tag == 0xff) {
        fprintf (stderr, "%d: got an invalid tag\n");
        while (true) {}
    }
    mca_btl_active_message_callback_t *reg = mca_btl_base_active_message_trigger + header->data.tag;
    mca_btl_base_segment_t seg = {.seg_addr = {.pval = (void *) ((intptr_t) data
                                                                 + sizeof(*header))},
                                  .seg_len = length - sizeof(*header)};
    mca_btl_base_receive_descriptor_t desc = {.endpoint = NULL,
                                              .des_segments = &seg,
                                              .des_segment_count = 1,
                                              .tag = header->data.tag,
                                              .cbdata = reg->cbdata};

    /* prevent recursion */
    tl_context->in_am_callback = true;
    reg->cbfunc(&uct_btl->super, &desc);
    tl_context->in_am_callback = false;
    header->data.tag = 0xff;

    return UCS_OK;
}

#if UCT_API >= UCT_VERSION(1, 7)
static int mca_btl_uct_component_process_uct_md(uct_component_h component,
                                                uct_md_resource_desc_t *md_desc)
#else
static int mca_btl_uct_component_process_uct_md(uct_md_resource_desc_t *md_desc)
#endif
{
    mca_rcache_base_resources_t rcache_resources;
    uct_tl_resource_desc_t *tl_desc;
    mca_btl_uct_module_t *module;
    uct_md_config_t *uct_config;
    uct_md_attr_t md_attr;
    mca_btl_uct_md_t *md;
    int list_rank;
    unsigned num_tls;
    char *tmp;
    ucs_status_t ucs_status;
    int connection_list_rank = -1;
    bool consider_for_connection_module = false;

    BTL_VERBOSE(("processing memory domain %s", md_desc->md_name));

    if (MCA_BTL_UCT_MAX_MODULES == mca_btl_uct_component.module_count) {
        BTL_VERBOSE(("created the maximum number of allowable modules"));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    BTL_VERBOSE(("checking if %s should be used for communication", md_desc->md_name));
    list_rank = mca_btl_uct_include_list_rank (md_desc->md_name, &mca_btl_uct_component.memory_domain_list);

    if (list_rank < 0) {
        BTL_VERBOSE(("checking if %s should be used for connections", md_desc->md_name));
        connection_list_rank = mca_btl_uct_include_list_rank (md_desc->md_name, &mca_btl_uct_component.connection_domain_list);

        if (connection_list_rank < 0) {
            /* nothing to do */
            BTL_VERBOSE(("not continuing with memory domain %s", md_desc->md_name));
            return OPAL_SUCCESS;
        }

        BTL_VERBOSE(("will be considering domain %s for connections only", md_desc->md_name));
        consider_for_connection_module = true;
    }

    md = OBJ_NEW(mca_btl_uct_md_t);

#if UCT_API >= UCT_VERSION(1, 7)
    ucs_status = uct_md_config_read(component, NULL, NULL, &uct_config);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_config_read failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }
    ucs_status = uct_md_open(component, md_desc->md_name, uct_config, &md->uct_md);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_open failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }
#else
    ucs_status = uct_md_config_read(md_desc->md_name, NULL, NULL, &uct_config);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_config_read failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }
    ucs_status = uct_md_open(md_desc->md_name, uct_config, &md->uct_md);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_open failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }
#endif
    uct_config_release(uct_config);

    ucs_status = uct_md_query(md->uct_md, &md_attr);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_config_release failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }
    ucs_status = uct_md_query_tl_resources(md->uct_md, &tl_desc, &num_tls);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_config_release failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    module = mca_btl_uct_alloc_module(md_desc->md_name, md, md_attr.rkey_packed_size);
    if (NULL == module) {
        uct_release_tl_resource_list(tl_desc);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* if this module is not to be used for communication check if it has a transport suitable 
     * for forming connections. */
    (void) mca_btl_uct_query_tls(module, md, tl_desc, num_tls, consider_for_connection_module);

    uct_release_tl_resource_list(tl_desc);

    /* release the initial reference to the md object. if any modules were created the UCT md will
     * remain open until those modules are finalized. */
    OBJ_RELEASE(md);

    if (NULL == module->am_tl && NULL == module->rdma_tl && (NULL == module->conn_tl || !consider_for_connection_module)) {
        BTL_VERBOSE(("uct memory domain %s does not have any appropriate tls", md_desc->md_name));
        mca_btl_uct_finalize(&module->super);
        return OPAL_ERR_NOT_AVAILABLE;
    }

#if UCT_API >= UCT_VERSION(1, 7)
    module->uct_component = component;
#endif

    if (!consider_for_connection_module) {
        mca_btl_uct_component.modules[mca_btl_uct_component.module_count++] = module;

        /* NTH: a registration cache shouldn't be necessary when using UCT but there are measurable
         * performance benefits to using rcache/grdma instead of assuming UCT will do the right
         * thing. */
        (void) opal_asprintf(&tmp, "uct.%s", module->md_name);

        rcache_resources.cache_name = tmp;
        rcache_resources.reg_data = (void *) module;
        rcache_resources.sizeof_reg = sizeof(mca_btl_uct_reg_t)
            + module->super.btl_registration_handle_size;
        rcache_resources.register_mem = mca_btl_uct_reg_mem;
        rcache_resources.deregister_mem = mca_btl_uct_dereg_mem;

        module->rcache = mca_rcache_base_module_create("grdma", module, &rcache_resources);
        free(tmp);
        if (NULL == module->rcache) {
            /* something when horribly wrong */
            BTL_VERBOSE(("could not allocate a registration cache for this btl module"));
            mca_btl_uct_finalize(&module->super);
            return OPAL_ERROR;
        }
    } else {
        if (NULL == mca_btl_uct_component.conn_module) {
            BTL_VERBOSE(("memory domain %s may be used for connections", md_desc->md_name));
            mca_btl_uct_component.conn_module = module;
        } else {
            mca_btl_uct_finalize(&module->super);
        }
    }

    return OPAL_SUCCESS;
}

#if UCT_API >= UCT_VERSION(1, 7)
static int mca_btl_uct_component_process_uct_component(uct_component_h component)
{
    uct_component_attr_t attr = {.field_mask = UCT_COMPONENT_ATTR_FIELD_NAME
                                               | UCT_COMPONENT_ATTR_FIELD_MD_RESOURCE_COUNT};
    ucs_status_t ucs_status;
    int rc;

    ucs_status = uct_component_query(component, &attr);
    if (UCS_OK != ucs_status) {
        return OPAL_ERROR;
    }

    BTL_VERBOSE(("processing uct component %s", attr.name));

    attr.md_resources = calloc(attr.md_resource_count, sizeof(*attr.md_resources));
    attr.field_mask |= UCT_COMPONENT_ATTR_FIELD_MD_RESOURCES;
    ucs_status = uct_component_query(component, &attr);
    if (UCS_OK != ucs_status) {
        return OPAL_ERROR;
    }

    for (unsigned i = 0; i < attr.md_resource_count; ++i) {
        rc = mca_btl_uct_component_process_uct_md(component, attr.md_resources + i);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    free(attr.md_resources);

    return OPAL_SUCCESS;
}
#endif /* UCT_API >= UCT_VERSION(1, 7) */

static void mca_btl_uct_component_validate_modules(void) {
    if (mca_btl_uct_component.conn_module != NULL) {
        /* verify that a connection-only module is required. this might be the case in some systems
         * where rc verbs is avaiable but ud is not. */
        bool need_conn_module = false;
        for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
            mca_btl_uct_module_t *module =  mca_btl_uct_component.modules[i];
            if (module->conn_tl != NULL) {
                continue;
            }
            if ((module->rdma_tl && mca_btl_uct_tl_requires_connection_tl(module->rdma_tl)) ||
                (module->am_tl && mca_btl_uct_tl_requires_connection_tl(module->am_tl))) {
                need_conn_module = true;
                break;
            }
        }

        if (!need_conn_module) {
            mca_btl_uct_finalize (&mca_btl_uct_component.conn_module->super);
            mca_btl_uct_component.conn_module = NULL;
        }
    } else {
        int usable_module_count = mca_btl_uct_component.module_count;

        /* check that all modules can be used */
        for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
            mca_btl_uct_module_t *module =  mca_btl_uct_component.modules[i];
            if (NULL != module->conn_tl) {
                /* module has its own connection transport */
                continue;
            }

            if (((module->rdma_tl && mca_btl_uct_tl_requires_connection_tl(module->rdma_tl)) ||
                 (module->am_tl && mca_btl_uct_tl_requires_connection_tl(module->am_tl)))
                && NULL == module->conn_tl) {
                /* module can not be used */
                BTL_VERBOSE(("module for memory domain %s can not be used due to missing connection transport",
                             module->md_name));
                mca_btl_uct_finalize (&mca_btl_uct_component.modules[i]->super);
                mca_btl_uct_component.modules[i] = NULL;
            }
        }

        /* remove holes in the module array */
        if (usable_module_count < mca_btl_uct_component.module_count) {
            for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
                if (mca_btl_uct_component.modules[i] == NULL) {
                    for (int j = i ; j < mca_btl_uct_component.module_count ; ++j) {
                        mca_btl_uct_component.modules[i++] = mca_btl_uct_component.modules[j];
                    }
                }
            }
            mca_btl_uct_component.module_count = usable_module_count;
        }
    }
}

/*
 *  UCT component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup UCT listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */

static mca_btl_base_module_t **mca_btl_uct_component_init(int *num_btl_modules,
                                                          bool enable_progress_threads,
                                                          bool enable_mpi_threads)
{
    /* for this BTL to be useful the interface needs to support RDMA and certain atomic operations
     */
    struct mca_btl_base_module_t **base_modules;
    ucs_status_t ucs_status;
    int rc;

    BTL_VERBOSE(("initializing uct btl"));

    if (NULL == mca_btl_uct_component.memory_domains
        || 0 == strlen(mca_btl_uct_component.memory_domains)
        || 0 == strcmp(mca_btl_uct_component.memory_domains, "none")) {
        BTL_VERBOSE(("no uct memory domains specified"));
        return NULL;
    }

    mca_btl_uct_component_parse_include_list(mca_btl_uct_component.memory_domains,
                                             &mca_btl_uct_component.memory_domain_list);
    mca_btl_uct_component_parse_include_list(mca_btl_uct_component.allowed_transports,
                                             &mca_btl_uct_component.allowed_transport_list);
    mca_btl_uct_component_parse_include_list(mca_btl_uct_component.connection_domains,
                                             &mca_btl_uct_component.connection_domain_list);

    mca_btl_uct_component.module_count = 0;

#if UCT_API >= UCT_VERSION(1, 7)
    uct_component_h *components;
    unsigned num_components;

    ucs_status = uct_query_components(&components, &num_components);
    if (UCS_OK != ucs_status) {
        BTL_ERROR(("could not query UCT components"));
        return NULL;
    }

    /* generate all suitable btl modules */
    for (unsigned i = 0; i < num_components; ++i) {
        rc = mca_btl_uct_component_process_uct_component(components[i]);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    uct_release_component_list(components);

#else /* UCT 1.6 and older */
    uct_md_resource_desc_t *resources;
    unsigned resource_count;

    uct_query_md_resources(&resources, &resource_count);

    /* generate all suitable btl modules */
    for (unsigned i = 0; i < resource_count; ++i) {
        rc = mca_btl_uct_component_process_uct_md(resources + i);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    uct_release_md_resource_list(resources);

#endif /* UCT_API >= UCT_VERSION(1, 7) */

    /* filter out unusable modules before sending the modex */
    mca_btl_uct_component_validate_modules();

    mca_btl_uct_modex_send();

    /* pass module array back to caller */
    base_modules = calloc(mca_btl_uct_component.module_count, sizeof(*base_modules));
    if (NULL == base_modules) {
        return NULL;
    }

    memcpy(base_modules, mca_btl_uct_component.modules,
           mca_btl_uct_component.module_count * sizeof(mca_btl_uct_component.modules[0]));

    *num_btl_modules = mca_btl_uct_component.module_count;

    BTL_VERBOSE(("uct btl initialization complete. found %d suitable memory domains",
                 mca_btl_uct_component.module_count));

    return base_modules;
}

static int mca_btl_uct_tl_progress(mca_btl_uct_tl_t *tl, int starting_index)
{
    unsigned int ret = 0;

    if (NULL == tl) {
        return 0;
    }

    for (int j = 0; j < tl->max_device_contexts; ++j) {
        if (tl->uct_dev_contexts[j]) {
            ret += mca_btl_uct_context_progress(tl->uct_dev_contexts[j]);
        }
    }

    return ret;
}

static int mca_btl_uct_component_progress_pending(mca_btl_uct_module_t *uct_btl)
{
    mca_btl_uct_base_frag_t *frag, *next;
    int completed = 0;
    size_t count;

    if (0 == (count = opal_list_get_size(&uct_btl->pending_frags))) {
        return 0;
    }

    OPAL_THREAD_LOCK(&uct_btl->lock);
    OPAL_LIST_FOREACH_SAFE (frag, next, &uct_btl->pending_frags, mca_btl_uct_base_frag_t) {
        if (!frag->ready) {
            continue;
        }

        opal_list_remove_item(&uct_btl->pending_frags, (opal_list_item_t *) frag);

        if (OPAL_SUCCESS > mca_btl_uct_send_frag(uct_btl, frag, false)) {
            opal_list_prepend(&uct_btl->pending_frags, (opal_list_item_t *) frag);
        } else {
            completed++;
        }
    }
    OPAL_THREAD_UNLOCK(&uct_btl->lock);

    return completed;
}

static int mca_btl_uct_component_progress_connections (mca_btl_uct_module_t *module) {
    mca_btl_uct_pending_connection_request_t *request;
    int ret;

    if (module->conn_tl == NULL) {
        return 0;
    }

    ret = mca_btl_uct_tl_progress(module->conn_tl, 0);

    while (NULL
           != (request = (mca_btl_uct_pending_connection_request_t *) opal_fifo_pop_atomic(
                                                                                           &module->pending_connection_reqs))) {
        mca_btl_uct_conn_req_t *conn_req = (mca_btl_uct_conn_req_t *) request->request_data;
        BTL_VERBOSE(("processing connection request...."));
        for (int i = 0; i < mca_btl_uct_component.module_count; ++i) {
            if (0 == strncmp(mca_btl_uct_component.modules[i]->md_name, conn_req->module_name, sizeof(conn_req->module_name) - 1)) {
                module = mca_btl_uct_component.modules[i];
                break;
            }
        }
        int rc = mca_btl_uct_process_connection_request(module, conn_req);
        if (rc != OPAL_SUCCESS) {
            opal_fifo_push_atomic(&module->pending_connection_reqs, &request->super);
            break;
        }
        OBJ_RELEASE(request);
    }

    return ret;
}

/**
 * @brief UCT BTL progress function
 *
 * This function explicitly progresses all workers.
 */
static int mca_btl_uct_component_progress(void)
{
    int starting_index = mca_btl_uct_get_context_index();
    unsigned ret = 0;

    for (int i = 0; i < mca_btl_uct_component.module_count; ++i) {
        mca_btl_uct_module_t *module = mca_btl_uct_component.modules[i];

        /* unlike ucp, uct actually tells us something useful! its almost like it was "inspired"
         * by the btl progress functions.... */
        ret += mca_btl_uct_tl_progress(module->rdma_tl, starting_index);

        if (module->am_tl != module->rdma_tl) {
            ret += mca_btl_uct_tl_progress(module->am_tl, starting_index);
        }

        mca_btl_uct_component_progress_connections (module);
 
        if (0 != opal_list_get_size(&module->pending_frags)) {
            mca_btl_uct_component_progress_pending(module);
        }
    }

    if (NULL != mca_btl_uct_component.conn_module) {
        ret += mca_btl_uct_component_progress_connections (mca_btl_uct_component.conn_module);
    }

    return (int) ret;
}

/** UCT btl component */
mca_btl_uct_component_t mca_btl_uct_component = {
    .super = {
        .btl_version =
            {
                MCA_BTL_DEFAULT_VERSION("uct"),
                .mca_open_component = mca_btl_uct_component_open,
                .mca_close_component = mca_btl_uct_component_close,
                .mca_register_component_params = mca_btl_uct_component_register,
            },
        .btl_data =
            {/* The component is not checkpoint ready */
             .param_field = MCA_BASE_METADATA_PARAM_NONE},

        .btl_init = mca_btl_uct_component_init,
        .btl_progress = mca_btl_uct_component_progress,
    }};
MCA_BASE_COMPONENT_INIT(opal, btl, uct)

static void safety_valve(void) __opal_attribute_destructor__;
void safety_valve(void) {
    opal_mem_hooks_unregister_release(mca_btl_uct_mem_release_cb);
}
