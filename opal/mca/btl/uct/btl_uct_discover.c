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

#include "opal_config.h"

#include "btl_uct_device_context.h"
#include "btl_uct_discover.h"
#include "btl_uct_include_list.h"

#include "btl_uct.h"
#include "opal/class/opal_list.h"
#include "opal/util/printf.h"

#if UCT_API >= UCT_VERSION(1, 7)
static int mca_btl_uct_component_process_uct_md(uct_component_h component,
                                                uct_md_resource_desc_t *md_desc)
#else
static int mca_btl_uct_component_process_uct_md(uct_md_resource_desc_t *md_desc)
#endif
{
    uct_tl_resource_desc_t *tl_desc;
    uct_md_config_t *uct_config;
    mca_btl_uct_md_t *md;
    int list_rank;
    unsigned num_tls;
    ucs_status_t ucs_status;
    int connection_list_rank = -1;
    bool consider_for_connection_module = false;

    BTL_VERBOSE(("processing memory domain %s", md_desc->md_name));

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
    md->md_name = strdup(md_desc->md_name);
#if UCT_API >= UCT_VERSION(1, 7)
    md->uct_component = component;
#endif
    md->connection_only_domain = consider_for_connection_module;

#if UCT_API >= UCT_VERSION(1, 7)
    ucs_status = uct_md_config_read(component, NULL, NULL, &uct_config);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_config_read failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    ucs_status = uct_md_open(component, md->md_name, uct_config, &md->uct_md);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_open failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }
#else
    ucs_status = uct_md_config_read(md->md_name, NULL, NULL, &uct_config);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_config_read failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    ucs_status = uct_md_open(md->md_name, uct_config, &md->uct_md);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_md_open failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }
#endif
    uct_config_release(uct_config);

    ucs_status = uct_md_query(md->uct_md, &md->md_attr);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_config_release failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    ucs_status = uct_md_query_tl_resources(md->uct_md, &tl_desc, &num_tls);
    if (UCS_OK != ucs_status) {
        BTL_VERBOSE(("uct_config_release failed %d (%s)", ucs_status, ucs_status_string(ucs_status)));
        return OPAL_ERR_NOT_AVAILABLE;
    }

    (void) mca_btl_uct_populate_tls(md, tl_desc, num_tls);

    uct_release_tl_resource_list(tl_desc);
    opal_list_append(&mca_btl_uct_component.md_list, &md->super);

    return OPAL_SUCCESS;
}

#if UCT_API >= UCT_VERSION(1, 7)
static int mca_btl_uct_component_process_uct_component(uct_component_h component)
{
    uct_component_attr_t attr = {
        .field_mask = UCT_COMPONENT_ATTR_FIELD_NAME
            | UCT_COMPONENT_ATTR_FIELD_MD_RESOURCE_COUNT,
    };
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

int mca_btl_uct_component_discover_mds(void)
{
    mca_btl_uct_include_list_parse(mca_btl_uct_component.memory_domains,
                                   &mca_btl_uct_component.memory_domain_list);
    mca_btl_uct_include_list_parse(mca_btl_uct_component.connection_domains,
                                   &mca_btl_uct_component.connection_domain_list);

#if UCT_API >= UCT_VERSION(1, 7)
    ucs_status_t ucs_status = uct_query_components(&mca_btl_uct_component.uct_components,
                                                   &mca_btl_uct_component.num_uct_components);
    if (UCS_OK != ucs_status) {
        BTL_ERROR(("could not query UCT components"));
        return OPAL_ERROR;
    }

    /* generate list of memory domains */
    for (unsigned i = 0; i < mca_btl_uct_component.num_uct_components; ++i) {
        int rc = mca_btl_uct_component_process_uct_component(mca_btl_uct_component.uct_components[i]);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }
#else /* UCT 1.6 and older */
    uct_md_resource_desc_t *resources;
    unsigned resource_count;

    uct_query_md_resources(&resources, &resource_count);

    /* generate all suitable btl modules */
    for (unsigned i = 0; i < resource_count; ++i) {
        int rc = mca_btl_uct_component_process_uct_md(resources + i);
        if (OPAL_SUCCESS != rc) {
            break;
        }
    }

    uct_release_md_resource_list(resources);

#endif /* UCT_API >= UCT_VERSION(1, 7) */

    return OPAL_SUCCESS;
}

static int mca_btl_uct_module_register_mca_var(mca_btl_uct_module_t *module)
{
    mca_base_component_t dummy_component;
    /* mca_btl_uct_component starts with an mca_base_component_t structure */
    memcpy(&dummy_component, &mca_btl_uct_component, sizeof(dummy_component));
    snprintf(dummy_component.mca_component_name, sizeof(dummy_component.mca_component_name),
            "uct_%s", module->md->md_name);

    BTL_VERBOSE(("registering MCA parameters for module uct_%s", module->md->md_name));

    module->allowed_transports = mca_btl_uct_component.allowed_transports;
    (void) mca_base_component_var_register(
        &dummy_component, "transports",
        "Comma-delimited list of transports to use sorted by increasing "
        "priority. The list of transports available can be queried using ucx_info. Special"
        "values: any (any available) (default: dc_mlx5,rc_mlx5,ud,any)",
        MCA_BASE_VAR_TYPE_STRING, NULL, 0, MCA_BASE_VAR_FLAG_SETTABLE, OPAL_INFO_LVL_3,
        MCA_BASE_VAR_SCOPE_LOCAL, &module->allowed_transports);

    return mca_btl_base_param_register(&dummy_component, &module->super);
}

static int tl_compare(opal_list_item_t **a, opal_list_item_t **b)
{
    mca_btl_uct_tl_t *tl_a = (mca_btl_uct_tl_t *) *a;
    mca_btl_uct_tl_t *tl_b = (mca_btl_uct_tl_t *) *b;

    return tl_a->priority - tl_b->priority;
}

static int mca_btl_uct_generate_module(mca_btl_uct_md_t *md)
{
    mca_btl_uct_tl_t *tl;
    mca_btl_uct_module_t *module = mca_btl_uct_alloc_module(md, md->md_attr.rkey_packed_size);

    BTL_VERBOSE(("attempting to create a BTL module for memory domain: %s", md->md_name));

    int rc = mca_btl_uct_module_register_mca_var(module);
    if (OPAL_SUCCESS != rc) {
        mca_btl_uct_finalize(&module->super);
        return rc;
    }

    mca_btl_uct_include_list_parse(module->allowed_transports,
                                   &module->allowed_transport_list);
    mca_btl_uct_tl_t *next;
    OPAL_LIST_FOREACH_SAFE (tl, next, &md->tls, mca_btl_uct_tl_t) {
        int rank = mca_btl_uct_include_list_rank(tl->uct_tl_name, &module->allowed_transport_list);
        if (rank < 0) {
            opal_list_remove_item(&md->tls, &tl->super);
            OBJ_RELEASE(tl);
            continue;
        }
        tl->priority = rank;
    }

    opal_list_sort(&md->tls, tl_compare);

    /* Treat the flags specified by the user as a mask. */
    uint32_t btl_flags = module->super.btl_flags;
    uint32_t btl_atomic_flags = module->super.btl_atomic_flags;

    module->super.btl_flags = 0;
    module->super.btl_atomic_flags = 0;

    OPAL_LIST_FOREACH (tl, &md->tls, mca_btl_uct_tl_t) {
        mca_btl_uct_evaluate_tl(module, tl);
        if (NULL != module->am_tl && NULL != module->rdma_tl) {
            /* all done */
            break;
        }
    }

    module->super.btl_flags &= btl_flags;
    module->super.btl_atomic_flags &= btl_atomic_flags;

    if (NULL == module->rdma_tl) {
        /* no rdma tls */
        BTL_VERBOSE(("no rdma tl matched supplied filter. disabling RDMA support"));

        module->super.btl_flags &= ~MCA_BTL_FLAGS_RDMA;
        module->super.btl_put = NULL;
        module->super.btl_get = NULL;
        module->super.btl_atomic_fop = NULL;
        module->super.btl_atomic_op = NULL;
    }

    if (NULL == module->am_tl) {
        /* no active message tls == no send/recv */
        BTL_VERBOSE(("no active message tl matched supplied filter. disabling send/recv support"));

        module->super.btl_send = NULL;
        module->super.btl_sendi = NULL;
        module->super.btl_alloc = NULL;
        module->super.btl_free = NULL;
    }

    if (NULL == module->am_tl && NULL == module->rdma_tl) {
        mca_btl_uct_finalize(&module->super);
        return OPAL_ERR_NOT_AVAILABLE;
    }

    module->module_index = mca_btl_uct_component.module_count;
    mca_btl_uct_component.modules[mca_btl_uct_component.module_count++] = module;

    return OPAL_SUCCESS;
}

static void mca_btl_uct_enable_tl(mca_btl_uct_module_t *module, mca_btl_uct_tl_t *tl) {
    if (NULL == tl) {
        return;
    } 

    if (tl == module->am_tl) {
        mca_btl_uct_device_context_t *context =
            mca_btl_uct_module_get_tl_context_specific(module, tl, /*context_id=*/0);
        /* If this context was created before a module was created it may not
         * have an active message handler installed. Attempt to install one now. */
        mca_btl_uct_context_enable_am_handler(tl, context);
    }

    if (tl->max_device_contexts < 1) {
        tl->max_device_contexts = mca_btl_uct_component.num_contexts_per_module;
    }
}

static int mca_btl_uct_enable_module(mca_btl_uct_module_t *module)
{
    /* NTH: a registration cache shouldn't be necessary when using UCT but there are measurable
     * performance benefits to using rcache/grdma instead of assuming UCT will do the right
     * thing. */
    char *tmp = NULL;
    (void) opal_asprintf(&tmp, "uct.%s", module->md->md_name);

    mca_rcache_base_resources_t rcache_resources = {
        .cache_name = tmp,
        .reg_data = (void *) module,
        .sizeof_reg = sizeof(mca_btl_uct_reg_t) + module->super.btl_registration_handle_size,
        .register_mem = mca_btl_uct_reg_mem,
        .deregister_mem = mca_btl_uct_dereg_mem,
    };

    module->rcache = mca_rcache_base_module_create("grdma", module, &rcache_resources);
    free(tmp);
    if (NULL == module->rcache) {
        /* something went horribly wrong */
        BTL_VERBOSE(("could not allocate a registration cache for this btl module"));
        return OPAL_ERROR;
    }

    mca_btl_uct_enable_tl(module, module->rdma_tl);
    mca_btl_uct_enable_tl(module, module->am_tl);

    return OPAL_SUCCESS;
}

int mca_btl_uct_enable_modules(mca_btl_uct_module_t **modules, int module_count)
{
    for (int i = 0 ; i < module_count ; ++i) {
        int rc = mca_btl_uct_enable_module(modules[i]);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("could not enable module for memory domain %s", modules[i]->md->md_name));
            mca_btl_uct_finalize(&modules[i]->super);
        }
    }

    return OPAL_SUCCESS;
}

int mca_btl_uct_component_generate_modules(opal_list_t *md_list)
{
    mca_btl_uct_component.module_count = 0;

    mca_btl_uct_md_t *md;
    OPAL_LIST_FOREACH(md, md_list, mca_btl_uct_md_t) {
        if (MCA_BTL_UCT_MAX_MODULES == mca_btl_uct_component.module_count) {
            BTL_VERBOSE(("created the maximum number of allowable modules"));
            break;
        }

        if (md->connection_only_domain) {
            /* will not build a module for this domain */
            continue;
        }

        int rc = mca_btl_uct_generate_module(md);
        if (OPAL_SUCCESS != rc) {
            BTL_VERBOSE(("could not create a module for memory domain %s", md->md_name));
        }
    }

    return OPAL_SUCCESS;
}

int mca_btl_uct_component_maybe_setup_conn_tl(void)
{
    bool connection_tl_required = false;
    for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
        connection_tl_required |=
            mca_btl_uct_tl_requires_connection_tl(mca_btl_uct_component.modules[i]->am_tl);
        connection_tl_required |=
            mca_btl_uct_tl_requires_connection_tl(mca_btl_uct_component.modules[i]->rdma_tl);
        if (connection_tl_required) {
            break;
        }
    }

    if (!connection_tl_required) {
        return OPAL_SUCCESS;
    }

    mca_btl_uct_md_t *md;
    OPAL_LIST_FOREACH(md, &mca_btl_uct_component.md_list, mca_btl_uct_md_t) {
        mca_btl_uct_tl_t *tl, *next;
        OPAL_LIST_FOREACH_SAFE(tl, next, &md->tls, mca_btl_uct_tl_t) {
            if (mca_btl_uct_tl_supports_conn(tl)) {
                break;
            }
            tl = NULL;
        }

        if ((opal_list_item_t *) tl == &md->tls.opal_list_sentinel) {
            BTL_VERBOSE(("No suitable connection tls in md %s", md->md_name));
            continue;
        }

        if (NULL == mca_btl_uct_component.conn_tl) {
            mca_btl_uct_component.conn_tl = tl;
        }

        if (tl != NULL && (md->connection_only_domain || NULL == mca_btl_uct_component.conn_tl)) {
            mca_btl_uct_component.conn_tl = tl;
            if (md->connection_only_domain) {
                /* not going do to better */
                break;
            }
        }
    }

    if (NULL == mca_btl_uct_component.conn_tl) {
        /* no connection tl found, will need to disable all connect-to-endpoint modules */
        BTL_VERBOSE(("could not find a suitable transport to support forming connections"));
        return OPAL_ERR_NOT_FOUND;
    }

    BTL_VERBOSE(("using transport %s::%s for connection management",
                 mca_btl_uct_component.conn_tl->uct_md->md_name,
                 mca_btl_uct_component.conn_tl->uct_tl_name));

    return mca_btl_uct_enable_tl_conn(mca_btl_uct_component.conn_tl);
}

int mca_btl_uct_component_filter_mds(void)
{
    int usable_module_count = mca_btl_uct_component.module_count;
    /* clean out all unused mds, tls, and unusable modules */
    if (NULL == mca_btl_uct_component.conn_tl) {
        for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
            mca_btl_uct_module_t *module = mca_btl_uct_component.modules[i];
            if (!(mca_btl_uct_tl_requires_connection_tl(module->am_tl) ||
                  mca_btl_uct_tl_requires_connection_tl(module->rdma_tl))) {
                continue;
            }

            /* module is unusable */
            mca_btl_uct_finalize(&module->super);
            mca_btl_uct_component.modules[i] = NULL;
            --usable_module_count;
        }
    }

    mca_btl_uct_md_t *md, *md_next;
    OPAL_LIST_FOREACH_SAFE(md, md_next, &mca_btl_uct_component.md_list, mca_btl_uct_md_t) {
        mca_btl_uct_module_t *module = NULL;
        for (int i = 0 ; i < mca_btl_uct_component.module_count ; ++i) {
            module = mca_btl_uct_component.modules[i];
            if (NULL != module && module->md == md) {
                break;
            }
            module = NULL;
        }

        mca_btl_uct_tl_t *tl, *next;
        OPAL_LIST_FOREACH_SAFE(tl, next, &md->tls, mca_btl_uct_tl_t) {
            if (tl == mca_btl_uct_component.conn_tl || (NULL != module &&
                                                        (tl == module->rdma_tl ||
                                                         tl == module->am_tl))) {
                /* tl is in use */
                continue;
            }
            opal_list_remove_item(&md->tls, &tl->super);
            OBJ_RELEASE(tl);
        }

        if (opal_list_get_size(&md->tls) == 0) {
            opal_list_remove_item(&mca_btl_uct_component.md_list, &md->super);
            OBJ_RELEASE(md);
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

    return OPAL_SUCCESS;
}
