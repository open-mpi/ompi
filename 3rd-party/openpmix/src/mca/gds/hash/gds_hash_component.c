/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
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

#include "src/include/pmix_config.h"
#include "pmix_common.h"

#include "src/class/pmix_hash_table.h"
#include "src/util/pmix_hash.h"

#include "gds_hash.h"
#include "src/mca/gds/gds.h"

static pmix_status_t component_query(pmix_mca_base_module_t **module, int *priority);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
pmix_gds_hash_component_t pmix_mca_gds_hash_component = {
    .super = {
        PMIX_GDS_BASE_VERSION_1_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "hash",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PMIX_MAJOR_VERSION,
                                   PMIX_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_query_component = component_query,
        .reserved = {0}
    },
    .mysessions = PMIX_LIST_STATIC_INIT,
    .myjobs = PMIX_LIST_STATIC_INIT
};

static int component_query(pmix_mca_base_module_t **module, int *priority)
{
    *priority = 10;
    *module = (pmix_mca_base_module_t *) &pmix_hash_module;
    return PMIX_SUCCESS;
}

/**********************************************/
/* class instantiations */
static void scon(pmix_session_t *s)
{
    s->session = UINT32_MAX;
    PMIX_CONSTRUCT(&s->sessioninfo, pmix_list_t);
    PMIX_CONSTRUCT(&s->nodeinfo, pmix_list_t);
}
static void sdes(pmix_session_t *s)
{
    PMIX_LIST_DESTRUCT(&s->sessioninfo);
    PMIX_LIST_DESTRUCT(&s->nodeinfo);
}
PMIX_CLASS_INSTANCE(pmix_session_t, pmix_list_item_t, scon, sdes);

static void htcon(pmix_job_t *p)
{
    p->ns = NULL;
    p->nptr = NULL;
    PMIX_CONSTRUCT(&p->jobinfo, pmix_list_t);
    PMIX_CONSTRUCT(&p->internal, pmix_hash_table_t);
    pmix_hash_table_init(&p->internal, 256);
    p->internal.ht_label = "internal";
    PMIX_CONSTRUCT(&p->remote, pmix_hash_table_t);
    pmix_hash_table_init(&p->remote, 256);
    p->remote.ht_label = "remote";
    PMIX_CONSTRUCT(&p->local, pmix_hash_table_t);
    pmix_hash_table_init(&p->local, 256);
    p->local.ht_label = "local";
    p->gdata_added = false;
    PMIX_CONSTRUCT(&p->apps, pmix_list_t);
    PMIX_CONSTRUCT(&p->nodeinfo, pmix_list_t);
    p->session = NULL;
}
static void htdes(pmix_job_t *p)
{
    if (NULL != p->ns) {
        free(p->ns);
    }
    if (NULL != p->nptr) {
        PMIX_RELEASE(p->nptr);
    }
    PMIX_LIST_DESTRUCT(&p->jobinfo);
    pmix_hash_remove_data(&p->internal, PMIX_RANK_WILDCARD, NULL, NULL);
    PMIX_DESTRUCT(&p->internal);
    pmix_hash_remove_data(&p->remote, PMIX_RANK_WILDCARD, NULL, NULL);
    PMIX_DESTRUCT(&p->remote);
    pmix_hash_remove_data(&p->local, PMIX_RANK_WILDCARD, NULL, NULL);
    PMIX_DESTRUCT(&p->local);
    PMIX_LIST_DESTRUCT(&p->apps);
    PMIX_LIST_DESTRUCT(&p->nodeinfo);
    if (NULL != p->session) {
        PMIX_RELEASE(p->session);
    }
}
PMIX_CLASS_INSTANCE(pmix_job_t, pmix_list_item_t, htcon, htdes);

static void apcon(pmix_apptrkr_t *p)
{
    p->appnum = 0;
    PMIX_CONSTRUCT(&p->appinfo, pmix_list_t);
    PMIX_CONSTRUCT(&p->nodeinfo, pmix_list_t);
    p->job = NULL;
}
static void apdes(pmix_apptrkr_t *p)
{
    PMIX_LIST_DESTRUCT(&p->appinfo);
    PMIX_LIST_DESTRUCT(&p->nodeinfo);
}
PMIX_CLASS_INSTANCE(pmix_apptrkr_t, pmix_list_item_t, apcon, apdes);

static void ndinfocon(pmix_nodeinfo_t *p)
{
    p->nodeid = UINT32_MAX;
    p->hostname = NULL;
    p->aliases = NULL;
    PMIX_CONSTRUCT(&p->info, pmix_list_t);
}
static void ndinfodes(pmix_nodeinfo_t *p)
{
    if (NULL != p->hostname) {
        free(p->hostname);
    }
    if (NULL != p->aliases) {
        PMIx_Argv_free(p->aliases);
    }
    PMIX_LIST_DESTRUCT(&p->info);
}
PMIX_CLASS_INSTANCE(pmix_nodeinfo_t, pmix_list_item_t, ndinfocon, ndinfodes);
