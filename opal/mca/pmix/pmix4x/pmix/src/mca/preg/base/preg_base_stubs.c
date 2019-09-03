/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/include/pmix_globals.h"
#include "src/client/pmix_client_ops.h"
#include "src/mca/preg/base/base.h"

pmix_status_t pmix_preg_base_generate_node_regex(const char *input,
                                                 char **regex)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->generate_node_regex) {
            if (PMIX_SUCCESS == active->module->generate_node_regex(input, regex)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_generate_ppn(const char *input,
                                          char **ppn)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->generate_ppn) {
            if (PMIX_SUCCESS == active->module->generate_ppn(input, ppn)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_parse_nodes(const char *regexp,
                                         char ***names)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->parse_nodes) {
            if (PMIX_SUCCESS == active->module->parse_nodes(regexp, names)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_parse_procs(const char *regexp,
                                         char ***procs)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->parse_procs) {
            if (PMIX_SUCCESS == active->module->parse_procs(regexp, procs)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_resolve_peers(const char *nodename,
                                           const char *nspace,
                                           pmix_proc_t **procs, size_t *nprocs)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->resolve_peers) {
            if (PMIX_SUCCESS == active->module->resolve_peers(nodename, nspace, procs, nprocs)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_resolve_nodes(const char *nspace,
                                           char **nodelist)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->resolve_nodes) {
            if (PMIX_SUCCESS == active->module->resolve_nodes(nspace, nodelist)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_copy(char **dest, size_t *len, const char *input)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->copy) {
            if (PMIX_SUCCESS == active->module->copy(dest, len, input)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_pack(pmix_buffer_t *buffer, const char *input)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->pack) {
            if (PMIX_SUCCESS == active->module->pack(buffer, input)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_unpack(pmix_buffer_t *buffer, char **regex)
{
    pmix_preg_base_active_module_t *active;

    PMIX_LIST_FOREACH(active, &pmix_preg_globals.actives, pmix_preg_base_active_module_t) {
        if (NULL != active->module->unpack) {
            if (PMIX_SUCCESS == active->module->unpack(buffer, regex)) {
                return PMIX_SUCCESS;
            }
        }
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

pmix_status_t pmix_preg_base_std_resolve_peers(const char *nodename,
                                               const char *nspace,
                                               pmix_proc_t **procs, size_t *nprocs)
{
    pmix_cb_t cb;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_proc_t proc;
    char **ptr;
    pmix_info_t *info;
    pmix_proc_t *p=NULL;
    size_t ninfo, np=0, n, j;

    PMIX_CONSTRUCT(&cb, pmix_cb_t);

    cb.key = strdup(nodename);
    /* this data isn't going anywhere, so we don't require a copy */
    cb.copy = false;
    /* scope is irrelevant as the info we seek must be local */
    cb.scope = PMIX_SCOPE_UNDEF;
    /* let the proc point to the nspace */
    pmix_strncpy(proc.nspace, nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    cb.proc = &proc;

    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS != rc) {
        if (PMIX_ERR_INVALID_NAMESPACE != rc) {
            PMIX_ERROR_LOG(rc);
        }
        goto complete;
    }
    /* should just be the one value on the list */
    if (1 != pmix_list_get_size(&cb.kvs)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        rc = PMIX_ERR_BAD_PARAM;
        goto complete;
    }
    kv = (pmix_kval_t*)pmix_list_get_first(&cb.kvs);
    /* the hostname used as a key with wildcard rank will return
     * a pmix_data_array_t of pmix_info_t structs */
    if (NULL == kv->value ||
        PMIX_DATA_ARRAY != kv->value->type ||
        NULL == kv->value->data.darray ||
        PMIX_INFO != kv->value->data.darray->type) {
        PMIX_ERROR_LOG(PMIX_ERR_DATA_VALUE_NOT_FOUND);
        rc = PMIX_ERR_DATA_VALUE_NOT_FOUND;
        goto complete;
    }
    info = (pmix_info_t*)kv->value->data.darray->array;
    ninfo = kv->value->data.darray->size;
    /* find the PMIX_LOCAL_PEERS key */
    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_LOCAL_PEERS, PMIX_MAX_KEYLEN)) {
            /* split the string */
            ptr = pmix_argv_split(info[n].value.data.string, ',');
            np = pmix_argv_count(ptr);
            PMIX_PROC_CREATE(p, np);
            if (NULL == p) {
                rc = PMIX_ERR_NOMEM;
                pmix_argv_free(ptr);
                goto complete;
            }
            for (j=0; j < np; j++) {
                pmix_strncpy(p[j].nspace, nspace, PMIX_MAX_NSLEN);
                p[j].rank = strtoul(ptr[j], NULL, 10);
            }
            rc = PMIX_SUCCESS;
            pmix_argv_free(ptr);
            break;
        }
    }

  complete:
    if (NULL != cb.info) {
        PMIX_INFO_FREE(cb.info, cb.ninfo);
    }
    if (NULL != cb.key) {
        free(cb.key);
        cb.key = NULL;
    }
    PMIX_DESTRUCT(&cb);
    *procs = p;
    *nprocs = np;

    return rc;
}

pmix_status_t pmix_preg_base_std_resolve_nodes(const char *nspace,
                                               char **nodelist)
{
    pmix_cb_t cb;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_proc_t proc;

    PMIX_CONSTRUCT(&cb, pmix_cb_t);

    /* setup default answer */
    *nodelist = NULL;

    /* create a pmix_info_t so we can pass the nspace
    * into the fetch as a qualifier */
    PMIX_INFO_CREATE(cb.info, 1);
    if (NULL == cb.info) {
        PMIX_DESTRUCT(&cb);
        return PMIX_ERR_NOMEM;
    }
    cb.ninfo = 1;
    PMIX_INFO_LOAD(&cb.info[0], PMIX_NSPACE, nspace, PMIX_STRING);

    /* tell the GDS what we want */
    cb.key = PMIX_NODE_MAP;
    /* this data isn't going anywhere, so we don't require a copy */
    cb.copy = false;
    /* scope is irrelevant as the info we seek must be local */
    cb.scope = PMIX_SCOPE_UNDEF;
    /* put the nspace in the proc field */
    pmix_strncpy(proc.nspace, nspace, PMIX_MAX_NSLEN);
    /* the info will be associated with PMIX_RANK_WILDCARD */
    proc.rank = PMIX_RANK_WILDCARD;
    cb.proc = &proc;

    PMIX_GDS_FETCH_KV(rc, pmix_client_globals.myserver, &cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* should just be the one value on the list */
    if (1 != pmix_list_get_size(&cb.kvs)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        rc = PMIX_ERR_BAD_PARAM;
        goto complete;
    }
    kv = (pmix_kval_t*)pmix_list_get_first(&cb.kvs);
    /* the PMIX_NODE_MAP key is supposed to return
    * a regex string  - check that it did */
    if (NULL == kv->value ||
        PMIX_STRING != kv->value->type) {
        PMIX_ERROR_LOG(PMIX_ERR_DATA_VALUE_NOT_FOUND);
        rc = PMIX_ERR_DATA_VALUE_NOT_FOUND;
        goto complete;
    }
    /* return the string */
    if (NULL != kv->value->data.string) {
        *nodelist = strdup(kv->value->data.string);
    }

  complete:
    if (NULL != cb.info) {
      PMIX_INFO_FREE(cb.info, cb.ninfo);
    }
    return rc;
}

