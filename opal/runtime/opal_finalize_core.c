/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "opal_config.h"

#include "opal/class/opal_object.h"
#include "opal/constants.h"
#include "opal/runtime/opal.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"

extern int opal_util_initialized;

static opal_mutex_t opal_finalize_cleanup_fns_lock = OPAL_MUTEX_STATIC_INIT;
opal_list_t opal_finalize_cleanup_fns = {{0}};

struct opal_cleanup_fn_item_t {
    opal_list_item_t super;
    opal_cleanup_fn_t cleanup_fn;
    void *user_data;
#if OPAL_ENABLE_DEBUG
    char *cleanup_fn_name;
#endif
};

typedef struct opal_cleanup_fn_item_t opal_cleanup_fn_item_t;
OBJ_CLASS_DECLARATION(opal_cleanup_fn_item_t);

static void opal_cleanup_fn_item_construct(opal_cleanup_fn_item_t *item)
{
#if OPAL_ENABLE_DEBUG
    item->cleanup_fn_name = NULL;
#endif
}

static void opal_cleanup_fn_item_destruct(opal_cleanup_fn_item_t *item)
{
#if OPAL_ENABLE_DEBUG
    free(item->cleanup_fn_name);
    item->cleanup_fn_name = NULL;
#endif
}

OBJ_CLASS_INSTANCE(opal_cleanup_fn_item_t, opal_list_item_t, opal_cleanup_fn_item_construct,
                   opal_cleanup_fn_item_destruct);

static void opal_finalize_domain_construct(opal_finalize_domain_t *domain)
{
    domain->domain_name = NULL;
}

static void opal_finalize_domain_destruct(opal_finalize_domain_t *domain)
{
    free(domain->domain_name);
    domain->domain_name = NULL;
}

OBJ_CLASS_INSTANCE(opal_finalize_domain_t, opal_list_t, opal_finalize_domain_construct,
                   opal_finalize_domain_destruct);

static opal_finalize_domain_t *current_finalize_domain;
opal_finalize_domain_t opal_init_util_domain = {{{0}}};
opal_finalize_domain_t opal_init_domain = {{{0}}};

void opal_finalize_append_cleanup(opal_cleanup_fn_t cleanup_fn, const char *fn_name,
                                  void *user_data)
{
    opal_cleanup_fn_item_t *cleanup_item = OBJ_NEW(opal_cleanup_fn_item_t);
    assert(NULL != cleanup_item);
    cleanup_item->cleanup_fn = cleanup_fn;
    cleanup_item->user_data = user_data;
#if OPAL_ENABLE_DEBUG
    cleanup_item->cleanup_fn_name = strdup(fn_name);
    assert(NULL != cleanup_item->cleanup_fn_name);
#else
    (void) fn_name;
#endif

    opal_mutex_lock(&opal_finalize_cleanup_fns_lock);
    opal_list_append(&current_finalize_domain->super, &cleanup_item->super);
    opal_mutex_unlock(&opal_finalize_cleanup_fns_lock);
}

void opal_finalize_domain_init(opal_finalize_domain_t *domain, const char *domain_name)
{
    free(domain->domain_name);
    domain->domain_name = domain_name ? strdup(domain_name) : NULL;
}

void opal_finalize_set_domain(opal_finalize_domain_t *domain)
{
    current_finalize_domain = domain;
}

void opal_finalize_cleanup_domain(opal_finalize_domain_t *domain)
{
    opal_cleanup_fn_item_t *cleanup_item, *next;
    /* call any registered cleanup functions before tearing down OPAL */
    OPAL_LIST_FOREACH_SAFE_REV (cleanup_item, next, &domain->super, opal_cleanup_fn_item_t) {
        cleanup_item->cleanup_fn(cleanup_item->user_data);
        opal_list_remove_item(&domain->super, &cleanup_item->super);
        OBJ_RELEASE(cleanup_item);
    }
}

int opal_finalize_util(void)
{
    if (--opal_util_initialized != 0) {
        if (opal_util_initialized < 0) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }

    opal_finalize_cleanup_domain(&opal_init_util_domain);
    OBJ_DESTRUCT(&opal_init_util_domain);

    /* finalize the class/object system */
    opal_class_finalize();

    free(opal_process_info.nodename);
    opal_process_info.nodename = NULL;

    return OPAL_SUCCESS;
}

