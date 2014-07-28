/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/mca/pmix/pmix.h"

#include "dstore_pmi.h"

/* we only support the fetch API */
static int fetch(struct opal_dstore_base_module_t *mod,
                 const opal_identifier_t *proc,
                 const char *key,
                 opal_list_t *kvs);

mca_dstore_pmi_module_t opal_dstore_pmi_module = {
    {
        NULL,
        NULL,
        NULL,
        fetch,
        NULL
    }
};

static int fetch(struct opal_dstore_base_module_t *mod,
                 const opal_identifier_t *uid,
                 const char *key, opal_list_t *kvs)
{
    int rc;
    opal_value_t *kv;

    /* request the data from pmi */
    if (OPAL_SUCCESS != (rc = opal_pmix.get((opal_identifier_t*)uid, key, &kv))) {
        OPAL_ERROR_LOG(rc);
        return rc;
    }
    opal_list_append(kvs, &kv->super);

    return OPAL_SUCCESS;
}

