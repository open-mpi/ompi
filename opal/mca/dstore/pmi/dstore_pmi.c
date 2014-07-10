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

#include "opal/util/output.h"
#include "opal/mca/pmix/pmix.h"

#include "dstore_pmi.h"

/* we only support the finalize and fetch APIs */
static void finalize(void);
static int fetch(const opal_identifier_t *proc,
                 const char *key,
                 opal_list_t *kvs);

mca_dstore_pmi_module_t opal_dstore_pmi_module = {
    {
        NULL,
        finalize,
        NULL,
        fetch,
        NULL
    }
};

/* Local variables */

static void finalize(void)
{
    /* balance the calls to pmi_init */
    opal_pmix.finalize();
}

static int fetch(const opal_identifier_t *uid,
                 const char *key, opal_list_t *kvs)
{
    int rc;
    opal_value_t *kv;

    /* request the data from pmi */
    kv = OBJ_NEW(opal_value_t);
    if (OPAL_SUCCESS != (rc = opal_pmix.get(uid, key, kv))) {
        OPAL_ERROR_LOG(rc);
        OBJ_RELEASE(kv);
        return rc;
    }
    opal_list_append(kvs, &kv->super);

    return OPAL_SUCCESS;
}

