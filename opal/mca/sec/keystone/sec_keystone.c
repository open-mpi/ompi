/*
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

#include "opal_stdint.h"
#include "opal/dss/dss_types.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/sec/base/base.h"
#include "sec_basic.h"

static int init(void);
static void finalize(void);
static int get_my_cred(opal_identifier_t *my_id,
                       opal_sec_cred_t **cred);
static int authenticate(opal_sec_cred_t *cred);

opal_sec_base_module_t opal_sec_keystone_module = {
    init,
    finalize,
    get_my_cred,
    authenticate
};

static int init(void)
{
    return OPAL_SUCCESS;
}

static void finalize(void)
{
}

static int get_my_cred(opal_identifier_t *my_id,
                       opal_sec_cred_t **cred)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int authenticate(opal_sec_cred_t *cred)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

