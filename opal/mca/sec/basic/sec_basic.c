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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "opal_stdint.h"
#include "opal/dss/dss_types.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/db/db.h"

#include "opal/mca/sec/base/base.h"
#include "sec_basic.h"

static int init(void);
static void finalize(void);
static int get_my_cred(opal_identifier_t *my_id,
                       opal_sec_cred_t **cred);
static int authenticate(opal_sec_cred_t *cred);

opal_sec_base_module_t opal_sec_basic_module = {
    init,
    finalize,
    get_my_cred,
    authenticate
};

static opal_sec_cred_t my_cred;
static bool initialized = false;

static int init(void)
{
    return OPAL_SUCCESS;
}

static void finalize(void)
{
    if (initialized) {
        free(my_cred.credential);
    }
}

static int get_my_cred(opal_identifier_t *my_id,
                       opal_sec_cred_t **cred)
{
    opal_byte_object_t *cd;

    if (!initialized) {
        /* check first if a credential was stored for this job
         * in the database
         */
        if (OPAL_SUCCESS == opal_db.fetch(my_id, OPAL_DB_CREDENTIAL,
                                          (void**)&cd, OPAL_BYTE_OBJECT)) {
            my_cred.credential = (char*)cd->bytes;
            my_cred.size = cd->size;
        } else {
            my_cred.credential = strdup("1234567");
            my_cred.size = strlen(my_cred.credential)+1;  // include the NULL
        }
    }
    initialized = true;

    *cred = &my_cred;

    return OPAL_SUCCESS;
}

static int authenticate(opal_sec_cred_t *cred)
{

    if (0 == strncmp(cred->credential, "12345", strlen("12345"))) {
        return OPAL_SUCCESS;
    }
    return OPAL_ERR_AUTHENTICATION_FAILED;
}

