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
#include "opal/mca/dstore/dstore.h"

#include "opal/mca/sec/base/base.h"
#include "sec_basic.h"

static int init(void);
static void finalize(void);
static int get_my_cred(int dstorehandle,
                       opal_process_name_t *my_id,
                       opal_sec_cred_t *cred);
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

static int get_my_cred(int dstorehandle,
                       opal_process_name_t *my_id,
                       opal_sec_cred_t *cred)
{
    opal_list_t vals;
    opal_value_t *kv;

    if (!initialized) {
        /* check first if a credential was stored for this job
         * in the database
         */
        OBJ_CONSTRUCT(&vals, opal_list_t);
        if (OPAL_SUCCESS == opal_dstore.fetch(dstorehandle, my_id, OPAL_DSTORE_CREDENTIAL, &vals)) {
            kv = (opal_value_t*)opal_list_remove_first(&vals);
            if (NULL == kv) {
                /* make the default credential 7-bytes long so we hit a nice
                 * 8-byte alignment (including NULL terminator) to keep valgrind
                 * from barking in optimized builds
                 */
                my_cred.credential = strdup("1234567");
                my_cred.size = strlen(my_cred.credential)+1;  // include the NULL
            } else {
                my_cred.credential = strdup(kv->data.string);
                my_cred.size = strlen(kv->data.string)+1;  // include the NULL
                OBJ_RELEASE(kv);
            }
        } else {
            my_cred.credential = strdup("1234567");
            my_cred.size = strlen(my_cred.credential)+1;  // include the NULL
        }
        OPAL_LIST_DESTRUCT(&vals);
    }
    initialized = true;

    cred->method = strdup("basic");
    cred->credential = strdup(my_cred.credential);
    cred->size = my_cred.size;

    return OPAL_SUCCESS;
}

static int authenticate(opal_sec_cred_t *cred)
{
    opal_output_verbose(5, opal_sec_base_framework.framework_output,
                        "opal_sec:basic Received credential %s of size %lu",
                        cred->credential, (unsigned long)cred->size);

    if (0 == strncmp(cred->credential, "1234567", strlen("1234567"))) {
        return OPAL_SUCCESS;
    }
    return OPAL_ERR_AUTHENTICATION_FAILED;
}

