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

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <stdio.h>
#include <curl/curl.h>

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
    /* init libcurl */
    curl_global_init(CURL_GLOBAL_ALL);

    return OPAL_SUCCESS;
}

static void finalize(void)
{
    /* cleanup libcurl */
    curl_global_cleanup();
}

static size_t op_cbfunc(void *ptr, size_t size, size_t count, void *stream)
{
    opal_output(0, "CURL RETURNED: %s", (char*)stream);
    return size;
}

static int get_my_cred(opal_identifier_t *my_id,
                       opal_sec_cred_t **cred)
{
    char *cmd;
    CURL *curl;
    CURLcode rc;

    opal_output_verbose(5, opal_sec_base_framework.framework_output,
                        "keystone:get_my_cred");

    /* ensure we return at least a NULL */
    *cred = NULL;

    /* query the keystone server */
    asprintf(&cmd, "%sget_cred", mca_sec_keystone_component.url);
    curl = curl_easy_init();
    curl_easy_setopt(curl, CURLOPT_URL, cmd);
    /* send the data to this function */
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, op_cbfunc);
    /* execute it */
    if (CURLE_OK != (rc = curl_easy_perform(curl))) {
        opal_output(0, "Error while fetching '%s' : %s",
                    cmd, curl_easy_strerror(rc));
    }
    /* the data will have been returned in the callback
     * function when easy_perform completes
     */
    curl_easy_cleanup(curl);
    free(cmd);

    return OPAL_ERR_NOT_IMPLEMENTED;
}

static int authenticate(opal_sec_cred_t *cred)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

