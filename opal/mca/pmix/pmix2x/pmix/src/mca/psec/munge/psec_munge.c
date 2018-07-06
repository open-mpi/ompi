/*
 * Copyright (c) 2015-2018 Intel, Inc. All rights reserved.
 *
 * NOTE: THE MUNGE CLIENT LIBRARY (libmunge) IS LICENSED AS LGPL
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <pmix_common.h>

#include "src/include/pmix_globals.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <munge.h>

#include "src/threads/threads.h"
#include "src/mca/psec/psec.h"
#include "psec_munge.h"

static pmix_status_t munge_init(void);
static void munge_finalize(void);
static pmix_status_t create_cred(pmix_listener_protocol_t protocol,
                                 char **cred, size_t *len);
static pmix_status_t validate_cred(int sd, uid_t uid, gid_t gid,
                                   pmix_listener_protocol_t protocol,
                                   char *cred, size_t len);

pmix_psec_module_t pmix_munge_module = {
    .name = "munge",
    .init = munge_init,
    .finalize = munge_finalize,
    .create_cred = create_cred,
    .validate_cred = validate_cred
};

static pmix_lock_t lock;
static char *mycred = NULL;
static bool initialized = false;
static bool refresh = false;

static pmix_status_t munge_init(void)
{
    int rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge init");

    PMIX_CONSTRUCT_LOCK(&lock);
    lock.active = false;

    /* attempt to get a credential as a way of checking that
     * the munge server is available - cache the credential
     * for later use */

    if (EMUNGE_SUCCESS != (rc = munge_encode(&mycred, NULL, NULL, 0))) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec: munge failed to create credential: %s",
                            munge_strerror(rc));
        return PMIX_ERR_SERVER_NOT_AVAIL;
    }

    initialized = true;

    return PMIX_SUCCESS;
}

static void munge_finalize(void)
{
    PMIX_ACQUIRE_THREAD(&lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge finalize");
    if (initialized) {
        if (NULL != mycred) {
            free(mycred);
            mycred = NULL;
        }
    }
    PMIX_RELEASE_THREAD(&lock);
    PMIX_DESTRUCT_LOCK(&lock);
}

static pmix_status_t create_cred(pmix_listener_protocol_t protocol,
                                 char **cred, size_t *len)
{
    int rc;

    PMIX_ACQUIRE_THREAD(&lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge create_cred");

    if (initialized) {
        if (!refresh) {
            refresh = true;
            *cred = strdup(mycred);
            *len = strlen(mycred) + 1;
        } else {
            /* munge does not allow reuse of a credential, so we have to
             * refresh it for every use */
            if (NULL != mycred) {
                free(mycred);
            }
            if (EMUNGE_SUCCESS != (rc = munge_encode(&mycred, NULL, NULL, 0))) {
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "psec: munge failed to create credential: %s",
                                    munge_strerror(rc));
                PMIX_RELEASE_THREAD(&lock);
                return PMIX_ERR_NOT_SUPPORTED;
            }
            *cred = strdup(mycred);
            *len = strlen(mycred) + 1;
        }
    }
    PMIX_RELEASE_THREAD(&lock);
    return PMIX_SUCCESS;
}

static pmix_status_t validate_cred(int sd, uid_t uid, gid_t gid,
                                   pmix_listener_protocol_t protocol,
                                   char *cred, size_t len)
{
    uid_t euid;
    gid_t egid;
    munge_err_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge validate_cred %s", cred ? cred : "NULL");

    /* parse the inbound string */
    if (EMUNGE_SUCCESS != (rc = munge_decode(cred, NULL, NULL, NULL, &euid, &egid))) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec: munge failed to decode credential: %s",
                            munge_strerror(rc));
        return PMIX_ERR_INVALID_CRED;
    }

    /* check uid */
    if (euid != uid) {
        return PMIX_ERR_INVALID_CRED;
    }

    /* check guid */
    if (egid != gid) {
        return PMIX_ERR_INVALID_CRED;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge credential valid");
    return PMIX_SUCCESS;
}
