/*
 * Copyright (c) 2015-2016 Intel, Inc.  All rights reserved.
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

#include "src/mca/psec/psec.h"
#include "psec_munge.h"

static int munge_init(void);
static void munge_finalize(void);
static char* create_cred(void);
static int validate_cred(pmix_peer_t *peer, char *cred);

pmix_psec_module_t pmix_munge_module = {
    "munge",
    munge_init,
    munge_finalize,
    create_cred,
    NULL,
    validate_cred,
    NULL
};

static char *mycred = NULL;
static bool initialized = false;
static bool refresh = false;

static int munge_init(void)
{
    int rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge init");

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
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge finalize");
    if (initialized) {
        if (NULL != mycred) {
            free(mycred);
            mycred = NULL;
        }
    }
}

static char* create_cred(void)
{
    int rc;
    char *resp=NULL;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge create_cred");

    if (initialized) {
        if (!refresh) {
            refresh = true;
            resp = strdup(mycred);
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
                return NULL;
            }
            resp = strdup(mycred);
        }
    }
    return resp;
}

static int validate_cred(pmix_peer_t *peer, char *cred)
{
    uid_t uid;
    gid_t gid;
    munge_err_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge validate_cred %s", cred ? cred : "NULL");

    /* parse the inbound string */
    if (EMUNGE_SUCCESS != (rc = munge_decode(cred, NULL, NULL, NULL, &uid, &gid))) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec: munge failed to decode credential: %s",
                            munge_strerror(rc));
        return PMIX_ERR_INVALID_CRED;
    }

    /* check uid */
    if (uid != peer->info->uid) {
        return PMIX_ERR_INVALID_CRED;
    }

    /* check guid */
    if (gid != peer->info->gid) {
        return PMIX_ERR_INVALID_CRED;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge credential valid");
    return PMIX_SUCCESS;
}
