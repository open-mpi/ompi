/*
 * Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
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
static pmix_status_t create_cred(struct pmix_peer_t *peer,
                                 const pmix_info_t directives[], size_t ndirs,
                                 pmix_info_t **info, size_t *ninfo,
                                 pmix_byte_object_t *cred);
static pmix_status_t validate_cred(struct pmix_peer_t *peer,
                                   const pmix_info_t directives[], size_t ndirs,
                                   pmix_info_t **info, size_t *ninfo,
                                   const pmix_byte_object_t *cred);

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

static pmix_status_t create_cred(struct pmix_peer_t *peer,
                                 const pmix_info_t directives[], size_t ndirs,
                                 pmix_info_t **info, size_t *ninfo,
                                 pmix_byte_object_t *cred)
{
    int rc;
    bool takeus;
    char **types;
    size_t n, m;

    PMIX_ACQUIRE_THREAD(&lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge create_cred");

    /* ensure initialization */
    PMIX_BYTE_OBJECT_CONSTRUCT(cred);

    /* if we are responding to a local request to create a credential,
     * then see if they specified a mechanism */
    if (NULL != directives && 0 < ndirs) {
        for (n=0; n < ndirs; n++) {
            if (0 == strncmp(directives[n].key, PMIX_CRED_TYPE, PMIX_MAX_KEYLEN)) {
                /* split the specified string */
                types = pmix_argv_split(directives[n].value.data.string, ',');
                takeus = false;
                for (m=0; NULL != types[m]; m++) {
                    if (0 == strcmp(types[m], "munge")) {
                        /* it's us! */
                        takeus = true;
                        break;
                    }
                }
                pmix_argv_free(types);
                if (!takeus) {
                    PMIX_RELEASE_THREAD(&lock);
                    return PMIX_ERR_NOT_SUPPORTED;
                }
            }
        }
    }

    if (initialized) {
        if (!refresh) {
            refresh = true;
            cred->bytes = strdup(mycred);
            cred->size = strlen(mycred) + 1;
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
            cred->bytes = strdup(mycred);
            cred->size = strlen(mycred) + 1;
        }
    }
    if (NULL != info) {
        /* mark that this came from us */
        PMIX_INFO_CREATE(*info, 1);
        if (NULL == *info) {
            PMIX_RELEASE_THREAD(&lock);
            return PMIX_ERR_NOMEM;
        }
        *ninfo = 1;
        PMIX_INFO_LOAD(info[0], PMIX_CRED_TYPE, "munge", PMIX_STRING);
    }
    PMIX_RELEASE_THREAD(&lock);
    return PMIX_SUCCESS;
}

static pmix_status_t validate_cred(struct pmix_peer_t *peer,
                                   const pmix_info_t directives[], size_t ndirs,
                                   pmix_info_t **info, size_t *ninfo,
                                   const pmix_byte_object_t *cred)
{
    pmix_peer_t *pr = (pmix_peer_t*)peer;
    uid_t euid;
    gid_t egid;
    munge_err_t rc;
    bool takeus;
    char **types;
    size_t n, m;
    uint32_t u32;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge validate_cred %s",
                        (NULL == cred) ? "NULL" : "NON-NULL");

    /* if we are responding to a local request to validate a credential,
     * then see if they specified a mechanism */
    if (NULL != directives && 0 < ndirs) {
        for (n=0; n < ndirs; n++) {
            if (0 == strncmp(directives[n].key, PMIX_CRED_TYPE, PMIX_MAX_KEYLEN)) {
                /* split the specified string */
                types = pmix_argv_split(directives[n].value.data.string, ',');
                takeus = false;
                for (m=0; NULL != types[m]; m++) {
                    if (0 == strcmp(types[m], "munge")) {
                        /* it's us! */
                        takeus = true;
                        break;
                    }
                }
                pmix_argv_free(types);
                if (!takeus) {
                    return PMIX_ERR_NOT_SUPPORTED;
                }
            }
        }
    }

    /* parse the inbound string */
    if (EMUNGE_SUCCESS != (rc = munge_decode(cred->bytes, NULL, NULL, NULL, &euid, &egid))) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec: munge failed to decode credential: %s",
                            munge_strerror(rc));
        return PMIX_ERR_INVALID_CRED;
    }

    /* check uid */
    if (euid != pr->info->uid) {
        return PMIX_ERR_INVALID_CRED;
    }

    /* check guid */
    if (egid != pr->info->gid) {
        return PMIX_ERR_INVALID_CRED;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: munge credential valid");
    if (NULL != info) {
        PMIX_INFO_CREATE(*info, 3);
        if (NULL == *info) {
            return PMIX_ERR_NOMEM;
        }
        *ninfo = 3;
        /* mark that this came from us */
        PMIX_INFO_LOAD(info[0], PMIX_CRED_TYPE, "munge", PMIX_STRING);
        /* provide the uid it contained */
        u32 = euid;
        PMIX_INFO_LOAD(info[1], PMIX_USERID, &u32, PMIX_UINT32);
        /* provide the gid it contained */
        u32 = egid;
        PMIX_INFO_LOAD(info[2], PMIX_GRPID, &u32, PMIX_UINT32);
    }
    return PMIX_SUCCESS;
}
