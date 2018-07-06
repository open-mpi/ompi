/*
 * Copyright (c) 2015-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <pmix_common.h>

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"

#include "src/mca/psec/psec.h"
#include "psec_native.h"

static pmix_status_t native_init(void);
static void native_finalize(void);
static pmix_status_t create_cred(pmix_listener_protocol_t protocol,
                                 char **cred, size_t *len);
static pmix_status_t validate_cred(int sd, uid_t uid, gid_t gid,
                                   pmix_listener_protocol_t protocol,
                                   char *cred, size_t len);

pmix_psec_module_t pmix_native_module = {
    .name = "native",
    .init = native_init,
    .finalize = native_finalize,
    .create_cred = create_cred,
    .validate_cred = validate_cred
};

static pmix_status_t native_init(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: native init");
    return PMIX_SUCCESS;
}

static void native_finalize(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: native finalize");
}

static pmix_status_t create_cred(pmix_listener_protocol_t protocol,
                                 char **cred, size_t *len)
{
    uid_t euid;
    gid_t egid;
    char *tmp, *ptr;

    if (PMIX_PROTOCOL_V1 == protocol ||
        PMIX_PROTOCOL_V3 == protocol) {
        /* these are usock protocols - nothing to do */
        *cred = NULL;
        *len = 0;
        return PMIX_SUCCESS;
    }
    if (PMIX_PROTOCOL_V2 == protocol) {
        /* tcp protocol - need to provide our effective
         * uid and gid for validation on remote end */
        tmp = (char*)malloc(sizeof(uid_t) + sizeof(gid_t));
        if (NULL == tmp) {
            return PMIX_ERR_NOMEM;
        }
        euid = geteuid();
        memcpy(tmp, &euid, sizeof(uid_t));
        ptr = tmp + sizeof(uid_t);
        egid = getegid();
        memcpy(ptr, &egid, sizeof(gid_t));
        *cred = tmp;
        *len = sizeof(uid_t) + sizeof(gid_t);
        return PMIX_SUCCESS;
    }

    /* unrecognized protocol */
    return PMIX_ERR_NOT_SUPPORTED;
}

static pmix_status_t validate_cred(int sd, uid_t uid, gid_t gid,
                                   pmix_listener_protocol_t protocol,
                                   char *cred, size_t len)
{
#if defined(SO_PEERCRED)
#ifdef HAVE_STRUCT_SOCKPEERCRED_UID
#define HAVE_STRUCT_UCRED_UID
    struct sockpeercred ucred;
#else
    struct ucred ucred;
#endif
    socklen_t crlen = sizeof (ucred);
#endif
    uid_t euid;
    gid_t egid;
    char *ptr;
    size_t ln;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: native validate_cred %s", cred ? cred : "NULL");

    if (PMIX_PROTOCOL_V1 == protocol ||
        PMIX_PROTOCOL_V3 == protocol) {
        /* these are usock protocols - get the remote side's uid/gid */
#if defined(SO_PEERCRED) && (defined(HAVE_STRUCT_UCRED_UID) || defined(HAVE_STRUCT_UCRED_CR_UID))
        /* Ignore received 'cred' and validate ucred for socket instead. */
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec:native checking getsockopt on socket %d for peer credentials", sd);
        if (getsockopt (sd, SOL_SOCKET, SO_PEERCRED, &ucred, &crlen) < 0) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "psec: getsockopt SO_PEERCRED failed: %s",
                                strerror (pmix_socket_errno));
            return PMIX_ERR_INVALID_CRED;
        }
#if defined(HAVE_STRUCT_UCRED_UID)
        euid = ucred.uid;
        egid = ucred.gid;
#else
        euid = ucred.cr_uid;
        egid = ucred.cr_gid;
#endif

#elif defined(HAVE_GETPEEREID)
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec:native checking getpeereid on socket %d for peer credentials", sd);
        if (0 != getpeereid(sd, &euid, &egid)) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "psec: getsockopt getpeereid failed: %s",
                                strerror (pmix_socket_errno));
            return PMIX_ERR_INVALID_CRED;
    }
#else
        return PMIX_ERR_NOT_SUPPORTED;
#endif

        /* check uid */
        if (euid != uid) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "psec: socket cred contains invalid uid %u", euid);
            return PMIX_ERR_INVALID_CRED;
        }

        /* check gid */
        if (egid != gid) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "psec: socket cred contains invalid gid %u", egid);
            return PMIX_ERR_INVALID_CRED;
        }

        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec: native credential %u:%u valid",
                            euid, egid);
        return PMIX_SUCCESS;
    }

    if (PMIX_PROTOCOL_V2 == protocol) {
        /* this is a tcp protocol, so the cred is actually the uid/gid
         * passed upwards from the client */
        if (NULL == cred) {
            /* not allowed */
            return PMIX_ERR_INVALID_CRED;
        }
        ln = len;
        euid = 0;
        egid = 0;
        if (sizeof(uid_t) <= ln) {
            memcpy(&euid, cred, sizeof(uid_t));
            ln -= sizeof(uid_t);
            ptr = cred + sizeof(uid_t);
        } else {
            return PMIX_ERR_INVALID_CRED;
        }
        if (sizeof(gid_t) <= ln) {
            memcpy(&egid, ptr, sizeof(gid_t));
        } else {
            return PMIX_ERR_INVALID_CRED;
        }
        /* check uid */
        if (euid != uid) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "psec: socket cred contains invalid uid %u", euid);
            return PMIX_ERR_INVALID_CRED;
        }

        /* check gid */
        if (egid != gid) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "psec: socket cred contains invalid gid %u", egid);
            return PMIX_ERR_INVALID_CRED;
        }

        pmix_output_verbose(2, pmix_globals.debug_output,
                            "psec: native credential %u:%u valid",
                            euid, egid);
        return PMIX_SUCCESS;
    }

    /* don't recognize the protocol */
    return PMIX_ERR_NOT_SUPPORTED;
}
