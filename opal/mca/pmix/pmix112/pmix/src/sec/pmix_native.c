/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <pmix/pmix_common.h>

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/util/argv.h"
#include "src/util/output.h"
#include "src/usock/usock.h"

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "pmix_sec.h"
#include "pmix_native.h"

static int native_init(void);
static void native_finalize(void);
static pmix_status_t validate_cred(pmix_peer_t *peer, char *cred);

pmix_sec_base_module_t pmix_native_module = {
    "native",
    native_init,
    native_finalize,
    NULL,
    NULL,
    validate_cred,
    NULL
};

static int native_init(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: native init");
    return PMIX_SUCCESS;
}

static void native_finalize(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: native finalize");
}

static pmix_status_t validate_cred(pmix_peer_t *peer, char *cred)
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
    gid_t gid;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: native validate_cred %s", cred ? cred : "NULL");

#if defined(SO_PEERCRED) && (defined(HAVE_STRUCT_UCRED_UID) || defined(HAVE_STRUCT_UCRED_CR_UID))
    /* Ignore received 'cred' and validate ucred for socket instead. */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec:native checking getsockopt for peer credentials");
    if (getsockopt (peer->sd, SOL_SOCKET, SO_PEERCRED, &ucred, &crlen) < 0) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "sec: getsockopt SO_PEERCRED failed: %s",
                            strerror (pmix_socket_errno));
        return PMIX_ERR_INVALID_CRED;
    }
#if defined(HAVE_STRUCT_UCRED_UID)
    euid = ucred.uid;
    gid = ucred.gid;
#else
    euid = ucred.cr_uid;
    gid = ucred.cr_gid;
#endif

#elif defined(HAVE_GETPEEREID)
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec:native checking getpeereid for peer credentials");
    if (0 != getpeereid(peer->sd, &euid, &gid)) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "sec: getsockopt getpeereid failed: %s",
                            strerror (pmix_socket_errno));
        return PMIX_ERR_INVALID_CRED;
    }
#else
    return PMIX_ERR_NOT_SUPPORTED;
#endif

    /* check uid */
    if (euid != peer->info->uid) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "sec: socket cred contains invalid uid %u", euid);
        return PMIX_ERR_INVALID_CRED;
    }

    /* check gid */
    if (gid != peer->info->gid) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "sec: socket cred contains invalid gid %u", gid);
        return PMIX_ERR_INVALID_CRED;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: native credential %u:%u valid",
                        euid, gid);
    return PMIX_SUCCESS;
}

