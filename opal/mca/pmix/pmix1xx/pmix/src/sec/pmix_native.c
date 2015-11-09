/*
 * Copyright (c) 2015      Intel, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <pmix/pmix_common.h>

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
static char* create_cred(void);
static int validate_cred(pmix_peer_t *peer, char *cred);

pmix_sec_base_module_t pmix_native_module = {
    "native",
    native_init,
    native_finalize,
    create_cred,
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

static char* create_cred(void)
{
    char *cred;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: native create_cred");

    /* print them and return the string */
    (void)asprintf(&cred, "%lu:%lu", (unsigned long)pmix_globals.uid,
                   (unsigned long)pmix_globals.gid);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: using credential %s", cred);

    return cred;
}

static int validate_cred(pmix_peer_t *peer, char *cred)
{
    uid_t uid;
    gid_t gid;
    char **vals;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: native validate_cred %s", cred);

    /* parse the inbound string */
    vals = pmix_argv_split(cred, ':');
    if (2 != pmix_argv_count(vals)) {
        pmix_argv_free(vals);
        return PMIX_ERR_INVALID_CRED;
    }

    /* check uid */
    uid = strtoul(vals[0], NULL, 10);
    if (uid != peer->info->uid) {
        pmix_argv_free(vals);
        return PMIX_ERR_INVALID_CRED;
    }

    /* check guid */
    gid = strtoul(vals[1], NULL, 10);
    if (gid != peer->info->gid) {
        pmix_argv_free(vals);
        return PMIX_ERR_INVALID_CRED;
    }
    pmix_argv_free(vals);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sec: native credential valid");
    return PMIX_SUCCESS;
}

