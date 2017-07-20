/*
 * Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <pmix_common.h>

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_globals.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"

#include <unistd.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "src/mca/psec/psec.h"
#include "psec_none.h"

static pmix_status_t none_init(void);
static void none_finalize(void);
static pmix_status_t validate_cred(int sd, uid_t uid, gid_t gid,
                                   pmix_listener_protocol_t protocol,
                                   char *cred, size_t len);

pmix_psec_module_t pmix_none_module = {
    .name = "none",
    .init = none_init,
    .finalize = none_finalize,
    .validate_cred = validate_cred
};

static pmix_status_t none_init(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: none init");
    return PMIX_SUCCESS;
}

static void none_finalize(void)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: none finalize");
}

static pmix_status_t validate_cred(int sd, uid_t uid, gid_t gid,
                                   pmix_listener_protocol_t protocol,
                                   char *cred, size_t len)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "psec: none always reports valid");
    return PMIX_SUCCESS;
}
