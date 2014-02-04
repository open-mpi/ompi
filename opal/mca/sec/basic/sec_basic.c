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

#include "opal/mca/sec/base/base.h"
#include "sec_basic.h"

static int init(void);
static void finalize(void);
static int get_token(const opal_identifier_t *proc,
                     opal_sec_cred_t token,
                     size_t size);
static int authenticate(const opal_identifier_t *proc,
                        opal_sec_cred_t token,
                        size_t size);

opal_sec_base_module_t opal_sec_basic_module = {
    init,
    finalize,
    get_token,
    authenticate
};

static int init(void)
{
    return OPAL_SUCCESS;
}

static void finalize(void)
{
}

static int get_token(const opal_identifier_t *proc,
                     opal_sec_cred_t token,
                     size_t size)
{
    uint32_t ui32;

    opal_output_verbose(2, opal_sec_base_framework.framework_output,
                        "creating sec token for %"PRIu64"", *proc);

    ui32 = htonl(12345);
    memcpy(token, &ui32, 4);

    opal_output_verbose(2, opal_sec_base_framework.framework_output,
                        "proc %"PRIu64" was assigned token %u",
                        *proc, 12345);
    return OPAL_SUCCESS;
}

static int authenticate(const opal_identifier_t *proc,
                        opal_sec_cred_t token,
                        size_t size)
{
    uint32_t ui32;
    uint32_t chk;

    opal_output_verbose(2, opal_sec_base_framework.framework_output,
                        "authenticating %"PRIu64"", *proc);

    /* for now, just check the identifier against the proc id */
    memcpy(&ui32, token, 4);

    chk = ntohl(ui32);

    if (12345 != chk) {
        opal_output_verbose(2, opal_sec_base_framework.framework_output,
                            "proc %"PRIu64" was not authenticated %u vs %u",
                            *proc, chk, 12345);
        return OPAL_ERROR;
    }

    opal_output_verbose(2, opal_sec_base_framework.framework_output,
                        "proc %"PRIu64" was authenticated", *proc);
    return OPAL_SUCCESS;
}

