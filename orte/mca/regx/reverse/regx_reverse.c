/*
 * Copyright (c) 2016-2018 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/basename.h"
#include "opal/util/opal_environ.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/mca/regx/base/base.h"

#include "regx_reverse.h"

static int nidmap_create(opal_pointer_array_t *pool, char **regex);
static int nidmap_parse(char *regex);
static int encode_nodemap(opal_buffer_t *buffer);
static int decode_daemon_nodemap(opal_buffer_t *buffer);
static int generate_ppn(orte_job_t *jdata, char **ppn);
static int parse_ppn(orte_job_t *jdata, char *ppn);

orte_regx_base_module_t orte_regx_reverse_module = {
    .nidmap_create = nidmap_create,
    .nidmap_parse = nidmap_parse,
    .encode_nodemap = encode_nodemap,
    .decode_daemon_nodemap = decode_daemon_nodemap,
    .generate_ppn = generate_ppn,
    .parse_ppn = parse_ppn
};

static int nidmap_create(opal_pointer_array_t *pool, char **regex)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int nidmap_parse(char *regex)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int encode_nodemap(opal_buffer_t *buffer)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int decode_daemon_nodemap(opal_buffer_t *buffer)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int generate_ppn(orte_job_t *jdata, char **ppn)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int parse_ppn(orte_job_t *jdata, char *ppn)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
