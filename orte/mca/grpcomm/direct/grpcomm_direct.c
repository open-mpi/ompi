/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>

#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_direct.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *buf);
static int allgather(orte_vpid_t *vpids,
                     size_t nprocs,
                     opal_buffer_t *buf,
                     orte_grpcomm_cbfunc_t cbfunc,
                     void *cbdata);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_direct_module = {
    init,
    finalize,
    xcast,
    allgather
};

/**
 * Initialize the module
 */
static int init(void)
{
    return OPAL_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    return;
}

static int xcast(orte_vpid_t *vpids,
                 size_t nprocs,
                 opal_buffer_t *buf)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int allgather(orte_vpid_t *vpids,
                     size_t nprocs,
                     opal_buffer_t *buf,
                     orte_grpcomm_cbfunc_t cbfunc,
                     void *cbdata)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}
