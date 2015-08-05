/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"

#include "pmix_server_internal.h"

int pmix_server_spawn_fn(opal_list_t *apps,
                         opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    /* for now, just ack the call */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, ORTE_JOBID_INVALID, cbdata);
    }

    return OPAL_SUCCESS;
}

int pmix_server_connect_fn(opal_list_t *procs,
                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    /* for now, just ack the call */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }

    return OPAL_SUCCESS;
}

int pmix_server_disconnect_fn(opal_list_t *procs,
                              opal_pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    /* for now, just ack the call */
    if (NULL != cbfunc) {
        cbfunc(OPAL_SUCCESS, cbdata);
    }

    return OPAL_SUCCESS;
}

