/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pmix_config.h"
#include "pmix_common.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#include <stdarg.h>

#include "src/include/pmix_globals.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_show_help.h"

#include "plog_default.h"
#include "src/mca/plog/base/base.h"

/* Static API's */
static int init(void);
static pmix_status_t mylog(const pmix_proc_t *source, const pmix_info_t data[], size_t ndata,
                           const pmix_info_t directives[], size_t ndirs, pmix_op_cbfunc_t cbfunc,
                           void *cbdata);

/* Module def */
pmix_plog_module_t pmix_plog_default_module = {
    .name = "default",
    .channels = NULL,
    .init = init,
    .finalize = NULL,
    .log = mylog
};

/* local object */
typedef struct {
    pmix_object_t super;
    const pmix_info_t *data;
    size_t ndata;
    pmix_op_cbfunc_t cbfunc;
    void *cbdata;
} local_caddy_t;
static void lcon(local_caddy_t *p)
{
    p->data = NULL;
    p->ndata = 0;
}
static PMIX_CLASS_INSTANCE(local_caddy_t, pmix_object_t,
                           lcon, NULL);

static int init(void)
{
    /* we cannot operate if our host doesn't support log */
    if (NULL == pmix_host_server.log) {
        return PMIX_ERR_NOT_AVAILABLE;
    }
    return PMIX_SUCCESS;
}

static void localcbfn(pmix_status_t status, void *cbdata)
{
    local_caddy_t *cd = (local_caddy_t *) cbdata;

    if (NULL != cd->cbfunc) {
        cd->cbfunc(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

static pmix_status_t mylog(const pmix_proc_t *source, const pmix_info_t data[], size_t ndata,
                           const pmix_info_t directives[], size_t ndirs, pmix_op_cbfunc_t cbfunc,
                           void *cbdata)
{
    local_caddy_t *cd;

    /* send it upwards for potential handling. This might seem
     * odd in the case where we are a gateway, but we must allow
     * for the possibility that the host has a channel we don't
     * directly support */
    cd = PMIX_NEW(local_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->data = data;
    cd->ndata = ndata;
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* ask the host to log the remainder */
    pmix_host_server.log(source, cd->data, cd->ndata, directives, ndirs, localcbfn, (void *) cd);

    return PMIX_OPERATION_IN_PROGRESS;
}
