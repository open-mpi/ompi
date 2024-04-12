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
#include "pmix_server.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#include <stdarg.h>

#include "src/common/pmix_iof.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_show_help.h"

#include "plog_stdfd.h"
#include "src/mca/plog/base/base.h"

/* Static API's */
static int init(void);
static void finalize(void);
static pmix_status_t mylog(const pmix_proc_t *source, const pmix_info_t data[], size_t ndata,
                           const pmix_info_t directives[], size_t ndirs, pmix_op_cbfunc_t cbfunc,
                           void *cbdata);

/* Module def */
pmix_plog_module_t pmix_plog_stdfd_module = {
    .name = "stdfd",
    .init = init,
    .finalize = finalize,
    .log = mylog
};

static int init(void)
{
    char *mychannels = "stdout,stderr";

    pmix_plog_stdfd_module.channels = PMIx_Argv_split(mychannels, ',');
    return PMIX_SUCCESS;
}

static void finalize(void)
{
    PMIx_Argv_free(pmix_plog_stdfd_module.channels);
}

typedef struct{
    pmix_object_t super;
    pmix_proc_t source;
    pmix_byte_object_t bo;
} pmix_iof_deliver_t;
static void pdcon(pmix_iof_deliver_t *p)
{
    p->bo.bytes = NULL;
    p->bo.size = 0;
}
static void pddes(pmix_iof_deliver_t *p)
{
    if (NULL != p->bo.bytes) {
        free(p->bo.bytes);
    }
}
static PMIX_CLASS_INSTANCE(pmix_iof_deliver_t,
                           pmix_object_t,
                           pdcon, pddes);

static void lkcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_iof_deliver_t *p = (pmix_iof_deliver_t*)cbdata;

    /* nothing to do here - we use this solely to
     * ensure that IOF_deliver doesn't block */
    if (PMIX_SUCCESS != status) {
        PMIX_ERROR_LOG(status);
    }
    PMIX_RELEASE(p);
}

static pmix_status_t mylog(const pmix_proc_t *source, const pmix_info_t data[], size_t ndata,
                           const pmix_info_t directives[], size_t ndirs, pmix_op_cbfunc_t cbfunc,
                           void *cbdata)
{
    size_t n;
    pmix_status_t rc;
    pmix_iof_deliver_t *p;

    /* if there is no data, then we don't handle it */
    if (NULL == data || 0 == ndata) {
        return PMIX_ERR_NOT_AVAILABLE;
    }

    /* if we are not a gateway or tool, then we don't handle this */
    if (!PMIX_PEER_IS_GATEWAY(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) {
        return PMIX_ERR_TAKE_NEXT_OPTION;
    }


    PMIX_HIDE_UNUSED_PARAMS(directives, ndirs, cbfunc, cbdata);
    
#if 0
    /* check to see if there are any relevant directives */
    for (n = 0; n < ndirs; n++) {
        if (0 == strncmp(directives[n].key, PMIX_LOG_TIMESTAMP, PMIX_MAX_KEYLEN)) {
            flags.timestamp = directives[n].value.data.time;
        } else if (0 == strncmp(directives[n].key, PMIX_LOG_XML_OUTPUT, PMIX_MAX_KEYLEN)) {
            flags.xml = PMIX_INFO_TRUE(&directives[n]);
        } else if (0 == strncmp(directives[n].key, PMIX_LOG_TAG_OUTPUT, PMIX_MAX_KEYLEN)) {
            flags.tag = PMIX_INFO_TRUE(&directives[n]);
        }
    }
#endif
    /* check to see if there are any stdfd entries */
    rc = PMIX_ERR_TAKE_NEXT_OPTION;
    for (n = 0; n < ndata; n++) {
        if (PMIX_INFO_OP_IS_COMPLETE(&data[n])) {
            continue;
        }
        if (0 == strncmp(data[n].key, PMIX_LOG_STDERR, PMIX_MAX_KEYLEN)) {
            p = PMIX_NEW(pmix_iof_deliver_t);
            PMIX_XFER_PROCID(&p->source, source);
            p->bo.size = strlen(data[n].value.data.string) + 1; // include NULL terminator
            p->bo.bytes = (char*)malloc(p->bo.size);
            memcpy(p->bo.bytes, data[n].value.data.string, p->bo.size);
            rc = PMIx_server_IOF_deliver(&p->source, PMIX_FWD_STDERR_CHANNEL, &p->bo, NULL, 0, lkcbfunc, (void*)p);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(p);
            }
        } else if (0 == strncmp(data[n].key, PMIX_LOG_STDOUT, PMIX_MAX_KEYLEN)) {
            p = PMIX_NEW(pmix_iof_deliver_t);
            PMIX_XFER_PROCID(&p->source, source);
            p->bo.size = strlen(data[n].value.data.string) + 1; // include NULL terminator
            p->bo.bytes = (char*)malloc(p->bo.size);
            memcpy(p->bo.bytes, data[n].value.data.string, p->bo.size);
            rc = PMIx_server_IOF_deliver(&p->source, PMIX_FWD_STDOUT_CHANNEL, &p->bo, NULL, 0, lkcbfunc, (void*)p);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(p);
            }
        }
    }
    return rc;
}
