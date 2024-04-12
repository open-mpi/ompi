/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2013-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2017      Mellanox Technologies. All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <string.h>

#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_output.h"

#include "src/rml/rml.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"

#include "src/mca/iof/base/base.h"
#include "src/mca/iof/iof.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public prte_base_component_t struct.
 */

#include "src/mca/iof/base/static-components.h"

prte_iof_base_module_t prte_iof = {0};

/*
 * Global variables
 */

int prte_iof_base_output_limit = 0;

static int prte_iof_base_register(pmix_mca_base_register_flag_t flags)
{
    PRTE_HIDE_UNUSED_PARAMS(flags);

    /* check for maximum number of pending output messages */
    prte_iof_base_output_limit = (size_t) INT_MAX;
    (void) pmix_mca_base_var_register("prte", "iof", "base", "output_limit",
                                      "Maximum backlog of output messages [default: unlimited]",
                                      PMIX_MCA_BASE_VAR_TYPE_INT,
                                      &prte_iof_base_output_limit);

    return PRTE_SUCCESS;
}

static int prte_iof_base_close(void)
{
    /* Close the selected component */
    if (NULL != prte_iof.finalize) {
        prte_iof.finalize();
    }
    return pmix_mca_base_framework_components_close(&prte_iof_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
static int prte_iof_base_open(pmix_mca_base_open_flag_t flags)
{
    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_iof_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, iof, "PRTE I/O Forwarding",
                                prte_iof_base_register,
                                prte_iof_base_open, prte_iof_base_close,
                                prte_iof_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);


static void lkcbfunc(pmix_status_t status, void *cbdata)
{
    prte_iof_deliver_t *p = (prte_iof_deliver_t*)cbdata;

    /* nothing to do here - we use this solely to
     * ensure that IOF_deliver doesn't block */
    if (PMIX_SUCCESS != status) {
        PMIX_ERROR_LOG(status);
    }
    PMIX_RELEASE(p);
}


void prte_iof_base_output(const pmix_proc_t *source,
                          pmix_iof_channel_t channel,
                          char *string)
{
    prte_iof_deliver_t *p;
    pmix_status_t rc;

    p = PMIX_NEW(prte_iof_deliver_t);
    PMIX_XFER_PROCID(&p->source, source);
    p->bo.bytes = string;
    p->bo.size = strlen(string);
    rc = PMIx_server_IOF_deliver(&p->source, channel, &p->bo, NULL, 0, lkcbfunc, (void*)p);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(p);  // releases string
    }
}


/* class instances */
static void prte_iof_base_proc_construct(prte_iof_proc_t *ptr)
{
    ptr->stdinev = NULL;
    ptr->revstdout = NULL;
    ptr->revstderr = NULL;
}
static void prte_iof_base_proc_destruct(prte_iof_proc_t *ptr)
{
    if (NULL != ptr->stdinev) {
        PMIX_RELEASE(ptr->stdinev);
    }
    if (NULL != ptr->revstdout) {
        PMIX_RELEASE(ptr->revstdout);
    }
    if (NULL != ptr->revstderr) {
        PMIX_RELEASE(ptr->revstderr);
    }
}
PMIX_CLASS_INSTANCE(prte_iof_proc_t, pmix_list_item_t,
                    prte_iof_base_proc_construct,
                    prte_iof_base_proc_destruct);

static void prte_iof_base_sink_construct(prte_iof_sink_t *ptr)
{
    PMIX_LOAD_PROCID(&ptr->daemon, NULL, PMIX_RANK_INVALID);
    ptr->wev = PMIX_NEW(prte_iof_write_event_t);
    ptr->xoff = false;
    ptr->exclusive = false;
    ptr->closed = false;
}
static void prte_iof_base_sink_destruct(prte_iof_sink_t *ptr)
{
    if (NULL != ptr->wev) {
        PMIX_OUTPUT_VERBOSE((20, prte_iof_base_framework.framework_output,
                             "%s iof: closing sink for process %s on fd %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&ptr->name),
                             ptr->wev->fd));
        PMIX_RELEASE(ptr->wev);
    }
}
PMIX_CLASS_INSTANCE(prte_iof_sink_t, pmix_list_item_t,
                    prte_iof_base_sink_construct,
                    prte_iof_base_sink_destruct);

static void prte_iof_base_read_event_construct(prte_iof_read_event_t *rev)
{
    rev->proc = NULL;
    rev->fd = -1;
    rev->active = false;
    rev->activated = false;
    rev->always_readable = false;
    rev->ev = prte_event_alloc();
    rev->sink = NULL;
    rev->tv.tv_sec = 0;
    rev->tv.tv_usec = 0;
}
static void prte_iof_base_read_event_destruct(prte_iof_read_event_t *rev)
{
    prte_iof_proc_t *proct = (prte_iof_proc_t *) rev->proc;

    if (0 <= rev->fd) {
        prte_event_free(rev->ev);
        PMIX_OUTPUT_VERBOSE((20, prte_iof_base_framework.framework_output,
                             "%s iof: closing fd %d for process %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), rev->fd,
                             (NULL == proct) ? "UNKNOWN" : PRTE_NAME_PRINT(&proct->name)));
        close(rev->fd);
        rev->fd = -1;
    } else {
        free(rev->ev);
    }
    if (NULL != rev->sink) {
        PMIX_RELEASE(rev->sink);
    }
    if (NULL != proct) {
        PMIX_RELEASE(proct);
    }
}
PMIX_CLASS_INSTANCE(prte_iof_read_event_t, pmix_object_t,
                    prte_iof_base_read_event_construct,
                    prte_iof_base_read_event_destruct);

static void prte_iof_base_write_event_construct(prte_iof_write_event_t *wev)
{
    wev->pending = false;
    wev->always_writable = false;
    wev->fd = -1;
    PMIX_CONSTRUCT(&wev->outputs, pmix_list_t);
    wev->ev = prte_event_alloc();
    wev->tv.tv_sec = 0;
    wev->tv.tv_usec = 0;
}
static void prte_iof_base_write_event_destruct(prte_iof_write_event_t *wev)
{
    if (0 <= wev->fd) {
        prte_event_free(wev->ev);
    } else {
        free(wev->ev);
    }
    if (2 < wev->fd) {
        PMIX_OUTPUT_VERBOSE((20, prte_iof_base_framework.framework_output,
                             "%s iof: closing fd %d for write event",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), wev->fd));
        close(wev->fd);
    }
    PMIX_DESTRUCT(&wev->outputs);
}
PMIX_CLASS_INSTANCE(prte_iof_write_event_t, pmix_list_item_t,
                    prte_iof_base_write_event_construct,
                    prte_iof_base_write_event_destruct);

PMIX_CLASS_INSTANCE(prte_iof_write_output_t, pmix_list_item_t, NULL, NULL);

static void pdcon(prte_iof_deliver_t *p)
{
    p->bo.bytes = NULL;
    p->bo.size = 0;
}
static void pddes(prte_iof_deliver_t *p)
{
    if (NULL != p->bo.bytes) {
        free(p->bo.bytes);
    }
}
PMIX_CLASS_INSTANCE(prte_iof_deliver_t, pmix_object_t,
                    pdcon, pddes);
