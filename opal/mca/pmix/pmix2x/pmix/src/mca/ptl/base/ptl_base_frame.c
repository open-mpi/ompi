/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */
#include <src/include/pmix_config.h>

#include <pmix_common.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include "src/mca/mca.h"
#include "src/mca/base/base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/class/pmix_list.h"
#include "src/mca/ptl/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "src/mca/ptl/base/static-components.h"

/* Instantiate the global vars */
pmix_ptl_globals_t pmix_ptl_globals = {{{0}}};
pmix_ptl_API_t pmix_ptl = {
    .set_notification_cbfunc = pmix_ptl_stub_set_notification_cbfunc,
    .get_available_modules = pmix_ptl_stub_get_available_modules,
    .send_recv = pmix_ptl_stub_send_recv,
    .send_oneway = pmix_ptl_stub_send_oneway,
    .connect_to_peer = pmix_ptl_stub_connect_to_peer,
    .start_listening = pmix_ptl_base_start_listening,
    .stop_listening = pmix_ptl_base_stop_listening
};

static int pmix_ptl_register(pmix_mca_base_register_flag_t flags)
{
    return PMIX_SUCCESS;
}

static pmix_status_t pmix_ptl_close(void)
{
    if (!pmix_ptl_globals.initialized) {
        return PMIX_SUCCESS;
    }
    pmix_ptl_globals.initialized = false;

    /* ensure the listen thread has been shut down */
    pmix_ptl.stop_listening();

    /* the components will cleanup when closed */
    PMIX_DESTRUCT(&pmix_ptl_globals.actives);
    PMIX_LIST_DESTRUCT(&pmix_ptl_globals.posted_recvs);
    PMIX_LIST_DESTRUCT(&pmix_ptl_globals.listeners);

    return pmix_mca_base_framework_components_close(&pmix_ptl_base_framework, NULL);
}

static pmix_status_t pmix_ptl_open(pmix_mca_base_open_flag_t flags)
{
    /* initialize globals */
    pmix_ptl_globals.initialized = true;
    PMIX_CONSTRUCT(&pmix_ptl_globals.actives, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_ptl_globals.posted_recvs, pmix_list_t);
    pmix_ptl_globals.listen_thread_active = false;
    PMIX_CONSTRUCT(&pmix_ptl_globals.listeners, pmix_list_t);

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&pmix_ptl_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(pmix, ptl, "PMIx Transfer Layer",
                                pmix_ptl_register, pmix_ptl_open, pmix_ptl_close,
                                mca_ptl_base_static_components, 0);

/***   INSTANTIATE INTERNAL CLASSES   ***/
PMIX_CLASS_INSTANCE(pmix_ptl_base_active_t,
                    pmix_list_item_t,
                    NULL, NULL);

static void scon(pmix_ptl_send_t *p)
{
    memset(&p->hdr, 0, sizeof(pmix_ptl_hdr_t));
    p->hdr.tag = UINT32_MAX;
    p->hdr.nbytes = 0;
    p->data = NULL;
    p->hdr_sent = false;
    p->sdptr = NULL;
    p->sdbytes = 0;
}
static void sdes(pmix_ptl_send_t *p)
{
    if (NULL != p->data) {
        PMIX_RELEASE(p->data);
    }
}
PMIX_CLASS_INSTANCE(pmix_ptl_send_t,
                    pmix_list_item_t,
                    scon, sdes);

static void rcon(pmix_ptl_recv_t *p)
{
    memset(&p->hdr, 0, sizeof(pmix_ptl_hdr_t));
    p->hdr.tag = UINT32_MAX;
    p->hdr.nbytes = 0;
    p->data = NULL;
    p->hdr_recvd = false;
    p->rdptr = NULL;
    p->rdbytes = 0;
}
PMIX_CLASS_INSTANCE(pmix_ptl_recv_t,
                    pmix_list_item_t,
                    rcon, NULL);

static void prcon(pmix_ptl_posted_recv_t *p)
{
    p->tag = UINT32_MAX;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
PMIX_CLASS_INSTANCE(pmix_ptl_posted_recv_t,
                    pmix_list_item_t,
                    prcon, NULL);


static void srcon(pmix_ptl_sr_t *p)
{
    p->bfr = NULL;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_ptl_sr_t,
                                pmix_object_t,
                                srcon, NULL);

static void pccon(pmix_pending_connection_t *p)
{
    memset(p->nspace, 0, PMIX_MAX_NSLEN+1);
    p->info = NULL;
    p->ninfo = 0;
    p->bfrop = NULL;
    p->psec = NULL;
    p->ptl = NULL;
    p->cred = NULL;
}
static void pcdes(pmix_pending_connection_t *p)
{
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
    if (NULL != p->bfrop) {
        free(p->bfrop);
    }
    if (NULL != p->psec) {
        free(p->psec);
    }
    if (NULL != p->cred) {
        free(p->cred);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_pending_connection_t,
                                pmix_object_t,
                                pccon, pcdes);

static void lcon(pmix_listener_t *p)
{
    p->socket = -1;
    p->varname = NULL;
    p->uri = NULL;
    p->owner_given = false;
    p->group_given = false;
    p->mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH;
}
static void ldes(pmix_listener_t *p)
{
    if (0 <= p->socket) {
        CLOSE_THE_SOCKET(p->socket);
    }
    if (NULL != p->varname) {
        free(p->varname);
    }
    if (NULL != p->uri) {
        free(p->uri);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_listener_t,
                                pmix_list_item_t,
                                lcon, ldes);

PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_ptl_queue_t,
                                pmix_object_t,
                                NULL, NULL);
