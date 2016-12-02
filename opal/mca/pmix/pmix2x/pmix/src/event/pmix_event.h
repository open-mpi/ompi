/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_EVENT_H
#define PMIX_EVENT_H

#include <src/include/pmix_config.h>

#include <pmix_common.h>
#include "src/class/pmix_list.h"
#include "src/util/output.h"

 BEGIN_C_DECLS

/* define an object for tracking event handlers focused on a
 * single status code */
typedef struct {
    pmix_list_item_t super;
    char *name;
    size_t index;
    pmix_status_t code;
    pmix_notification_fn_t evhdlr;
    void *cbobject;
} pmix_single_event_t;
PMIX_CLASS_DECLARATION(pmix_single_event_t);

/* define an object for tracking event handlers registered
 * on multiple status codes, generally corresponding to a
 * functional group */
typedef struct {
    pmix_list_item_t super;
    char *name;
    size_t index;
    pmix_status_t *codes;
    size_t ncodes;
    pmix_notification_fn_t evhdlr;
    void *cbobject;
} pmix_multi_event_t;
PMIX_CLASS_DECLARATION(pmix_multi_event_t);

/* define an object for tracking default event handlers */
typedef struct {
    pmix_list_item_t super;
    char *name;
    size_t index;
    pmix_notification_fn_t evhdlr;
    void *cbobject;
} pmix_default_event_t;
PMIX_CLASS_DECLARATION(pmix_default_event_t);

/* define an object for tracking status codes we are actively
 * registered to receive */
typedef struct {
    pmix_list_item_t super;
    pmix_status_t code;
} pmix_active_code_t;
PMIX_CLASS_DECLARATION(pmix_active_code_t);

/* define an object for housing the different lists of events
 * we have registered so we can easily scan them in precedent
 * order when we get an event */
typedef struct {
    pmix_object_t super;
    size_t nhdlrs;
    pmix_list_t actives;
    pmix_list_t single_events;
    pmix_list_t multi_events;
    pmix_list_t default_events;
} pmix_events_t;
PMIX_CLASS_DECLARATION(pmix_events_t);

/* define an object for chaining event notifications thru
 * the local state machine. Each registered event handler
 * that asked to be notified for a given code is given a
 * chance to "see" the reported event, starting with
 * single-code handlers, then multi-code handlers, and
 * finally default handlers. This object provides a
 * means for us to relay the event across that chain
 */
typedef struct pmix_event_chain_t {
    pmix_object_t super;
    pmix_status_t status;
    bool nondefault;
    pmix_proc_t source;
    pmix_data_range_t range;
    pmix_info_t *info;
    size_t ninfo;
    pmix_info_t *results;
    size_t nresults;
    pmix_single_event_t *sing;
    pmix_multi_event_t *multi;
    pmix_default_event_t *def;
    pmix_op_cbfunc_t final_cbfunc;
    void *final_cbdata;
} pmix_event_chain_t;
PMIX_CLASS_DECLARATION(pmix_event_chain_t);

/* invoke the error handler that is registered against the given
 * status, passing it the provided info on the procs that were
 * affected, plus any additional info provided by the server */
void pmix_invoke_local_event_hdlr(pmix_event_chain_t *chain);

#define PMIX_REPORT_EVENT(e, f)                     \
    do {                                            \
        pmix_event_chain_t *_ch;                    \
        _ch = PMIX_NEW(pmix_event_chain_t);         \
        _ch->status = (e);                          \
        _ch->ninfo = 1;                             \
        _ch->final_cbfunc = (f);                    \
        _ch->final_cbdata = _ch;                    \
        PMIX_INFO_CREATE(_ch->info, _ch->ninfo);    \
        PMIX_INFO_LOAD(&_ch->info[0],               \
                       PMIX_EVENT_RETURN_OBJECT,    \
                       NULL, PMIX_POINTER);         \
        pmix_invoke_local_event_hdlr(_ch);          \
    } while(0)


 END_C_DECLS

#endif /* PMIX_EVENT_H */
