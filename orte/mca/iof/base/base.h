/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * I/O Forwarding Service
 */
                                                                                         
#ifndef MCA_IOF_BASE_H
#define MCA_IOF_BASE_H

#include "orte_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/class/opal_bitmap.h"
#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"

#include "orte/mca/iof/iof.h"
#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

/*
 * MCA framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_iof_base_framework;
/*
 * Select an available component.
 */
ORTE_DECLSPEC int orte_iof_base_select(void);

/* track xon/xoff of processes */
typedef struct {
    opal_object_t super;
    orte_job_t *jdata;
    opal_bitmap_t xoff;
} orte_iof_job_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_job_t);

/*
 * Maximum size of single msg 
 */
#define ORTE_IOF_BASE_MSG_MAX           4096
#define ORTE_IOF_BASE_TAG_MAX             50
#define ORTE_IOF_BASE_TAGGED_OUT_MAX    8192
#define ORTE_IOF_MAX_INPUT_BUFFERS        50

typedef struct {
    opal_list_item_t super;
    bool pending;
    opal_event_t *ev;
    int fd;
    opal_list_t outputs;
} orte_iof_write_event_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_write_event_t);

typedef struct {
    opal_list_item_t super;
    orte_process_name_t name;
    orte_process_name_t daemon;
    orte_iof_tag_t tag;
    orte_iof_write_event_t *wev;
    bool xoff;
} orte_iof_sink_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_sink_t);

typedef struct {
    opal_object_t super;
    orte_process_name_t name;
    opal_event_t *ev;
    int fd;
    orte_iof_tag_t tag;
    bool active;
} orte_iof_read_event_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_read_event_t);

typedef struct {
    opal_list_item_t super;
    orte_process_name_t name;
    orte_iof_read_event_t *revstdout;
    orte_iof_read_event_t *revstderr;
    orte_iof_read_event_t *revstddiag;
    orte_iof_sink_t *sink;
} orte_iof_proc_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_proc_t);

typedef struct {
    opal_list_item_t super;
    char data[ORTE_IOF_BASE_TAGGED_OUT_MAX];
    int numbytes;
} orte_iof_write_output_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_iof_write_output_t);

/* the iof globals struct */
struct orte_iof_base_t {
    size_t                  output_limit;
    char                    *input_files;
    orte_iof_sink_t         *iof_write_stdout;
    orte_iof_sink_t         *iof_write_stderr;
};
typedef struct orte_iof_base_t orte_iof_base_t;


#define ORTE_IOF_SINK_DEFINE(snk, nm, fid, tg, wrthndlr, eplist)    \
    do {                                                            \
        orte_iof_sink_t *ep;                                        \
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,           \
                            "defining endpt: file %s line %d fd %d",\
                            __FILE__, __LINE__, (fid)));            \
        ep = OBJ_NEW(orte_iof_sink_t);                              \
        ep->name.jobid = (nm)->jobid;                               \
        ep->name.vpid = (nm)->vpid;                                 \
        ep->tag = (tg);                                             \
        if (0 <= (fid)) {                                           \
            ep->wev->fd = (fid);                                    \
            opal_event_set(orte_event_base,                         \
                           ep->wev->ev, ep->wev->fd,                \
                           OPAL_EV_WRITE,                           \
                           wrthndlr, ep);                           \
            opal_event_set_priority(ep->wev->ev, ORTE_MSG_PRI);     \
        }                                                           \
        if (NULL != (eplist)) {                                     \
            opal_list_append((eplist), &ep->super);                 \
        }                                                           \
        *(snk) = ep;                                                \
    } while(0);

/* add list of structs that has name of proc + orte_iof_tag_t - when
 * defining a read event, search list for proc, add flag to the tag.
 * when closing a read fd, find proc on list and zero out that flag
 * when all flags = 0, then iof is complete - set message event to
 * daemon processor indicating proc iof is terminated
 */
#define ORTE_IOF_READ_EVENT(rv, nm, fid, tg, cbfunc, actv)          \
    do {                                                            \
        orte_iof_read_event_t *rev;                                 \
        OPAL_OUTPUT_VERBOSE((1, orte_iof_base_framework.framework_output,           \
                            "%s defining read event for %s: %s %d", \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),     \
                            ORTE_NAME_PRINT((nm)),                  \
                            __FILE__, __LINE__));                   \
        rev = OBJ_NEW(orte_iof_read_event_t);                       \
        rev->name.jobid = (nm)->jobid;                              \
        rev->name.vpid = (nm)->vpid;                                \
        rev->tag = (tg);                                            \
        rev->fd = (fid);                                            \
        *(rv) = rev;                                                \
        opal_event_set(orte_event_base,                             \
                       rev->ev, (fid),                              \
                       OPAL_EV_READ,                                \
                       (cbfunc), rev);                              \
        opal_event_set_priority(rev->ev, ORTE_MSG_PRI);             \
        if ((actv)) {                                               \
            rev->active = true;                                     \
            opal_event_add(rev->ev, 0);                             \
        }                                                           \
    } while(0);


ORTE_DECLSPEC int orte_iof_base_flush(void);

ORTE_DECLSPEC extern orte_iof_base_t orte_iof_base;

/* base functions */
ORTE_DECLSPEC int orte_iof_base_write_output(orte_process_name_t *name, orte_iof_tag_t stream,
                                             unsigned char *data, int numbytes,
                                             orte_iof_write_event_t *channel);
ORTE_DECLSPEC void orte_iof_base_write_handler(int fd, short event, void *cbdata);

END_C_DECLS

#endif /* MCA_IOF_BASE_H */
