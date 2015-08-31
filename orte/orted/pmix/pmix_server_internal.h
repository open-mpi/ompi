/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
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
 */

#ifndef _PMIX_SERVER_INTERNAL_H_
#define _PMIX_SERVER_INTERNAL_H_

#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include "opal/types.h"
#include "opal/class/opal_hotel.h"
#include "opal/mca/base/base.h"
#include "opal/mca/event/event.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/proc.h"

#include "orte/mca/grpcomm/base/base.h"

 BEGIN_C_DECLS

/* object for tracking requests so we can
 * correctly route the eventual reply */
 typedef struct {
    opal_object_t super;
    opal_event_t ev;
    int room_num;
    int remote_room_num;
    orte_process_name_t proxy;
    opal_process_name_t target;
    orte_job_t *jdata;
    opal_buffer_t msg;
    opal_pmix_op_cbfunc_t opcbfunc;
    opal_pmix_modex_cbfunc_t mdxcbfunc;
    opal_pmix_spawn_cbfunc_t spcbfunc;
    opal_pmix_lookup_cbfunc_t lkcbfunc;
    void *cbdata;
} pmix_server_req_t;
OBJ_CLASS_DECLARATION(pmix_server_req_t);

/* object for thread-shifting server operations */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_job_t *jdata;
    orte_process_name_t proc;
    int status;
    orte_proc_t *object;
    opal_pmix_op_cbfunc_t cbfunc;
    void *cbdata;
} orte_pmix_server_op_caddy_t;
OBJ_CLASS_DECLARATION(orte_pmix_server_op_caddy_t);

typedef struct {
    opal_object_t super;
    orte_grpcomm_signature_t *sig;
    opal_pmix_modex_cbfunc_t cbfunc;
    void *cbdata;
} orte_pmix_mdx_caddy_t;
OBJ_CLASS_DECLARATION(orte_pmix_mdx_caddy_t);

#define ORTE_DMX_REQ(p, cf, ocf, ocd)                    \
do {                                                     \
    pmix_server_req_t *_req;                             \
    _req = OBJ_NEW(pmix_server_req_t);                   \
    _req->target = (p);                                  \
    _req->mdxcbfunc = (ocf);                             \
    _req->cbdata = (ocd);                                \
    opal_event_set(orte_event_base, &(_req->ev),         \
                   -1, OPAL_EV_WRITE, (cf), _req);       \
    opal_event_set_priority(&(_req->ev), ORTE_MSG_PRI);  \
    opal_event_active(&(_req->ev), OPAL_EV_WRITE, 1);    \
} while(0);

#define ORTE_SPN_REQ(j, cf, ocf, ocd)                    \
do {                                                     \
    pmix_server_req_t *_req;                             \
    _req = OBJ_NEW(pmix_server_req_t);                   \
    _req->jdata = (j);                                   \
    _req->spcbfunc = (ocf);                              \
    _req->cbdata = (ocd);                                \
    opal_event_set(orte_event_base, &(_req->ev),         \
                   -1, OPAL_EV_WRITE, (cf), _req);       \
    opal_event_set_priority(&(_req->ev), ORTE_MSG_PRI);  \
    opal_event_active(&(_req->ev), OPAL_EV_WRITE, 1);    \
} while(0);

#define ORTE_PMIX_OPERATION(n, r, ob, s, fn, cf, cb)                \
do {                                                                \
    orte_pmix_server_op_caddy_t *_cd;                               \
    _cd = OBJ_NEW(orte_pmix_server_op_caddy_t);                     \
    /* convert the namespace to jobid and create name */            \
    orte_util_convert_string_to_jobid(&(_cd->proc.jobid), (n));     \
    _cd->proc.vpid = (r);                                           \
    _cd->object = (ob);                                             \
    _cd->cbfunc = (cf);                                             \
    _cd->cbdata = (cb);                                             \
    _cd->status = (s);                                              \
    opal_event_set(orte_event_base, &(_cd->ev), -1,                 \
                   OPAL_EV_WRITE, (fn), _cd);                       \
    opal_event_set_priority(&(_cd->ev), ORTE_MSG_PRI);              \
    opal_event_active(&(_cd->ev), OPAL_EV_WRITE, 1);                \
} while(0);


/* define the server module functions */
extern int pmix_server_client_connected_fn(opal_process_name_t *proc, void* server_object);
extern int pmix_server_client_finalized_fn(opal_process_name_t *proc, void* server_object,
                                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_abort_fn(opal_process_name_t *proc, void *server_object,
                                int status, const char msg[],
                                opal_list_t *procs_to_abort,
                                opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_fencenb_fn(opal_list_t *procs, opal_list_t *info,
                                  char *data, size_t ndata,
                                  opal_pmix_modex_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_dmodex_req_fn(opal_process_name_t *proc, opal_list_t *info,
                                     opal_pmix_modex_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_publish_fn(opal_process_name_t *proc,
                                  opal_pmix_data_range_t range,
                                  opal_pmix_persistence_t persist,
                                  opal_list_t *info,
                                  opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_lookup_fn(opal_process_name_t *proc,
                                 opal_pmix_data_range_t range,
                                 opal_list_t *info, char **keys,
                                 opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_unpublish_fn(opal_process_name_t *proc,
                                    opal_pmix_data_range_t range, char **keys,
                                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_spawn_fn(opal_process_name_t *requestor,
                                opal_list_t *job_info, opal_list_t *apps,
                                opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_connect_fn(opal_list_t *procs, opal_list_t *info,
                                  opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_disconnect_fn(opal_list_t *procs, opal_list_t *info,
                                     opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
extern int pmix_server_register_events_fn(opal_list_t *info,
                                          opal_pmix_op_cbfunc_t cbfunc,
                                          void *cbdata);

extern void pmix_server_launch_resp(int status, orte_process_name_t* sender,
                                    opal_buffer_t *buffer,
                                    orte_rml_tag_t tg, void *cbdata);

/* exposed shared variables */
typedef struct {
    bool initialized;
    int verbosity;
    int output;
    opal_hotel_t reqs;
    int num_rooms;
    int timeout;
    orte_process_name_t server;
} pmix_server_globals_t;

extern pmix_server_globals_t orte_pmix_server_globals;

END_C_DECLS

#endif /* PMIX_SERVER_INTERNAL_H_ */

