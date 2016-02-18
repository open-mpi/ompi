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
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <ctype.h>

#include "opal_stdint.h"
#include "opal/class/opal_hotel.h"
#include "opal/class/opal_list.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/util/opal_environ.h"
#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "pmix_server.h"
#include "pmix_server_internal.h"

/*
 * Local utility functions
 */
static void pmix_server_dmdx_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata);
static void pmix_server_dmdx_resp(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata);

pmix_server_globals_t orte_pmix_server_globals = {0};

static opal_pmix_server_module_t pmix_server = {
    pmix_server_client_connected_fn,
    pmix_server_client_finalized_fn,
    pmix_server_abort_fn,
    pmix_server_fencenb_fn,
    pmix_server_dmodex_req_fn,
    pmix_server_publish_fn,
    pmix_server_lookup_fn,
    pmix_server_unpublish_fn,
    pmix_server_spawn_fn,
    pmix_server_connect_fn,
    pmix_server_disconnect_fn,
    pmix_server_register_events_fn,
    pmix_server_deregister_events_fn,
    NULL
};

void pmix_server_register_params(void)
{
    /* register a verbosity */
    orte_pmix_server_globals.verbosity = -1;
    (void) mca_base_var_register ("orte", "pmix", NULL, "server_verbose",
                                  "Debug verbosity for PMIx server",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &orte_pmix_server_globals.verbosity);
    if (0 <= orte_pmix_server_globals.verbosity) {
        orte_pmix_server_globals.output = opal_output_open(NULL);
        opal_output_set_verbosity(orte_pmix_server_globals.output,
                                  orte_pmix_server_globals.verbosity);
    }
    /* specify the size of the hotel */
    orte_pmix_server_globals.num_rooms = 256;
    (void) mca_base_var_register ("orte", "pmix", NULL, "server_max_reqs",
                                  "Maximum number of backlogged PMIx server direct modex requests",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &orte_pmix_server_globals.num_rooms);
    /* specify the timeout for the hotel */
    orte_pmix_server_globals.timeout = 2;
    (void) mca_base_var_register ("orte", "pmix", NULL, "server_max_wait",
                                  "Maximum time (in seconds) the PMIx server should wait to service direct modex requests",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &orte_pmix_server_globals.timeout);

    /* register the URI of the UNIVERSAL data server */
    orte_pmix_server_globals.server_uri = NULL;
    (void) mca_base_var_register ("orte", "pmix", NULL, "server_uri",
                                  "URI of a session-level keyval server for publish/lookup operations",
                                  MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &orte_pmix_server_globals.server_uri);

    /* whether or not to wait for the universal server */
    orte_pmix_server_globals.wait_for_server = false;
    (void) mca_base_var_register ("orte", "pmix", NULL, "wait_for_server",
                                  "Whether or not to wait for the session-level server to start",
                                  MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &orte_pmix_server_globals.wait_for_server);
}

static void eviction_cbfunc(struct opal_hotel_t *hotel,
                            int room_num, void *occupant)
{
    pmix_server_req_t *req = (pmix_server_req_t*)occupant;
    int rc;

    /* decrement the request timeout */
    req->timeout -= orte_pmix_server_globals.timeout;
    if (0 < req->timeout) {
        /* not done yet - check us back in */
        if (OPAL_SUCCESS == (rc = opal_hotel_checkin(&orte_pmix_server_globals.reqs, req, &req->room_num))) {
            return;
        }
        ORTE_ERROR_LOG(rc);
        /* fall thru and return an error so the caller doesn't hang */
    }
    /* don't let the caller hang */
    if (NULL != req->opcbfunc) {
        req->opcbfunc(OPAL_ERR_TIMEOUT, req->cbdata);
    } else if (NULL != req->mdxcbfunc) {
        req->mdxcbfunc(OPAL_ERR_TIMEOUT, NULL, 0, req->cbdata, NULL, NULL);
    } else if (NULL != req->spcbfunc) {
        req->spcbfunc(OPAL_ERR_TIMEOUT, ORTE_JOBID_INVALID, req->cbdata);
    } else if (NULL != req->lkcbfunc) {
        req->lkcbfunc(OPAL_ERR_TIMEOUT, NULL, req->cbdata);
    }
    OBJ_RELEASE(req);
}

/*
 * Initialize global variables used w/in the server.
 */
int pmix_server_init(void)
{
    int rc;
    opal_list_t info;

    if (orte_pmix_server_globals.initialized) {
        return ORTE_SUCCESS;
    }
    orte_pmix_server_globals.initialized = true;

    /* setup the server's state variables */
    OBJ_CONSTRUCT(&orte_pmix_server_globals.reqs, opal_hotel_t);
    if (OPAL_SUCCESS != (rc = opal_hotel_init(&orte_pmix_server_globals.reqs,
                                              orte_pmix_server_globals.num_rooms,
                                              orte_event_base, orte_pmix_server_globals.timeout*1000000,
                                              ORTE_ERROR_PRI, eviction_cbfunc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    OBJ_CONSTRUCT(&orte_pmix_server_globals.notifications, opal_list_t);

   /* setup recv for direct modex requests */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX,
                            ORTE_RML_PERSISTENT, pmix_server_dmdx_recv, NULL);

    /* setup recv for replies to direct modex requests */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX_RESP,
                            ORTE_RML_PERSISTENT, pmix_server_dmdx_resp, NULL);

    /* setup recv for replies to proxy launch requests */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_LAUNCH_RESP,
                            ORTE_RML_PERSISTENT, pmix_server_launch_resp, NULL);

    /* setup recv for replies from data server */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DATA_CLIENT,
                            ORTE_RML_PERSISTENT, pmix_server_keyval_client, NULL);

    /* setup recv for notifications */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_NOTIFICATION,
                            ORTE_RML_PERSISTENT, pmix_server_notify, NULL);

    /* ensure the PMIx server uses the proper rendezvous directory */
    opal_setenv("PMIX_SERVER_TMPDIR", orte_process_info.proc_session_dir, true, &environ);

    /* pass the server the local topology - we do this so the procs won't read the
     * topology themselves as this could overwhelm the local
     * system on large-scale SMPs */
    OBJ_CONSTRUCT(&info, opal_list_t);
    if (NULL != opal_hwloc_topology) {
        char *xmlbuffer=NULL;
        int len;
        opal_value_t *kv;
        kv = OBJ_NEW(opal_value_t);
        kv->key = strdup(OPAL_PMIX_LOCAL_TOPO);
        if (0 != hwloc_topology_export_xmlbuffer(opal_hwloc_topology, &xmlbuffer, &len)) {
            OBJ_RELEASE(kv);
            OBJ_DESTRUCT(&info);
            return ORTE_ERROR;
        }
        kv->data.string = xmlbuffer;
        kv->type = OPAL_STRING;
        opal_list_append(&info, &kv->super);
    }

    /* setup the local server */
    if (ORTE_SUCCESS != (rc = opal_pmix.server_init(&pmix_server, &info))) {
        ORTE_ERROR_LOG(rc);
        /* memory cleanup will occur when finalize is called */
    }
    OPAL_LIST_DESTRUCT(&info);

    /* if the universal server wasn't specified, then we use
     * our own HNP for that purpose */
    if (NULL == orte_pmix_server_globals.server_uri) {
        orte_pmix_server_globals.server = *ORTE_PROC_MY_HNP;
    } else {
        char *server;
        opal_buffer_t buf;
        if (0 == strncmp(orte_pmix_server_globals.server_uri, "file", strlen("file")) ||
            0 == strncmp(orte_pmix_server_globals.server_uri, "FILE", strlen("FILE"))) {
            char input[1024], *filename;
            FILE *fp;

            /* it is a file - get the filename */
            filename = strchr(orte_pmix_server_globals.server_uri, ':');
            if (NULL == filename) {
                /* filename is not correctly formatted */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-bad", true,
                               orte_basename, orte_pmix_server_globals.server_uri);
                return ORTE_ERR_BAD_PARAM;
            }
            ++filename; /* space past the : */

            if (0 >= strlen(filename)) {
                /* they forgot to give us the name! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-missing", true,
                               orte_basename, orte_pmix_server_globals.server_uri);
                return ORTE_ERR_BAD_PARAM;
            }

            /* open the file and extract the uri */
            fp = fopen(filename, "r");
            if (NULL == fp) { /* can't find or read file! */
                orte_show_help("help-orterun.txt", "orterun:ompi-server-filename-access", true,
                               orte_basename, orte_pmix_server_globals.server_uri);
                return ORTE_ERR_BAD_PARAM;
            }
            if (NULL == fgets(input, 1024, fp)) {
                /* something malformed about file */
                fclose(fp);
                orte_show_help("help-orterun.txt", "orterun:ompi-server-file-bad", true,
                               orte_basename, orte_pmix_server_globals.server_uri,
                               orte_basename);
                return ORTE_ERR_BAD_PARAM;
            }
            fclose(fp);
            input[strlen(input)-1] = '\0';  /* remove newline */
            server = strdup(input);
        } else {
            server = strdup(orte_pmix_server_globals.server_uri);
        }
        /* setup our route to the server */
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &server, 1, OPAL_STRING);
        if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(&buf))) {
            ORTE_ERROR_LOG(rc);
            ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
            return rc;
        }
        OBJ_DESTRUCT(&buf);
        /* parse the URI to get the server's name */
        if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(server, &orte_pmix_server_globals.server, NULL))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* check if we are to wait for the server to start - resolves
         * a race condition that can occur when the server is run
         * as a background job - e.g., in scripts
         */
        if (orte_pmix_server_globals.wait_for_server) {
            /* ping the server */
            struct timeval timeout;
            timeout.tv_sec = orte_pmix_server_globals.timeout;
            timeout.tv_usec = 0;
            if (ORTE_SUCCESS != (rc = orte_rml.ping(server, &timeout))) {
                /* try it one more time */
                if (ORTE_SUCCESS != (rc = orte_rml.ping(server, &timeout))) {
                    /* okay give up */
                    orte_show_help("help-orterun.txt", "orterun:server-not-found", true,
                                   orte_basename, server,
                                   (long)orte_pmix_server_globals.timeout,
                                   ORTE_ERROR_NAME(rc));
                    ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
                    return rc;
                }
            }
        }
    }

    return rc;
}

void pmix_server_finalize(void)
{
    if (!orte_pmix_server_globals.initialized) {
        return;
    }

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s Finalizing PMIX server",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* stop receives */
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX_RESP);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_LAUNCH_RESP);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DATA_CLIENT);
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_NOTIFICATION);

    /* shutdown the local server */
    opal_pmix.server_finalize();

    /* cleanup collectives */
    OBJ_DESTRUCT(&orte_pmix_server_globals.reqs);
    OPAL_LIST_DESTRUCT(&orte_pmix_server_globals.notifications);
}

static void send_error(int status, opal_process_name_t *idreq,
                       orte_process_name_t *remote)
{
    opal_buffer_t *reply;
    int rc;

    reply = OBJ_NEW(opal_buffer_t);
    /* pack the status */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &status, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        return;
    }
    /* pack the id of the requested proc */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, idreq, 1, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        return;
    }
    /* send the response */
    orte_rml.send_buffer_nb(remote, reply,
                            ORTE_RML_TAG_DIRECT_MODEX_RESP,
                            orte_rml_send_callback, NULL);
    return;
}

static void _mdxresp(int sd, short args, void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t*)cbdata;
    int rc;
    opal_buffer_t *reply;

    /* check us out of the hotel */
    opal_hotel_checkout(&orte_pmix_server_globals.reqs, req->room_num);

    reply = OBJ_NEW(opal_buffer_t);
    /* return the status */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &req->status, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        goto done;
    }
    /* pack the id of the requested proc */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &req->target, 1, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        goto done;
    }
    /* pack the remote daemon's request room number */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &req->remote_room_num, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        goto done;
    }
    /* return any provided data */
    opal_dss.copy_payload(reply, &req->msg);

    /* send the response */
    orte_rml.send_buffer_nb(&req->proxy, reply,
                            ORTE_RML_TAG_DIRECT_MODEX_RESP,
                            orte_rml_send_callback, NULL);

  done:
    /* if they asked for a release, give it to them */
    if (NULL != req->rlcbfunc) {
        req->rlcbfunc(req->cbdata);
    }
    OBJ_RELEASE(req);
    return;
}
/* the modex_resp function takes place in the local PMIx server's
 * progress thread - we must therefore thread-shift it so we can
 * access our global data */
static void modex_resp(int status,
                       const char *data, size_t sz, void *cbdata,
                       opal_pmix_release_cbfunc_t relcbfunc, void *relcbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t*)cbdata;
    opal_buffer_t xfer;

    req->status = status;
    /* we need to preserve the data as the caller
     * will free it upon our return */
    OBJ_CONSTRUCT(&xfer, opal_buffer_t);
    opal_dss.load(&xfer, (void*)data, sz);
    opal_dss.copy_payload(&req->msg, &xfer);
    xfer.base_ptr = NULL; // protect the incoming data
    OBJ_DESTRUCT(&xfer);
    /* point to the callback */
    req->rlcbfunc = relcbfunc;
    req->cbdata = relcbdata;
    opal_event_set(orte_event_base, &(req->ev),
                   -1, OPAL_EV_WRITE, _mdxresp, req);
    opal_event_set_priority(&(req->ev), ORTE_MSG_PRI);
    opal_event_active(&(req->ev), OPAL_EV_WRITE, 1);
}
static void pmix_server_dmdx_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata)
{
    int rc, room_num;
    int32_t cnt;
    opal_process_name_t idreq;
    orte_process_name_t name;
    orte_job_t *jdata;
    orte_proc_t *proc;
    pmix_server_req_t *req;


    /* unpack the id of the proc whose data is being requested */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &idreq, &cnt, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s dmdx:recv request from proc %s for proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender),
                        ORTE_NAME_PRINT(&idreq));
    /* and the remote daemon's tracking room number */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &room_num, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    /* is this proc one of mine? */
    memcpy((char*)&name, (char*)&idreq, sizeof(orte_process_name_t));
    if (NULL == (jdata = orte_get_job_data_object(name.jobid))) {
        /* not having the jdata means that we haven't unpacked the
         * the launch message for this job yet - this is a race
         * condition, so just log the request and we will fill
         * it later */
        req = OBJ_NEW(pmix_server_req_t);
        req->proxy = *sender;
        req->target = idreq;
        req->remote_room_num = room_num;
        if (OPAL_SUCCESS != (rc = opal_hotel_checkin(&orte_pmix_server_globals.reqs, req, &req->room_num))) {
            OBJ_RELEASE(req);
            send_error(rc, &idreq, sender);
        }
        return;
    }
    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, name.vpid))) {
        /* this is truly an error, so notify the sender */
        send_error(ORTE_ERR_NOT_FOUND, &idreq, sender);
        return;
    }
    if (!ORTE_FLAG_TEST(proc, ORTE_PROC_FLAG_LOCAL)) {
        /* send back an error - they obviously have made a mistake */
        send_error(ORTE_ERR_NOT_FOUND, &idreq, sender);
        return;
    }
    /* track the request since the call down to the PMIx server
     * is asynchronous */
    req = OBJ_NEW(pmix_server_req_t);
    req->proxy = *sender;
    req->target = idreq;
    req->remote_room_num = room_num;
    if (OPAL_SUCCESS != (rc = opal_hotel_checkin(&orte_pmix_server_globals.reqs, req, &req->room_num))) {
        OBJ_RELEASE(req);
        send_error(rc, &idreq, sender);
        return;
    }

    /* ask our local pmix server for the data */
    if (OPAL_SUCCESS != (rc = opal_pmix.server_dmodex_request(&idreq, modex_resp, req))) {
        ORTE_ERROR_LOG(rc);
        opal_hotel_checkout(&orte_pmix_server_globals.reqs, req->room_num);
        OBJ_RELEASE(req);
        send_error(rc, &idreq, sender);
        return;
    }
    return;
}

typedef struct {
    opal_object_t super;
    char *data;
    int32_t ndata;
} datacaddy_t;
static void dccon(datacaddy_t *p)
{
    p->data = NULL;
}
static void dcdes(datacaddy_t *p)
{
    if (NULL != p->data) {
        free(p->data);
    }
}
static OBJ_CLASS_INSTANCE(datacaddy_t,
                          opal_object_t,
                          dccon, dcdes);

static void relcbfunc(void *relcbdata)
{
    datacaddy_t *d = (datacaddy_t*)relcbdata;

    OBJ_RELEASE(d);
}

static void pmix_server_dmdx_resp(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata)
{
    int rc, ret, room_num, rnum;
    int32_t cnt;
    opal_process_name_t target;
    pmix_server_req_t *req;
    datacaddy_t *d;

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s dmdx:recv response from proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender));

    /* unpack the status */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the id of the target whose info we just received */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &target, &cnt, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack our tracking room number */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &room_num, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unload the remainder of the buffer */
    d = OBJ_NEW(datacaddy_t);
    if (OPAL_SUCCESS != (rc = opal_dss.unload(buffer, (void**)&d->data, &d->ndata))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* check the request out of the tracking hotel */
    opal_hotel_checkout_and_return_occupant(&orte_pmix_server_globals.reqs, room_num, (void**)&req);
    /* return the returned data to the requestor */
    if (NULL != req) {
        if (NULL != req->mdxcbfunc) {
            OBJ_RETAIN(d);
            req->mdxcbfunc(ret, d->data, d->ndata, req->cbdata, relcbfunc, d);
        }
        OBJ_RELEASE(req);
    }

    /* now see if anyone else was waiting for data from this target */
    for (rnum=0; rnum < orte_pmix_server_globals.reqs.num_rooms; rnum++) {
        opal_hotel_knock(&orte_pmix_server_globals.reqs, rnum, (void**)&req);
        if (NULL == req) {
            continue;
        }
        if (req->target.jobid == target.jobid &&
            req->target.vpid == target.vpid) {
            if (NULL != req->mdxcbfunc) {
                OBJ_RETAIN(d);
                req->mdxcbfunc(ret, d->data, d->ndata, req->cbdata, relcbfunc, d);
            }
            opal_hotel_checkout(&orte_pmix_server_globals.reqs, rnum);
            OBJ_RELEASE(req);
        }
    }
    OBJ_RELEASE(d);  // maintain accounting
}

static void opcon(orte_pmix_server_op_caddy_t *p)
{
    p->procs = NULL;
    p->eprocs = NULL;
    p->info = NULL;
    p->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(orte_pmix_server_op_caddy_t,
                   opal_object_t,
                   opcon, NULL);

static void rqcon(pmix_server_req_t *p)
{
    p->target = *ORTE_NAME_INVALID;
    p->proxy = *ORTE_NAME_INVALID;
    p->timeout = orte_pmix_server_globals.timeout;
    p->jdata = NULL;
    OBJ_CONSTRUCT(&p->msg, opal_buffer_t);
    p->opcbfunc = NULL;
    p->mdxcbfunc = NULL;
    p->spcbfunc = NULL;
    p->lkcbfunc = NULL;
    p->rlcbfunc = NULL;
    p->cbdata = NULL;
}
static void rqdes(pmix_server_req_t *p)
{
    if (NULL != p->jdata) {
        OBJ_RELEASE(p->jdata);
    }
    OBJ_DESTRUCT(&p->msg);
}
OBJ_CLASS_INSTANCE(pmix_server_req_t,
                   opal_object_t,
                   rqcon, rqdes);

static void mdcon(orte_pmix_mdx_caddy_t *p)
{
    p->sig = NULL;
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
static void mddes(orte_pmix_mdx_caddy_t *p)
{
    if (NULL != p->sig) {
        OBJ_RELEASE(p->sig);
    }
}
OBJ_CLASS_INSTANCE(orte_pmix_mdx_caddy_t,
                   opal_object_t,
                   mdcon, mddes);
