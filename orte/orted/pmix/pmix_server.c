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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
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
#include "orte/util/name_fns.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "pmix_server.h"
#include "pmix_server_internal.h"

/*
 * Local utility functions
 */
static void pmix_server_release(int status,
                                opal_buffer_t *buffer,
                                void *cbdata);
static void pmix_server_dmdx_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata);
static void pmix_server_dmdx_resp(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata);

pmix_server_globals_t orte_pmix_server_globals = {0};

static opal_pmix_server_module_t pmix_server = {
    pmix_server_finalized_fn,
    pmix_server_abort_fn,
    pmix_server_fencenb_fn,
    pmix_server_dmodex_req_fn,
    pmix_server_publish_fn,
    pmix_server_lookup_fn,
    pmix_server_unpublish_fn,
    pmix_server_spawn_fn,
    pmix_server_connect_fn,
    pmix_server_disconnect_fn
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
    orte_pmix_server_globals.timeout = -1;
    (void) mca_base_var_register ("orte", "pmix", NULL, "server_max_wait",
                                  "Maximum time the PMIx server should wait to service direct modex requests",
                                  MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                  OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_ALL,
                                  &orte_pmix_server_globals.timeout);
}

static void eviction_cbfunc(struct opal_hotel_t *hotel,
                            int room_num, void *occupant)
{

}

/*
 * Initialize global variables used w/in the server.
 */
int pmix_server_init(void)
{
    int rc;

    if (orte_pmix_server_globals.initialized) {
        return ORTE_SUCCESS;
    }
    orte_pmix_server_globals.initialized = true;

    /* setup the server's state variables */
    OBJ_CONSTRUCT(&orte_pmix_server_globals.dmx_reqs, opal_hotel_t);
    if (orte_pmix_server_globals.timeout <= 0) {
        opal_hotel_init(&orte_pmix_server_globals.dmx_reqs,
                        orte_pmix_server_globals.num_rooms,
                        NULL, 0, 0, NULL);
    } else {
        opal_hotel_init(&orte_pmix_server_globals.dmx_reqs,
                        orte_pmix_server_globals.num_rooms,
                        orte_event_base, orte_pmix_server_globals.timeout,
                        ORTE_ERROR_PRI, eviction_cbfunc);
    }

   /* setup recv for direct modex requests */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX,
                            ORTE_RML_PERSISTENT, pmix_server_dmdx_recv, NULL);

    /* setup recv for replies to direct modex requests */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DIRECT_MODEX_RESP,
                            ORTE_RML_PERSISTENT, pmix_server_dmdx_resp, NULL);

    /* setup the local server */
    if (ORTE_SUCCESS != (rc = opal_pmix.server_init(&pmix_server))) {
        ORTE_ERROR_LOG(rc);
        /* memory cleanup will occur when finalize is called */
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

    /* cleanup collectives */
    OBJ_DESTRUCT(&orte_pmix_server_globals.dmx_reqs);
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

static void modex_resp(int status,
                       const char *data, size_t sz,
                       void *cbdata)
{
    pmix_server_dmx_req_t *req = (pmix_server_dmx_req_t*)cbdata;
    opal_buffer_t *reply, xfer;
    int rc;

    /* check us out of the hotel */
    opal_hotel_checkout(&orte_pmix_server_globals.dmx_reqs, req->room_num);

    reply = OBJ_NEW(opal_buffer_t);
    /* return the status */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &status, 1, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        OBJ_RELEASE(req);
        return;
    }
    /* pack the id of the requested proc */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(reply, &req->target, 1, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(reply);
        OBJ_RELEASE(req);
        return;
    }
    /* return any provided data */
    if (NULL != data) {
        OBJ_CONSTRUCT(&xfer, opal_buffer_t);
        opal_dss.load(&xfer, (void*)data, sz);
        opal_dss.copy_payload(reply, &xfer);
        xfer.base_ptr = NULL; // protect the incoming data
        OBJ_DESTRUCT(&xfer);
    }

    /* send the response */
    orte_rml.send_buffer_nb(&req->proxy, reply,
                            ORTE_RML_TAG_DIRECT_MODEX_RESP,
                            orte_rml_send_callback, NULL);
    OBJ_RELEASE(req);
    return;    
}
static void pmix_server_dmdx_recv(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata)
{
    int rc, ret;
    int32_t cnt;
    opal_buffer_t *reply, *bptr, buf;
    opal_value_t *kvp, *kvp2, kv, *kp;
    opal_process_name_t idreq;
    orte_process_name_t name;
    orte_job_t *jdata;
    orte_proc_t *proc;
    opal_list_t values;
    bool found;
    pmix_server_dmx_req_t *req;

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s dmdx:recv request from proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender));

    /* unpack the id of the proc whose data is being requested */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &idreq, &cnt, OPAL_NAME))) {
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
        req = OBJ_NEW(pmix_server_dmx_req_t);
        req->proxy = *sender;
        req->target = idreq;
        if (OPAL_SUCCESS != (rc = opal_hotel_checkin(&orte_pmix_server_globals.dmx_reqs, req, &req->room_num))) {
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
    req = OBJ_NEW(pmix_server_dmx_req_t);
    req->proxy = *sender;
    req->target = idreq;
    if (OPAL_SUCCESS != (rc = opal_hotel_checkin(&orte_pmix_server_globals.dmx_reqs, req, &req->room_num))) {
        OBJ_RELEASE(req);
        send_error(rc, &idreq, sender);
        return;
    }

    /* ask our local pmix server for the data */
    if (OPAL_SUCCESS != (rc = opal_pmix.server_dmodex_request(&idreq, modex_resp, req))) {
        opal_hotel_checkout(&orte_pmix_server_globals.dmx_reqs, req->room_num);
        OBJ_RELEASE(req);
        send_error(rc, &idreq, sender);
        return;
    }
    return;
}

static void pmix_server_dmdx_resp(int status, orte_process_name_t* sender,
                                  opal_buffer_t *buffer,
                                  orte_rml_tag_t tg, void *cbdata)
{
    pmix_server_dmx_req_t *req, *nxt;
    int rc, ret;
    int32_t cnt;
    opal_buffer_t *reply, xfer, *bptr;
    opal_process_name_t target;
    opal_value_t kv;

    opal_output_verbose(2, orte_pmix_server_globals.output,
                        "%s dmdx:recv response from proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(sender));

    /* unpack the id of the target whose info we just received */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &target, &cnt, OPAL_NAME))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the status */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* unpack the hostname blob */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(buffer, &bptr, &cnt, OPAL_BUFFER))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

#if 0
    /* if we got something, store the blobs locally so we can
     * meet any further requests without doing a remote fetch.
     * This must be done as a single blob for later retrieval */
    if (ORTE_SUCCESS == ret) {
        OBJ_CONSTRUCT(&kv, opal_value_t);
        kv.key = strdup("modex");
        kv.type = OPAL_BYTE_OBJECT;
        OBJ_CONSTRUCT(&xfer, opal_buffer_t);
        opal_dss.copy_payload(&xfer, buffer);
        opal_dss.unload(&xfer, (void**)&kv.data.bo.bytes, &kv.data.bo.size);
        opal_dstore.store(pmix_server_remote_handle, &target, &kv);
        OBJ_DESTRUCT(&kv);
        OBJ_DESTRUCT(&xfer);
    }
#endif
}


static void rqcon(pmix_server_dmx_req_t *p)
{
    p->cbfunc = NULL;
    p->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(pmix_server_dmx_req_t,
                   opal_object_t,
                   rqcon, NULL);

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
