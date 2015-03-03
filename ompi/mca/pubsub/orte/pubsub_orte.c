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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2013      Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"

#include <string.h>
#include <stdio.h>
#include <ctype.h>

#include "opal/util/show_help.h"
#include "opal/util/argv.h"
#include "opal/dss/dss.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_data_server.h"

#include "ompi/info/info.h"
#include "ompi/mca/rte/rte.h"

#include "ompi/mca/pubsub/base/base.h"
#include "pubsub_orte.h"

/* Establish contact with the server
 *
 * NOTE: we do not do this automatically during init to avoid
 * forcing every process to pay the time penalty during MPI_Init
 * when only a few, if any, will ever call pub/lookup/unpub. In
 * addition, those that -do- call these functions may well only
 * use local (as opposed to global) storage, and hence will have
 * no need to talk to the server, even though a sys admin may
 * have set one up. So we do a lazy setup of the server contact
 * info - it only gets setup the first time we call a function
 * that wants to talk to the global server
 */
static bool server_setup=false;

static void setup_server(void)
{
    opal_buffer_t buf;
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, ompi_pubsub_base_framework.framework_output,
                         "%s pubsub:orte: setting up server at URI %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == mca_pubsub_orte_component.server_uri) ? "NULL" : mca_pubsub_orte_component.server_uri));
    
    /* flag setup as completed so we only pass through here once */
    server_setup = true;

    if (NULL == mca_pubsub_orte_component.server_uri) {
        /* if the contact info for the server is NULL, then there
         * is nothing we can do - there is no path to the server
         */
        mca_pubsub_orte_component.server_found = false;
        return;
    }
    
    /* init the route to the server - init_routes wants a buffer
     * passed to it, so we have to package the server's contact
     * info into a buffer
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.pack(&buf, &mca_pubsub_orte_component.server_uri, 1, OPAL_STRING);
    /* extract the server's name so we have its jobid */
    if (ORTE_SUCCESS != (rc = orte_rml_base_parse_uris(mca_pubsub_orte_component.server_uri,
                                                       &mca_pubsub_orte_component.server, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        mca_pubsub_orte_component.server_found = false;
        return;
    }
    /* init routes to the server's job */
    if (ORTE_SUCCESS != (rc = orte_routed.init_routes(mca_pubsub_orte_component.server.jobid, &buf))) {
        ORTE_ERROR_LOG(rc);
        mca_pubsub_orte_component.server_found = false;
        OBJ_DESTRUCT(&buf);
        return;
    }
    OBJ_DESTRUCT(&buf);

    /* flag the server as found */
    mca_pubsub_orte_component.server_found = true;

    OPAL_OUTPUT_VERBOSE((1, ompi_pubsub_base_framework.framework_output,
                         "%s pubsub:orte: server %s setup",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&mca_pubsub_orte_component.server)));
}

/*
 * Init the module
 */
static int init(void)
{    
    return OMPI_SUCCESS;
}

/*
 * publish the port_name for the specified service_name. This will
 * be published under our process name, so only we will be allowed
 * to remove it later.
 */
static int publish ( const char *service_name, ompi_info_t *info, const char *port_name )
{
    int rc, ret, flag;
    bool global_scope = false;
    orte_process_name_t *info_host;
    opal_buffer_t *buf;
    orte_data_server_cmd_t cmd=ORTE_DATA_SERVER_PUBLISH;
    orte_std_cntr_t cnt;
    orte_rml_recv_cb_t xfer;
    bool unique=false;

    ompi_info_get_bool(info, "ompi_global_scope", &global_scope, &flag);

    if (0 == flag) {
        /* scope was not defined - see if server exists */
        if (!server_setup) {
            setup_server();
        }
        if (mca_pubsub_orte_component.server_found) {
            /* server was found - use it as our default store */
            info_host = &mca_pubsub_orte_component.server;
            global_scope = true;
        } else {
            /* server was not found - use our HNP as default store */
            info_host = ORTE_PROC_MY_HNP;
        }
    } else if (!global_scope) {
        /* if the scope is not global, then store the value on the HNP */
        info_host = ORTE_PROC_MY_HNP;
    } else {
        /* has the server been setup yet? */
        if (!server_setup) {
            setup_server();
        }
        /* store the value on the global ompi_server, but error
         * if that server wasn't contacted
         */
        if (!mca_pubsub_orte_component.server_found) {
            opal_show_help("help-ompi-pubsub-orte.txt", "pubsub-orte:no-server",
                           true, (long)ORTE_PROC_MY_NAME->vpid, "publish to");
            return OMPI_ERR_NOT_FOUND;
        }
        info_host = &mca_pubsub_orte_component.server;
    }
    
    OPAL_OUTPUT_VERBOSE((1, ompi_pubsub_base_framework.framework_output,
                         "%s pubsub:orte: publishing service %s scope %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         service_name, global_scope ? "Global" : "Local"));

    ompi_info_get_bool(info, "ompi_unique", &unique, &flag);
    if (0 == flag) {
        /* uniqueness not specified - overwrite by default */
        unique = false;
    }

    /* construct the buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* pack the publish command */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &cmd, 1, ORTE_DATA_SERVER_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    
    /* pack the service name */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &service_name, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    
    /* pack the port name */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &port_name, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }

    /* pack the uniqueness flag */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &unique, 1, OPAL_BOOL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    
    /* send the data */
    if (0 > (rc = orte_rml.send_buffer_nb(info_host, buf,
                                          ORTE_RML_TAG_DATA_SERVER,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }

    /* get the answer */
    OBJ_CONSTRUCT(&xfer, orte_rml_recv_cb_t);
    xfer.active = true;
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD,
                            ORTE_RML_TAG_DATA_CLIENT,
                            ORTE_RML_NON_PERSISTENT,
                            orte_rml_recv_callback, &xfer);
    OMPI_WAIT_FOR_COMPLETION(xfer.active);

    /* unpack the result */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer.data, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
    }
    rc = ret;
    OBJ_DESTRUCT(&xfer);
    
CLEANUP:    
    return rc;
}

enum { NONE, LOCAL, GLOBAL };

static char* lookup ( const char *service_name, ompi_info_t *info )
{
    orte_process_name_t *info_host;
    opal_buffer_t *buf;
    orte_data_server_cmd_t cmd=ORTE_DATA_SERVER_LOOKUP;
    orte_std_cntr_t cnt=0;
    char *port_name=NULL;
    int ret, rc, flag, i;
    char value[256], **tokens, *ptr;
    int lookup[2] = { GLOBAL, LOCAL };
    size_t num_tokens;
    orte_rml_recv_cb_t xfer;

    /* Look in the MPI_Info (ompi_info_t*) for the key
     * "ompi_lookup_order".  Acceptable values are:
     *
     * - "local" -- only check the local scope
     * - "global" -- only check the global scope
     * - "local,global" -- check the local scope first, then check the
     *   global scope
     * - "global,local" -- check the global scope first, then check the
     *   local scope
     *
     * Give a little leeway in terms of whitespace in the value.
     *
     * The lookup[2] array will contain the results: lookup[0] is the
     * first scope to check, lookup[1] is the 2nd.  Either value may
     * be NONE, LOCAL, or GLOBAL.  If both are NONE, clearly that's an
     * error.  :-)
     */
    ompi_info_get(info, "ompi_lookup_order", sizeof(value) - 1, value, &flag);
    if (flag) {
        ptr = &value[0];
        while (isspace(*ptr) && (ptr - value) < (int)sizeof(value)) {
            ++ptr;
        }
        if (ptr - value < (int)sizeof(value)) {
            tokens = opal_argv_split(ptr, ',');
            if (NULL != tokens) {
                if ((num_tokens = opal_argv_count(tokens)) > 2) {
                    /* too many values in the comma-delimited list */
                    opal_show_help("help-ompi-pubsub-orte.txt",
                                   "pubsub-orte:too-many-orders",
                                   true, (long)ORTE_PROC_MY_NAME->vpid,
                                   (long)num_tokens);
                    opal_argv_free(tokens);
                    return NULL;
                }
                for (i = 0; i < 2; ++i) {
                    if (NULL != tokens[i]) {
                        if (0 == strcasecmp(tokens[i], "local")) {
                            lookup[i] = LOCAL;
                        } else if (0 == strcasecmp(tokens[i], "global")) {
                            lookup[i] = GLOBAL;
                        } else {
                            /* unrecognized value -- that's an error */
                            opal_show_help("help-ompi-pubsub-orte.txt",
                                           "pubsub-orte:unknown-order",
                                           true, (long)ORTE_PROC_MY_NAME->vpid);
                            opal_argv_free(tokens);
                            return NULL;
                        }
                    } else {
                        lookup[i] = NONE;
                    }
                }
                opal_argv_free(tokens);
            }
        }
        
        if (NONE == lookup[0]) {
            /* if the user provided an info key, then we at least must
             * be given one place to look
             */
            opal_show_help("help-ompi-pubsub-orte.txt",
                           "pubsub-orte:unknown-order",
                           true, (long)ORTE_PROC_MY_NAME->vpid);
            return NULL;
        }
        
    } else {
        /* if no info key was provided, then we default to the global
         * server IF it is active
         */
        if (!server_setup) {
            setup_server();
        }
        lookup[1] = NONE;
        if (mca_pubsub_orte_component.server_found) {
            lookup[0] = GLOBAL;
        } else {
            /* global server was not found - just look local */
            lookup[0] = LOCAL;
        }
    }
    
    OPAL_OUTPUT_VERBOSE((1, ompi_pubsub_base_framework.framework_output,
                         "%s pubsub:orte: lookup service %s scope %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         service_name, lookup[0]));
    
    /* go find the value */
    for (i=0; i < 2; i++) {
        if (LOCAL == lookup[i]) {
            /* if the scope is local, then lookup the value on the HNP */
            info_host = ORTE_PROC_MY_HNP;
        } else if (GLOBAL == lookup[i]) {
            /* has the server been setup yet? */
            if (!server_setup) {
                setup_server();
            }
            /* lookup the value on the global ompi_server, but error
             * if that server wasn't contacted
             */
            if (!mca_pubsub_orte_component.server_found) {
                opal_show_help("help-ompi-pubsub-orte.txt",
                               "pubsub-orte:no-server",
                               true, (long)ORTE_PROC_MY_NAME->vpid,
                               "lookup from");
                return NULL;
            }
            info_host = &mca_pubsub_orte_component.server;
        } else if (NONE == lookup[i]) {
            continue;
        } else {
            /* unknown host! */
            opal_show_help("help-ompi-pubsub-orte.txt",
                           "pubsub-orte:unknown-order",
                           true, (long)ORTE_PROC_MY_NAME->vpid);
            return NULL;
        }
        
        /* go look it up */
        /* construct the buffer */
        buf = OBJ_NEW(opal_buffer_t);
        
        /* pack the lookup command */
        if (OPAL_SUCCESS != (ret = opal_dss.pack(buf, &cmd, 1, ORTE_DATA_SERVER_CMD))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buf);
            goto CLEANUP;
        }
        
        /* pack the service name */
        if (OPAL_SUCCESS != (ret = opal_dss.pack(buf, &service_name, 1, OPAL_STRING))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buf);
            goto CLEANUP;
        }
        
        /* send the cmd */
        if (0 > (ret = orte_rml.send_buffer_nb(info_host, buf,
                                               ORTE_RML_TAG_DATA_SERVER,
                                               orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(ret);
            OBJ_RELEASE(buf);
            goto CLEANUP;
        }
        
        /* get the answer */
        OBJ_CONSTRUCT(&xfer, orte_rml_recv_cb_t);
        xfer.active = true;
        orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, 
                                ORTE_RML_TAG_DATA_CLIENT,
                                ORTE_RML_NON_PERSISTENT,
                                orte_rml_recv_callback, &xfer);
        OMPI_WAIT_FOR_COMPLETION(xfer.active);

        /* unpack the return code */
        cnt = 1;
        if (OPAL_SUCCESS != (ret = opal_dss.unpack(&xfer.data, &rc, &cnt, OPAL_INT))) {
            ORTE_ERROR_LOG(ret);
            goto CLEANUP;
        }

        OPAL_OUTPUT_VERBOSE((1, ompi_pubsub_base_framework.framework_output,
                             "%s pubsub:orte: lookup returned status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), rc));

        if (ORTE_SUCCESS == rc) {
            /* the server was able to lookup the port - unpack the port name */
            cnt=1;
            if (OPAL_SUCCESS != (ret = opal_dss.unpack(&xfer.data, &port_name, &cnt, OPAL_STRING))) {
                ORTE_ERROR_LOG(ret);
                OBJ_DESTRUCT(&xfer);
                goto CLEANUP;
            }
            
            OPAL_OUTPUT_VERBOSE((1, ompi_pubsub_base_framework.framework_output,
                                 "%s pubsub:orte: lookup returned port %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (NULL == port_name) ? "NULL" : port_name));

            if (NULL != port_name) {
                /* got an answer - return it */
                OBJ_DESTRUCT(&xfer);
                return port_name;
            }
        }
        
        /* if we didn't get a port_name, then continue */
        OBJ_DESTRUCT(&xfer);
    }
    
    /* only get here if we tried both options and failed - since the
     * buffer will already have been cleaned up, just return
     */
 CLEANUP:
    return NULL;
}

/*
 * delete the entry. Only the process who has published
 * the service_name has the right to remove this
 * service - the server will verify and report the result
 */
static int unpublish ( const char *service_name, ompi_info_t *info )
{
    int rc, ret, flag;
    bool global_scope;
    orte_process_name_t *info_host;
    opal_buffer_t *buf;
    orte_data_server_cmd_t cmd=ORTE_DATA_SERVER_UNPUBLISH;
    orte_std_cntr_t cnt;
    orte_rml_recv_cb_t xfer;

    ompi_info_get_bool(info, "ompi_global_scope", &global_scope, &flag);

    if (0 == flag) {
        /* scope was not defined - see if server exists */
        if (!server_setup) {
            setup_server();
        }
        if (mca_pubsub_orte_component.server_found) {
            /* server was found - use it as our default store */
            info_host = &mca_pubsub_orte_component.server;
            global_scope = true;
        } else {
            /* server was not found - use our HNP as default store */
            info_host = ORTE_PROC_MY_HNP;
        }
    } else if (!global_scope) {
        /* if the scope is not global, then unpublish the value from the HNP */
        info_host = ORTE_PROC_MY_HNP;
    } else {
        /* has the server been setup yet? */
        if (!server_setup) {
            setup_server();
        }
        /* unpublish the value from the global ompi_server, but error
        * if that server wasn't contacted
        */
        if (!mca_pubsub_orte_component.server_found) {
            opal_show_help("help-ompi-pubsub-orte.txt", "pubsub-orte:no-server",
                           true, (long)ORTE_PROC_MY_NAME->vpid, "unpublish from");
            return OMPI_ERR_NOT_FOUND;
        }
        info_host = &mca_pubsub_orte_component.server;
    }
    
    OPAL_OUTPUT_VERBOSE((1, ompi_pubsub_base_framework.framework_output,
                         "%s pubsub:orte: unpublish service %s scope %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         service_name, global_scope ? "Global" : "Local"));
    
    /* construct the buffer */
    buf = OBJ_NEW(opal_buffer_t);
    
    /* pack the unpublish command */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &cmd, 1, ORTE_DATA_SERVER_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    
    /* pack the service name */
    if (OPAL_SUCCESS != (rc = opal_dss.pack(buf, &service_name, 1, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    
    /* send the command */
    if (0 > (rc = orte_rml.send_buffer_nb(info_host, buf, ORTE_RML_TAG_DATA_SERVER,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buf);
        goto CLEANUP;
    }
    
    /* get the answer */
    OBJ_CONSTRUCT(&xfer, orte_rml_recv_cb_t);
    xfer.active = true;
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_DATA_CLIENT,
                            ORTE_RML_NON_PERSISTENT,
                            orte_rml_recv_callback, &xfer);
    OMPI_WAIT_FOR_COMPLETION(xfer.active);
    
    /* unpack the result */
    cnt = 1;
    if (OPAL_SUCCESS != (rc = opal_dss.unpack(&xfer.data, &ret, &cnt, OPAL_INT))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&xfer);
        goto CLEANUP;
    }
    OBJ_DESTRUCT(&xfer);
    rc = ret;
    
CLEANUP:    
    return rc;
}


/*
 * finalize the module
 */
static int finalize(void)
{
    return OMPI_SUCCESS;
}

/*
 * instantiate the module
 */
ompi_pubsub_base_module_t ompi_pubsub_orte_module = {
    init,
    publish,
    unpublish,
    lookup,
    finalize
};


