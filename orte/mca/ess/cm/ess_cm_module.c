/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#include <stdio.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/if.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/mca/sysinfo/sysinfo.h"
#include "opal/mca/sysinfo/base/base.h"

#include "orte/mca/rmcast/base/base.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/plm/base/base.h"
#include "orte/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/cm/ess_cm.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int status, bool report) __opal_attribute_noreturn__;
static uint8_t proc_get_locality(orte_process_name_t *proc);
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);


orte_ess_base_module_t orte_ess_cm_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    proc_get_locality,
    proc_get_daemon,
    proc_get_hostname,
    proc_get_local_rank,
    proc_get_node_rank,
    update_pidmap,
    update_nidmap,
    NULL /* ft_event */
};

static int cm_set_name(void);

static int rte_init(void)
{
    int ret;
    char *error = NULL;
    char **hosts = NULL;
    char *nodelist;

    /* only daemons that are bootstrapping should
     * be calling this module
     */

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    if (ORTE_PROC_IS_DAEMON) {
        /* if we do not know the HNP, then we have to
         * use the multicast system to find it
         */
        if (NULL == orte_process_info.my_hnp_uri) {
            /* open the reliable multicast framework */
            if (ORTE_SUCCESS != (ret = orte_rmcast_base_open())) {
                ORTE_ERROR_LOG(ret);
                error = "orte_rmcast_base_open";
                goto error;
            }
            
            if (ORTE_SUCCESS != (ret = orte_rmcast_base_select())) {
                ORTE_ERROR_LOG(ret);
                error = "orte_rmcast_base_select";
                goto error;
            }

            /* open and setup the local resource discovery framework */
            if (ORTE_SUCCESS != (ret = opal_sysinfo_base_open())) {
                ORTE_ERROR_LOG(ret);
                error = "opal_sysinfo_base_open";
                goto error;
            }
            if (ORTE_SUCCESS != (ret = opal_sysinfo_base_select())) {
                ORTE_ERROR_LOG(ret);
                error = "opal_sysinfo_base_select";
                goto error;
            }

            /* get a name for ourselves */
            if (ORTE_SUCCESS != (ret = cm_set_name())) {
                error = "set_name";
                goto error;
            }
        } else {
            /* if we were given an HNP, then we must have also
             * been given a vpid - we can get the jobid from
             * the HNP's name
             */
            ORTE_PROC_MY_NAME->jobid = orte_process_info.my_hnp.jobid;
            
        }
        
        /* get the list of nodes used for this job */
        nodelist = getenv("OMPI_MCA_orte_nodelist");
        
        if (NULL != nodelist) {
            /* split the node list into an argv array */
            hosts = opal_argv_split(nodelist, ',');
        }
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup(hosts))) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
        opal_argv_free(hosts);
    } else if (ORTE_PROC_IS_TOOL) {
        if (ORTE_SUCCESS != (ret = orte_plm_base_open())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_open";
            goto error;
        }
        
        if (ORTE_SUCCESS != (ret = orte_plm_base_select())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_base_select";
            goto error;
        }
        if (ORTE_SUCCESS != (ret = orte_plm.set_hnp_name())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_plm_set_hnp_name";
            goto error;
        }
        /* close the plm since we opened it to set our
         * name, but have no further use for it
         */
        orte_plm_base_close();

        /* if we do not know the HNP, then we have to use
         * the multicast system to find it
         */
        if (NULL == orte_process_info.my_hnp_uri) {
            /* open the reliable multicast framework */
            if (ORTE_SUCCESS != (ret = orte_rmcast_base_open())) {
                ORTE_ERROR_LOG(ret);
                error = "orte_rmcast_base_open";
                goto error;
            }
            
            if (ORTE_SUCCESS != (ret = orte_rmcast_base_select())) {
                ORTE_ERROR_LOG(ret);
                error = "orte_rmcast_base_select";
                goto error;
            }
            /* checkin with the HNP */
            if (ORTE_SUCCESS != (ret = cm_set_name())) {
                error = "set_name";
                goto error;
            }            
        }
        
        /* do the rest of the standard tool init */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto error;
        }
    }
    return ORTE_SUCCESS;
    
error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    
    return ret;
}

static int rte_finalize(void)
{
    int ret = ORTE_SUCCESS;
    
    if (ORTE_PROC_IS_DAEMON) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        
        /* deconstruct the nidmap and jobmap arrays */
        orte_util_nidmap_finalize();
    } else if (ORTE_PROC_IS_TOOL) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    return ret;    
}

/*
 * If we are a cm, it could be beneficial to get a core file, so
 * we call abort.
 */
static void rte_abort(int status, bool report)
{
    /* do NOT do a normal finalize as this will very likely
     * hang the process. We are aborting due to an abnormal condition
     * that precludes normal cleanup 
     *
     * We do need to do the following bits to make sure we leave a 
     * clean environment. Taken from orte_finalize():
     * - Assume errmgr cleans up child processes before we exit.
     */
    
    /* - Clean out the global structures 
     * (not really necessary, but good practice)
     */
    orte_proc_info_finalize();
    
    /* Now exit/abort */
    if (report) {
        abort();
    }
    
    /* otherwise, just exit */
    exit(status);
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return OPAL_PROC_NON_LOCAL;
    }
    
    if (nid->daemon == ORTE_PROC_MY_DAEMON->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                             "%s ess:cm: proc %s on LOCAL NODE",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return OPAL_PROC_NON_LOCAL;
    
}

static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if( ORTE_JOBID_IS_DAEMON(proc->jobid) ) {
        return proc->vpid;
    }
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        return ORTE_VPID_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(nid->daemon)));
    
    return nid->daemon;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->name));
    
    return nid->name;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->local_rank));
    
    return pmap->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    /* is this me? */
    if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
        proc->vpid == ORTE_PROC_MY_NAME->vpid) {
        /* yes it is - since I am a daemon, it can only
         * be zero
         */
        return 0;
    }
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        return ORTE_NODE_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:cm: updating pidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* build the pmap */
    if (ORTE_SUCCESS != (ret = orte_util_decode_pidmap(bo))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

static int update_nidmap(opal_byte_object_t *bo)
{
    int rc;
    /* decode the nidmap - the util will know what to do */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
        ORTE_ERROR_LOG(rc);
    }    
    return rc;
}

/* support for setting name */
static bool arrived = false;
static bool name_success = false;

static void cbfunc(int status,
                   int channel, orte_rmcast_tag_t tag,
                   orte_process_name_t *sender,
                   orte_rmcast_seq_t seq_num,
                   opal_buffer_t *buf, void *cbdata)
{
    int32_t n, np;
    orte_daemon_cmd_flag_t cmd;
    orte_process_name_t name;
    int rc;
    char *uri;
    char *host;
    
    /* ensure we default to failure */
    name_success = false;
    
    /* unpack the cmd */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &cmd, &n, ORTE_DAEMON_CMD_T))) {
        ORTE_ERROR_LOG(rc);
        arrived = true;
        return;
    }

    if (ORTE_DAEMON_NAME_REQ_CMD == cmd) {
        /* unpack the intended recipient's hostname */
        n=1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &host, &n, OPAL_STRING))) {
            ORTE_ERROR_LOG(rc);
            arrived = true;
            return;
        }
        
        /* is this intended for me? */
        if (0 != strcmp(host, orte_process_info.nodename)) {
            /* nope - ignore it */
            return;
        }
        
        /* unpack the name */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &name, &n, ORTE_NAME))) {
            ORTE_ERROR_LOG(rc);
            arrived = true;
            return;
        }
        /* if we got an invalid name, then declare failure */
        if (ORTE_JOBID_INVALID == name.jobid &&
            ORTE_VPID_INVALID == name.vpid) {
            arrived = true;
            return;
        }
        ORTE_PROC_MY_NAME->jobid = name.jobid;
        ORTE_PROC_MY_NAME->vpid = name.vpid;
        OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                             "set my name to %s", ORTE_NAME_PRINT(&name)));
    }

    /* unpack the HNP uri */
    n = 1;
    if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &uri, &n, OPAL_STRING))) {
        ORTE_ERROR_LOG(rc);
        arrived = true;
        return;
    }
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "%s got hnp uri %s",
                         ORTE_NAME_PRINT(&name), uri));
    orte_process_info.my_hnp_uri = uri;
    
    if (ORTE_DAEMON_NAME_REQ_CMD == cmd ||
        ORTE_DAEMON_CHECKIN_CMD == cmd) {
        /* unpack the number of daemons */
        n = 1;
        if (ORTE_SUCCESS != (rc = opal_dss.unpack(buf, &np, &n, OPAL_INT32))) {
            ORTE_ERROR_LOG(rc);
            arrived = true;
            return;
        }
        orte_process_info.num_procs = np;
    }
    
    name_success = true;
    arrived = true;
}

static int cm_set_name(void)
{
    int rc;
    opal_buffer_t buf;
    orte_daemon_cmd_flag_t cmd;
    char *keys[] = {
        OPAL_SYSINFO_CPU_TYPE,
        OPAL_SYSINFO_CPU_MODEL,
        OPAL_SYSINFO_NUM_CPUS,
        OPAL_SYSINFO_MEM_SIZE,
        NULL
    };
    opal_list_t resources;
    opal_list_item_t *item;
    opal_sysinfo_value_t *info;
    int32_t num_values;
    
    /* setup the query */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);

    if (ORTE_PROC_IS_DAEMON) {
        /* use the reliable multicast system to contact the HNP and
         * get a name
         */
        cmd = ORTE_DAEMON_NAME_REQ_CMD;
        opal_dss.pack(&buf, &cmd, 1, ORTE_DAEMON_CMD_T);
    } else if (ORTE_PROC_IS_TOOL) {
        cmd = ORTE_TOOL_CHECKIN_CMD;
        opal_dss.pack(&buf, &cmd, 1, ORTE_DAEMON_CMD_T);
    }

    /* always include our node name */
    opal_dss.pack(&buf, &orte_process_info.nodename, 1, OPAL_STRING);

    /* get our local resources */
    if (ORTE_PROC_IS_DAEMON) {
        OBJ_CONSTRUCT(&resources, opal_list_t);
        opal_sysinfo.query(keys, &resources);
        /* add number of values to the buffer */
        num_values = opal_list_get_size(&resources);
        opal_dss.pack(&buf, &num_values, 1, OPAL_INT32);
        /* add them to the buffer */
        while (NULL != (item = opal_list_remove_first(&resources))) {
            info = (opal_sysinfo_value_t*)item;
            opal_dss.pack(&buf, &info->key, 1, OPAL_STRING);
            opal_dss.pack(&buf, &info->type, 1, OPAL_DATA_TYPE_T);
            if (OPAL_INT64 == info->type) {
                opal_dss.pack(&buf, &(info->data.i64), 1, OPAL_INT64);
            } else if (OPAL_STRING == info->type) {
                opal_dss.pack(&buf, &(info->data.str), 1, OPAL_STRING);
            }
            /* if this is the cpu model, save it for later use */
            if (0 == strcmp(info->key, OPAL_SYSINFO_CPU_MODEL)) {
                orte_local_cpu_model = strdup(info->data.str);
            }
            OBJ_RELEASE(info);
        }
        OBJ_DESTRUCT(&resources);                
    }
    
    /* set the recv to get the answer */
    if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_SYS_CHANNEL,
                                                         ORTE_RMCAST_TAG_BOOTSTRAP,
                                                         ORTE_RMCAST_PERSISTENT,
                                                         cbfunc, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    
    /* send the request */
    if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer(ORTE_RMCAST_SYS_CHANNEL,
                                                      ORTE_RMCAST_TAG_BOOTSTRAP,
                                                      &buf))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buf);
        return rc;
    }
    OBJ_DESTRUCT(&buf);

    /* wait for response */
    ORTE_PROGRESSED_WAIT(arrived, 0, 1);
    
    /* cancel the recv */
    orte_rmcast.cancel_recv(ORTE_RMCAST_SYS_CHANNEL,
                            ORTE_RMCAST_TAG_BOOTSTRAP);
    
    /* if we got a valid name, return success */
    if (name_success) {
        return ORTE_SUCCESS;
    }

    return ORTE_ERR_NOT_FOUND;
}
