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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <ctype.h>
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif


#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss.h"
#include "opal/mca/paffinity/paffinity.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/nidmap.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/slurm/ess_slurm.h"

static char *get_slurm_nodename(int nodeid);
static int slurm_set_name(void);
static int build_daemon_nidmap(void);

static int rte_init(void);
static int rte_finalize(void);
static uint8_t proc_get_locality(orte_process_name_t *proc);
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static uint32_t proc_get_arch(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_arch(orte_process_name_t *proc, uint32_t arch);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);

orte_ess_base_module_t orte_ess_slurm_module = {
    rte_init,
    rte_finalize,
    orte_ess_base_app_abort,
    proc_get_locality,
    proc_get_daemon,
    proc_get_hostname,
    proc_get_arch,
    proc_get_local_rank,
    proc_get_node_rank,
    update_arch,
    update_pidmap,
    update_nidmap,
    NULL /* ft_event */
};


static int rte_init(void)
{
    int ret;
    char *error = NULL;

    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    /* Start by getting a unique name */
    slurm_set_name();
    
    /* if I am a daemon, complete my setup using the
     * default procedure
     */
    if (ORTE_PROC_IS_DAEMON) {
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_orted_setup";
            goto error;
        }
        /* if we are using static ports, then we need to setup
         * the daemon info so the RML can function properly
         * without requiring a wireup stage
         */
        if (orte_static_ports) {
            /* construct the nidmap arrays */
            if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
                ORTE_ERROR_LOG(ret);
                error = "orte_util_nidmap_init";
                goto error;
            }
            if (ORTE_SUCCESS != (ret = orte_util_setup_local_nidmap_entries())) {
                ORTE_ERROR_LOG(ret);
                error = "orte_util_nidmap_init";
                goto error;
            }
            /* extract the node info from the environment and
             * build a nidmap from it
             */
            if (ORTE_SUCCESS != (ret = build_daemon_nidmap())) {
                ORTE_ERROR_LOG(ret);
                error = "construct daemon map from static ports";
                goto error;
            }
            return ORTE_SUCCESS;
        }
    } else if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_tool_setup";
            goto error;
        }
        /* as a tool, I don't need a nidmap - so just return now */
        return ORTE_SUCCESS;
        
    } else {
        /* otherwise, I must be an application process - use
         * the default procedure to finish my setup
         */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
            ORTE_ERROR_LOG(ret);
            error = "orte_ess_base_app_setup";
            goto error;
        }
    }
    
    /* setup the nidmap arrays */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(orte_process_info.sync_buf))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
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
    int ret;
   
    /* if I am a daemon, finalize using the default procedure */
    if (ORTE_PROC_IS_DAEMON) {
        /* don't need to do the barrier */
        orte_orted_exit_with_barrier = false;
        if (ORTE_SUCCESS != (ret = orte_ess_base_orted_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    } else if (ORTE_PROC_IS_TOOL) {
        /* otherwise, if I am a tool proc, use that procedure */
        if (ORTE_SUCCESS != (ret = orte_ess_base_tool_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
        /* as a tool, I didn't create a nidmap - so just return now */
        return ret;
    } else {
        /* otherwise, I must be an application process
         * use the default procedure to finish
         */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    /* deconstruct my nidmap and jobmap arrays */
    orte_util_nidmap_finalize();
    
    return ret;    
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
                             "%s ess:slurm: proc %s is LOCAL",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return OPAL_PROC_NON_LOCAL;
    
}

static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        /* don't generate an error message here - it could be a call to
         * get a route to a proc in an unknown job. Let the caller decide
         * if an error message is required
         */
        return ORTE_VPID_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s is hosted by daemon %s",
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
                         "%s ess:slurm: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->name));
    
    return nid->name;
}

static uint32_t proc_get_arch(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return 0;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s has arch %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->arch));
    
    return nid->arch;
}

static int update_arch(orte_process_name_t *proc, uint32_t arch)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: updating proc %s to arch %0x",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         arch));
    
    nid->arch = arch;
    
    return ORTE_SUCCESS;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->local_rank));
    
    return pmap->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        return ORTE_NODE_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurm: updating pidmap",
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

static int slurm_set_name(void)
{
    int slurm_nodeid;
    int rc;
    int id;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    char* jobid_string;
    char* vpid_string;
    
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:slurm setting name"));
    
    id = mca_base_param_register_string("orte", "ess", "jobid", NULL, NULL);
    mca_base_param_lookup_string(id, &jobid_string);
    if (NULL == jobid_string) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_jobid(&jobid, jobid_string))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    
    id = mca_base_param_register_string("orte", "ess", "vpid", NULL, NULL);
    mca_base_param_lookup_string(id, &vpid_string);
    if (NULL == vpid_string) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    if (ORTE_SUCCESS != (rc = orte_util_convert_string_to_vpid(&vpid, vpid_string))) {
        ORTE_ERROR_LOG(rc);
        return(rc);
    }
    
    ORTE_PROC_MY_NAME->jobid = jobid;
    
    /* fix up the vpid and make it the "real" vpid */
    slurm_nodeid = atoi(getenv("SLURM_NODEID"));
    ORTE_PROC_MY_NAME->vpid = vpid + slurm_nodeid;

    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:slurm set name to %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* fix up the system info nodename to match exactly what slurm returned */
    if (NULL != orte_process_info.nodename) {
        free(orte_process_info.nodename);
    }
    orte_process_info.nodename = get_slurm_nodename(slurm_nodeid);

    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "ess:slurm set nodename to %s",
                         orte_process_info.nodename));
    
    /* get the non-name common environmental variables */
    if (ORTE_SUCCESS != (rc = orte_ess_env_get())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

static char *
get_slurm_nodename(int nodeid)
{
    char **names = NULL;
    char *slurm_nodelist;
    char *ret;

    slurm_nodelist = getenv("OMPI_MCA_orte_slurm_nodelist");
    
    if (NULL == slurm_nodelist) {
        return NULL;
    }
    
    /* split the node list into an argv array */
    names = opal_argv_split(slurm_nodelist, ',');
    if (NULL == names) {  /* got an error */
        return NULL;
    }

    /* check to see if there are enough entries */
    if (nodeid > opal_argv_count(names)) {
        return NULL;
    }

    ret = strdup(names[nodeid]);

    opal_argv_free(names);

    /* All done */
    return ret;
}

static int build_daemon_nidmap(void)
{
    char **names = NULL;
    char *slurm_nodelist;
    orte_nid_t *node;
    int i, num_nodes;
    int rc;
    struct hostent *h;
    opal_buffer_t buf;
    orte_process_name_t proc;
    char *uri, *addr;
    char *proc_name;
    
    OPAL_OUTPUT_VERBOSE((0, orte_ess_base_output,
                         "ess:slurm build daemon nidmap"));
    
    slurm_nodelist = getenv("OMPI_MCA_orte_slurm_nodelist");
    
    if (NULL == slurm_nodelist) {
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* split the node list into an argv array */
    names = opal_argv_split(slurm_nodelist, ',');
    if (NULL == names) {  /* got an error */
        return ORTE_ERR_NOT_FOUND;
    }
    
    num_nodes = opal_argv_count(names);
    
    OPAL_OUTPUT_VERBOSE((0, orte_ess_base_output,
                         "ess:slurm:build:daemon:nidmap found %d nodes", num_nodes));

    /* set the size of the nidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (rc = opal_pointer_array_set_size(&orte_nidmap, num_nodes+1))) {
        return rc;
    }

    /* install the entry for the HNP */
    node = OBJ_NEW(orte_nid_t);
    node->name = strdup("HNP");
    node->daemon = 0;
    /* the arch defaults to our arch so that non-hetero
     * case will yield correct behavior
     */
    opal_pointer_array_set_item(&orte_nidmap, 0, node);        
    
    /* the daemon vpids will be assigned in order,
     * starting with vpid=1 for the first node in
     * the list
     */
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    proc.jobid = ORTE_PROC_MY_NAME->jobid;
    for (i=0; i < num_nodes; i++) {
        node = OBJ_NEW(orte_nid_t);
        node->name = strdup(names[i]);
        node->daemon = i+1;
        /* the arch defaults to our arch so that non-hetero
         * case will yield correct behavior
         */
        opal_pointer_array_set_item(&orte_nidmap, node->daemon, node);        
        
        opal_output(0, "%s lookup address for node %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), node->name);
        /* lookup the address of this node */
        if (NULL == (h = gethostbyname(node->name))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        addr = inet_ntoa(*(struct in_addr*)h->h_addr_list[0]);
                         
        OPAL_OUTPUT_VERBOSE((0, orte_ess_base_output,
                             "ess:slurm:build:daemon:nidmap node %s daemon %d addr %s",
                             node->name, (int)node->daemon, addr));
        
        /* since we are using static ports, all my fellow daemons will be on my
         * port. Setup the contact info for each daemon in my hash tables. Note
         * that this will -not- open a port to those daemons, but will only
         * define the info necessary for opening such a port if/when I communicate
         * to them
         */
        /* construct the URI */
        proc.vpid = node->daemon;
orte_util_convert_process_name_to_string(&proc_name, &proc);
        asprintf(&uri, "%s;tcp://%s:%d", proc_name, addr, (int)orte_process_info.my_port);
opal_output(0, "contact info %s", uri);
        opal_dss.pack(&buf, &uri, 1, OPAL_STRING);
        free(proc_name);
        free(uri);
    }

    /* load the hash tables */
    if (ORTE_SUCCESS != (rc = orte_rml_base_update_contact_info(&buf))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buf);
    
    opal_argv_free(names);
    
    /* All done */
    return ORTE_SUCCESS;
}
