/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2011 Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights 
 *                         reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/show_help.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/rsh/plm_rsh.h"


/*
 * Public string showing the plm ompi_rsh component version number
 */
const char *mca_plm_rsh_component_version_string =
  "Open MPI rsh plm MCA component version " ORTE_VERSION;


static int rsh_component_open(void);
static int rsh_component_query(mca_base_module_t **module, int *priority);
static int rsh_component_close(void);
static int rsh_launch_agent_lookup(const char *agent_list, char *path);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_plm_rsh_component_t mca_plm_rsh_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        ORTE_PLM_BASE_VERSION_2_0_0,

        /* Component name and version */
        "rsh",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        rsh_component_open,
        rsh_component_close,
        rsh_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
    }
};



static int rsh_component_open(void)
{
    int tmp, value;
    mca_base_component_t *c = &mca_plm_rsh_component.super.base_version;
    char *ctmp, **cargv;

    /* initialize globals */
    OBJ_CONSTRUCT(&mca_plm_rsh_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_plm_rsh_component.cond, opal_condition_t);
    mca_plm_rsh_component.using_qrsh = false;
    mca_plm_rsh_component.using_llspawn = false;

    /* lookup parameters */
    mca_base_param_reg_int(c, "num_concurrent",
                           "How many plm_rsh_agent instances to invoke concurrently (must be > 0)",
                           false, false, 128, &tmp);
    if (tmp <= 0) {
        orte_show_help("help-plm-rsh.txt", "concurrency-less-than-zero",
                       true, tmp);
        tmp = 1;
    }
    mca_plm_rsh_component.num_concurrent = tmp;

    mca_base_param_reg_int(c, "force_rsh",
                           "Force the launcher to always use rsh",
                           false, false, false, &tmp);
    mca_plm_rsh_component.force_rsh = OPAL_INT_TO_BOOL(tmp);
    mca_base_param_reg_int(c, "disable_qrsh",
                           "Disable the launcher to use qrsh when under the Grid Engine parallel environment",
                           false, false, false, &tmp);
    mca_plm_rsh_component.disable_qrsh = OPAL_INT_TO_BOOL(tmp);

    mca_base_param_reg_int(c, "daemonize_qrsh",
                           "Daemonize the orted under the Grid Engine parallel environment",
                           false, false, false, &tmp);
    mca_plm_rsh_component.daemonize_qrsh = OPAL_INT_TO_BOOL(tmp);
    
    mca_base_param_reg_int(c, "disable_llspawn",
                           "Disable the use of llspawn when under the LoadLeveler environment",
                           false, false, false, &tmp);
    mca_plm_rsh_component.disable_llspawn = OPAL_INT_TO_BOOL(tmp);

    mca_base_param_reg_int(c, "daemonize_llspawn",
                           "Daemonize the orted when under the LoadLeveler environment",
                           false, false, false, &tmp);
    mca_plm_rsh_component.daemonize_llspawn = OPAL_INT_TO_BOOL(tmp);
    
    mca_base_param_reg_int(c, "priority",
                           "Priority of the rsh plm component",
                           false, false, 10,
                           &mca_plm_rsh_component.priority);
    mca_base_param_reg_string(c, "delay",
                              "Delay between invocations of the remote agent (sec[:usec])",
                              false, false, NULL,
                              &ctmp);
    if (NULL != ctmp) {
        cargv = opal_argv_split(ctmp, ':');
        mca_plm_rsh_component.delay.tv_sec = strtol(cargv[0], NULL, 10);
        if (1 < opal_argv_count(cargv)) {
            mca_plm_rsh_component.delay.tv_nsec = 1000 * strtol(cargv[1], NULL, 10);
        }
        opal_argv_free(cargv);
        free(ctmp);
    }

    mca_base_param_reg_int(c, "no_tree_spawn",
                           "If set to 1, do not launch via a tree-based topology",
                           false, false, 0, &tmp);
    if (0 == tmp) {
        mca_plm_rsh_component.tree_spawn = true;
    } else {
        mca_plm_rsh_component.tree_spawn = false;
    }

    /* local rsh/ssh launch agent */
    tmp = mca_base_param_reg_string(c, "agent",
                                    "The command used to launch executables on remote nodes (typically either \"ssh\" or \"rsh\")",
                                    false, false, "ssh : rsh", NULL);
    mca_base_param_reg_syn_name(tmp, "pls", "rsh_agent", true);
    mca_base_param_reg_syn_name(tmp, "orte", "rsh_agent", true);
    mca_base_param_lookup_string(tmp, &mca_plm_rsh_component.agent);

    tmp = mca_base_param_reg_int_name("orte", "assume_same_shell",
                                      "If set to 1, assume that the shell on the remote node is the same as the shell on the local node.  Otherwise, probe for what the remote shell [default: 1]",
                                      false, false, 1, NULL);
    mca_base_param_reg_syn_name(tmp, "plm", "rsh_assume_same_shell", true);
    mca_base_param_lookup_int(tmp, &value);
    mca_plm_rsh_component.assume_same_shell = OPAL_INT_TO_BOOL(value);
    
    return ORTE_SUCCESS;
}


static int rsh_component_query(mca_base_module_t **module, int *priority)
{
    char *tmp;
    
    /* Check if we are under Grid Engine parallel environment by looking at several
     * environment variables.  If so, setup the path and argv[0]. */
    if (!mca_plm_rsh_component.disable_qrsh &&
        NULL != getenv("SGE_ROOT") && NULL != getenv("ARC") &&
        NULL != getenv("PE_HOSTFILE") && NULL != getenv("JOB_ID")) {
        /* setup the search path for qrsh */
        asprintf(&tmp, "%s/bin/%s", getenv("SGE_ROOT"), getenv("ARC"));
        /* see if the agent is available */
        if (ORTE_SUCCESS != rsh_launch_agent_lookup("qrsh", tmp)) {
            /* can't be SGE */
             opal_output_verbose(1, orte_plm_globals.output,
                                "%s plm:rsh: unable to be used: SGE indicated but cannot find path "
                                "or execution permissions not set for launching agent qrsh", 
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
             free(tmp);
             *module = NULL;
             return ORTE_ERROR;
        }
        free(tmp);
        mca_plm_rsh_component.using_qrsh = true;
        /* no tree spawn allowed under qrsh */
        mca_plm_rsh_component.tree_spawn = false;
        goto success; 
    } else if (!mca_plm_rsh_component.disable_llspawn &&
               NULL != getenv("LOADL_STEP_ID")) { 
	/* We are running  as a LOADLEVELER job.
	   Search for llspawn in the users PATH */
        if (ORTE_SUCCESS != rsh_launch_agent_lookup("llspawn", NULL)) {
             opal_output_verbose(1, orte_plm_globals.output,
                                "%s plm:rsh: unable to be used: LoadLeveler "
                                "indicated but cannot find path or execution "
                                "permissions not set for launching agent llspawn",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            *module = NULL;
            return ORTE_ERROR;
        }
        mca_plm_rsh_component.using_llspawn = true;
        goto success;
    }
    
    /* if this isn't an Grid Engine or LoadLeveler environment, 
       see if MCA-specified agent (default: ssh:rsh) is available */
    
    if (ORTE_SUCCESS != rsh_launch_agent_lookup(NULL, NULL)) {
        /* this isn't an error - we just cannot be selected */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: unable to be used: cannot find path "
                             "for launching agent \"%s\"\n", 
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             mca_plm_rsh_component.agent));
        *module = NULL;
        return ORTE_ERROR;
    }
success: 
    /* we are good - make ourselves available */
    *priority = mca_plm_rsh_component.priority;
    *module = (mca_base_module_t *) &orte_plm_rsh_module;
    return ORTE_SUCCESS;
}


static int rsh_component_close(void)
{
    /* cleanup state */
    OBJ_DESTRUCT(&mca_plm_rsh_component.lock);
    OBJ_DESTRUCT(&mca_plm_rsh_component.cond);

    return ORTE_SUCCESS;
}

/*
 * Take a colon-delimited list of agents and locate the first one that
 * we are able to find in the PATH.  Split that one into argv and
 * return it.  If nothing found, then return NULL.
 */
char **orte_plm_rsh_search(const char* agent_list, const char *path)
{
    int i, j;
    char *line, **lines;
    char **tokens, *tmp;
    char cwd[OPAL_PATH_MAX];
    
    if (NULL == path) {
        getcwd(cwd, OPAL_PATH_MAX);
    } else {
        strncpy(cwd, path, OPAL_PATH_MAX);
    }
    if (NULL == agent_list) {
        lines = opal_argv_split(mca_plm_rsh_component.agent, ':');
    } else {
        lines = opal_argv_split(agent_list, ':');
    }
    for (i = 0; NULL != lines[i]; ++i) {
        line = lines[i];
        
        /* Trim whitespace at the beginning and end of the line */
        for (j = 0; '\0' != line[j] && isspace(line[j]); ++line) {
            continue;
        }
        for (j = strlen(line) - 2; j > 0 && isspace(line[j]); ++j) {
            line[j] = '\0';
        }
        if (strlen(line) <= 0) {
            continue;
        }
        
        /* Split it */
        tokens = opal_argv_split(line, ' ');
        
        /* Look for the first token in the PATH */
        tmp = opal_path_findv(tokens[0], X_OK, environ, cwd);
        if (NULL != tmp) {
            free(tokens[0]);
            tokens[0] = tmp;
            opal_argv_free(lines);
            return tokens;
        }
        
        /* Didn't find it */
        opal_argv_free(tokens);
    }
    
    /* Doh -- didn't find anything */
    opal_argv_free(lines);
    return NULL;
}

static int rsh_launch_agent_lookup(const char *agent_list, char *path)
{
    char **tmp;

    OPAL_OUTPUT_VERBOSE((5, orte_plm_globals.output,
                         "%s plm:rsh_lookup on agent %s path %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (NULL == agent_list) ? mca_plm_rsh_component.agent : agent_list,
                         (NULL == path) ? "NULL" : path));
    if (NULL == (tmp = orte_plm_rsh_search(agent_list, path))) {
        return ORTE_ERR_NOT_FOUND;
    }

    /* if we got here, then one of the given agents could be found */
    opal_argv_free(tmp);
    return ORTE_SUCCESS;
}

