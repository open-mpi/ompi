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
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/util/basename.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/rsh/plm_rsh.h"

/*
 * Local function
 */
static char **search(const char* agent_list);


/*
 * Public string showing the plm ompi_rsh component version number
 */
const char *mca_plm_rsh_component_version_string =
  "Open MPI rsh plm MCA component version " ORTE_VERSION;


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
        orte_plm_rsh_component_open,
        orte_plm_rsh_component_close,
        orte_plm_rsh_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
    }
};



int orte_plm_rsh_component_open(void)
{
    int tmp;
    mca_base_component_t *c = &mca_plm_rsh_component.super.base_version;

    /* initialize globals */
    OBJ_CONSTRUCT(&mca_plm_rsh_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_plm_rsh_component.cond, opal_condition_t);
    mca_plm_rsh_component.num_children = 0;
    mca_plm_rsh_component.agent_argv = NULL;
    mca_plm_rsh_component.agent_argc = 0;
    mca_plm_rsh_component.agent_path = NULL;
    OBJ_CONSTRUCT(&mca_plm_rsh_component.children, opal_list_t);

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
                           "Disable the launcher to use qrsh when under the SGE parallel environment",
                           false, false, false, &tmp);
    mca_plm_rsh_component.disable_qrsh = OPAL_INT_TO_BOOL(tmp);  

    mca_base_param_reg_int(c, "daemonize_qrsh",
                           "Daemonize the orted under the SGE parallel environment",
                           false, false, false, &tmp);
    mca_plm_rsh_component.daemonize_qrsh = OPAL_INT_TO_BOOL(tmp);
    
    mca_base_param_reg_int(c, "priority",
                           "Priority of the rsh plm component",
                           false, false, 10,
                           &mca_plm_rsh_component.priority);
    mca_base_param_reg_int(c, "delay",
                           "Delay (in seconds) between invocations of the remote agent, but only used when the \"debug\" MCA parameter is true, or the top-level MCA debugging is enabled (otherwise this value is ignored)",
                           false, false, 1,
                           &mca_plm_rsh_component.delay);
    mca_base_param_reg_int(c, "assume_same_shell",
                           "If set to 1, assume that the shell on the remote node is the same as the shell on the local node.  Otherwise, probe for what the remote shell.",
                           false, false, 1, &tmp);
    mca_plm_rsh_component.assume_same_shell = OPAL_INT_TO_BOOL(tmp);

    tmp = mca_base_param_reg_string(c, "agent",
                              "The command used to launch executables on remote nodes (typically either \"ssh\" or \"rsh\")",
                              false, false, "ssh : rsh", NULL);
    mca_base_param_reg_syn_name(tmp, "pls", "rsh_agent", true);
    mca_base_param_lookup_string(tmp, &mca_plm_rsh_component.agent_param);
    
    mca_base_param_reg_int(c, "tree_spawn",
                           "If set to 1, launch via a tree-based topology",
                           false, false, (int)false, &tmp);
    mca_plm_rsh_component.tree_spawn = OPAL_INT_TO_BOOL(tmp);
    
    return ORTE_SUCCESS;
}


int orte_plm_rsh_component_query(mca_base_module_t **module, int *priority)
{
    char *bname;
    size_t i;

    /* Take the string that was given to us by the plm_rsh_agent MCA
       param and search for it */
    mca_plm_rsh_component.agent_argv = 
        search(mca_plm_rsh_component.agent_param);
    mca_plm_rsh_component.agent_argc = 
        opal_argv_count(mca_plm_rsh_component.agent_argv);
    mca_plm_rsh_component.agent_path = NULL;


    /* To be absolutely sure that we are under an SGE parallel env */
    if (!mca_plm_rsh_component.disable_qrsh &&
        NULL != getenv("SGE_ROOT") && NULL != getenv("ARC") &&
        NULL != getenv("PE_HOSTFILE") && NULL != getenv("JOB_ID")) {
        /* setting exec_argv and exec_path for qrsh */
        asprintf(&mca_plm_rsh_component.agent_param, "qrsh");
        asprintf(&mca_plm_rsh_component.agent_path, "%s/bin/%s", getenv("SGE_ROOT"), getenv("ARC"));
        asprintf(&mca_plm_rsh_component.agent_argv[0], "%s/bin/%s/qrsh", getenv("SGE_ROOT"), getenv("ARC"));
        if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
            opal_output_verbose(1, orte_plm_globals.output,
               "%s plm:rsh: using %s for launching\n",
               ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
               mca_plm_rsh_component.agent_argv[0]);
        }
    }

    if (mca_plm_rsh_component.agent_argc > 0) {
        /* If the agent is ssh, and debug was not selected, then
           automatically add "-x" */

        bname = opal_basename(mca_plm_rsh_component.agent_argv[0]);
        if (NULL != bname && 0 == strcmp(bname, "ssh")) {
            /* if xterm option was given, add '-X', ensuring we don't do it twice */
            if (NULL != orte_xterm) {
                opal_argv_append(&mca_plm_rsh_component.agent_argc, 
                                 &mca_plm_rsh_component.agent_argv, "-X");
            } else if (0 >= opal_output_get_verbosity(orte_plm_globals.output)) {
                /* if debug was not specified, and the user didn't explicitly
                 * specify X11 forwarding/non-forwarding, add "-x" if it
                 * isn't already there (check either case)
                 */
                for (i = 1; NULL != mca_plm_rsh_component.agent_argv[i]; ++i) {
                    if (0 == strcasecmp("-x", 
                                        mca_plm_rsh_component.agent_argv[i])) {
                        break;
                    }
                }
                if (NULL == mca_plm_rsh_component.agent_argv[i]) {
                    opal_argv_append(&mca_plm_rsh_component.agent_argc, 
                                     &mca_plm_rsh_component.agent_argv, "-x");
                }
            }
        }

        /* If the agent is qrsh, then automatically add -inherit 
         * and grid engine PE related flags */
        if (NULL != bname && 0 == strcmp(bname, "qrsh")) {
            opal_argv_append(&mca_plm_rsh_component.agent_argc, 
                             &mca_plm_rsh_component.agent_argv, "-inherit");
            /* Don't use the "-noshell" flag as qrsh would have a problem 
             * swallowing a long command */
            opal_argv_append(&mca_plm_rsh_component.agent_argc, 
                             &mca_plm_rsh_component.agent_argv, "-nostdin");
            opal_argv_append(&mca_plm_rsh_component.agent_argc, 
                             &mca_plm_rsh_component.agent_argv, "-V");
            if (0 < opal_output_get_verbosity(orte_plm_globals.output)) {
                opal_argv_append(&mca_plm_rsh_component.agent_argc, 
                                 &mca_plm_rsh_component.agent_argv, "-verbose");
            }
        }
        if (NULL != bname) {
            free(bname);
        }
    }

    /* If we didn't find the agent in the path, then don't use this
       component */
    if (NULL == mca_plm_rsh_component.agent_argv || 
        NULL == mca_plm_rsh_component.agent_argv[0]) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: unable to be used: cannot find the "
                             "launching agent. Looked for: %s\n", 
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             mca_plm_rsh_component.agent_param));
        *module = NULL;
        return ORTE_ERROR;
    }
    mca_plm_rsh_component.agent_path = 
        opal_path_findv(mca_plm_rsh_component.agent_argv[0], X_OK,
                        environ, NULL);
    if (NULL == mca_plm_rsh_component.agent_path) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: unable to be used: cannot find path "
                             "for launching agent \"%s\"\n", 
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             mca_plm_rsh_component.agent_argv[0]));
        *module = NULL;
        return ORTE_ERROR;
    }
    *priority = mca_plm_rsh_component.priority;
    *module = (mca_base_module_t *) &orte_plm_rsh_module;
    return ORTE_SUCCESS;
}


int orte_plm_rsh_component_close(void)
{
    /* cleanup state */
    OBJ_DESTRUCT(&mca_plm_rsh_component.lock);
    OBJ_DESTRUCT(&mca_plm_rsh_component.cond);
    OBJ_DESTRUCT(&mca_plm_rsh_component.children);
    if (NULL != mca_plm_rsh_component.agent_param) {
        free(mca_plm_rsh_component.agent_param);
    }
    if (NULL != mca_plm_rsh_component.agent_argv) {
        opal_argv_free(mca_plm_rsh_component.agent_argv);
    }
    if (NULL != mca_plm_rsh_component.agent_path) {
        free(mca_plm_rsh_component.agent_path);
    }
    return ORTE_SUCCESS;
}


/*
 * Take a colon-delimited list of agents and locate the first one that
 * we are able to find in the PATH.  Split that one into argv and
 * return it.  If nothing found, then return NULL.
 */
static char **search(const char* agent_list)
{
    int i, j;
    char *line, **lines = opal_argv_split(agent_list, ':');
    char **tokens, *tmp;
    char cwd[OMPI_PATH_MAX];

    getcwd(cwd, OMPI_PATH_MAX);
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
