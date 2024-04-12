/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights
 *                         reserved.
 * Copyright (c) 2009-2021 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2020 IBM Corporation.  All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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

#include "prte_config.h"
#include "constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#include <ctype.h>

#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_string_copy.h"

#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/plm/base/plm_private.h"
#include "src/mca/plm/plm.h"
#include "src/mca/plm/ssh/plm_ssh.h"

/*
 * Public string showing the plm ompi_ssh component version number
 */
const char *prte_mca_plm_ssh_component_version_string
    = "PRTE ssh plm MCA component version " PRTE_VERSION;

static int ssh_component_register(void);
static int ssh_component_open(void);
static int ssh_component_query(pmix_mca_base_module_t **module, int *priority);
static int ssh_component_close(void);
static int ssh_launch_agent_lookup(const char *agent_list, char *path);

/* Local variables */
static char *prte_plm_ssh_delay_string = NULL;
static int agent_var_id = -1;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

prte_mca_plm_ssh_component_t prte_mca_plm_ssh_component = {
    .super = {
        PRTE_PLM_BASE_VERSION_2_0_0,

        /* Component name and version */
        .pmix_mca_component_name = "ssh",
        PMIX_MCA_BASE_MAKE_VERSION(component,
                                   PRTE_MAJOR_VERSION,
                                   PRTE_MINOR_VERSION,
                                   PMIX_RELEASE_VERSION),

        /* Component open and close functions */
        .pmix_mca_open_component = ssh_component_open,
        .pmix_mca_close_component = ssh_component_close,
        .pmix_mca_query_component = ssh_component_query,
        .pmix_mca_register_component_params = ssh_component_register,
    }
};

static int ssh_component_register(void)
{
    pmix_mca_base_component_t *c = &prte_mca_plm_ssh_component.super;
    int var_id;

    prte_mca_plm_ssh_component.num_concurrent = 128;
    (void) pmix_mca_base_component_var_register(c, "num_concurrent",
                                                "How many plm_ssh_agent instances to invoke concurrently (must be > 0)",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_plm_ssh_component.num_concurrent);

    prte_mca_plm_ssh_component.force_ssh = false;
    (void) pmix_mca_base_component_var_register(c, "force_ssh",
                                                "Force the launcher to always use ssh",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_plm_ssh_component.force_ssh);

    prte_mca_plm_ssh_component.disable_qrsh = false;
    (void) pmix_mca_base_component_var_register(c, "disable_qrsh",
                                                "Disable the use of qrsh when under the Grid Engine parallel environment",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_plm_ssh_component.disable_qrsh);

    prte_mca_plm_ssh_component.daemonize_qrsh = false;
    (void) pmix_mca_base_component_var_register(c, "daemonize_qrsh",
                                                "Daemonize the orted under the Grid Engine parallel environment",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_plm_ssh_component.daemonize_qrsh);

    prte_mca_plm_ssh_component.disable_llspawn = false;
    (void) pmix_mca_base_component_var_register(c, "disable_llspawn",
                                                "Disable the use of llspawn when under the LoadLeveler environment",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_plm_ssh_component.disable_llspawn);

    prte_mca_plm_ssh_component.daemonize_llspawn = false;
    (void) pmix_mca_base_component_var_register(c, "daemonize_llspawn",
                                                "Daemonize the orted when under the LoadLeveler environment",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_plm_ssh_component.daemonize_llspawn);

    prte_mca_plm_ssh_component.priority = 10;
    (void) pmix_mca_base_component_var_register(c, "priority", "Priority of the ssh plm component",
                                                PMIX_MCA_BASE_VAR_TYPE_INT,
                                                &prte_mca_plm_ssh_component.priority);

    prte_plm_ssh_delay_string = NULL;
    (void) pmix_mca_base_component_var_register(c, "delay",
                                                "Delay between invocations of the remote agent (sec[:usec])",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_plm_ssh_delay_string);

    prte_mca_plm_ssh_component.no_tree_spawn = false;
    (void) pmix_mca_base_component_var_register(c, "no_tree_spawn",
                                                "If set to true, do not launch via a tree-based topology",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_plm_ssh_component.no_tree_spawn);

    /* local ssh/ssh launch agent */
    prte_mca_plm_ssh_component.agent = "ssh : rsh";
    var_id = pmix_mca_base_component_var_register(c, "agent",
                                                  "The command used to launch executables on remote nodes (typically \"ssh\")",
                                                  PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                  &prte_mca_plm_ssh_component.agent);
    (void) pmix_mca_base_var_register_synonym(var_id, "prte", "pls", NULL, "ssh_agent",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    (void) pmix_mca_base_var_register_synonym(var_id, "prte", "prte", NULL, "ssh_agent",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    agent_var_id = var_id;

    prte_mca_plm_ssh_component.assume_same_shell = true;
    var_id = pmix_mca_base_component_var_register(c, "assume_same_shell",
                                                  "If set to true, assume that the shell on the remote node is the same as the shell on the "
                                                  "local node.  Otherwise, probe for what the remote shell [default: 1]",
                                                  PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                  &prte_mca_plm_ssh_component.assume_same_shell);
    /* XXX -- var_conversion -- Why does this component register prte_assume_same_shell? Components
     * should ONLY register THEIR OWN variables. */
    (void) pmix_mca_base_var_register_synonym(var_id, "prte", "prte", NULL, "assume_same_shell",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    prte_mca_plm_ssh_component.pass_environ_mca_params = true;
    (void) pmix_mca_base_component_var_register(c, "pass_environ_mca_params",
                                                "If set to false, do not include mca params from the environment on the orted cmd line",
                                                PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                                &prte_mca_plm_ssh_component.pass_environ_mca_params);

    prte_mca_plm_ssh_component.ssh_args = NULL;
    (void) pmix_mca_base_component_var_register(c, "args", "Arguments to add to ssh",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_mca_plm_ssh_component.ssh_args);

    prte_mca_plm_ssh_component.pass_libpath = NULL;
    (void) pmix_mca_base_component_var_register(c, "pass_libpath",
                                                "Prepend the specified library path to the remote shell's LD_LIBRARY_PATH",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_mca_plm_ssh_component.pass_libpath);

    prte_mca_plm_ssh_component.chdir = NULL;
    (void) pmix_mca_base_component_var_register(c, "chdir",
                                                "Change working directory after ssh, but before exec of prted",
                                                PMIX_MCA_BASE_VAR_TYPE_STRING,
                                                &prte_mca_plm_ssh_component.chdir);
    return PRTE_SUCCESS;
}

static int ssh_component_open(void)
{
    char *ctmp;

    /* initialize globals */
    prte_mca_plm_ssh_component.using_qrsh = false;
    prte_mca_plm_ssh_component.using_llspawn = false;
    prte_mca_plm_ssh_component.agent_argv = NULL;

    /* lookup parameters */
    if (prte_mca_plm_ssh_component.num_concurrent <= 0) {
        pmix_show_help("help-plm-ssh.txt", "concurrency-less-than-zero", true,
                       prte_mca_plm_ssh_component.num_concurrent);
        prte_mca_plm_ssh_component.num_concurrent = 1;
    }

    if (NULL != prte_plm_ssh_delay_string) {
        prte_mca_plm_ssh_component.delay.tv_sec = strtol(prte_plm_ssh_delay_string, &ctmp, 10);
        if (ctmp == prte_plm_ssh_delay_string) {
            prte_mca_plm_ssh_component.delay.tv_sec = 0;
        }
        if (':' == ctmp[0]) {
            prte_mca_plm_ssh_component.delay.tv_nsec = 1000 * strtol(ctmp + 1, NULL, 10);
        }
    }

    return PRTE_SUCCESS;
}

static int ssh_component_query(pmix_mca_base_module_t **module, int *priority)
{
    char *tmp;

    /* Check if we are under Grid Engine parallel environment by looking at several
     * environment variables.  If so, setup the path and argv[0].
     * Note that we allow the user to specify the launch agent
     * even if they are in a Grid Engine environment */
    int ret;
    pmix_mca_base_var_source_t source;
    ret = pmix_mca_base_var_get_value(agent_var_id, NULL, &source, NULL);
    if (PRTE_SUCCESS != ret) {
        return ret;
    }
    if (PMIX_MCA_BASE_VAR_SOURCE_DEFAULT != source) {
        /* if the user specified a launch agent, then
         * respect that request */
        goto lookup;
    }

    /* check for SGE */
    if (!prte_mca_plm_ssh_component.disable_qrsh && NULL != getenv("SGE_ROOT") && NULL != getenv("ARC")
        && NULL != getenv("PE_HOSTFILE") && NULL != getenv("JOB_ID")) {
        /* setup the search path for qrsh */
        pmix_asprintf(&tmp, "%s/bin/%s", getenv("SGE_ROOT"), getenv("ARC"));
        /* see if the agent is available */
        if (PRTE_SUCCESS != ssh_launch_agent_lookup("qrsh", tmp)) {
            /* can't be SGE */
            pmix_output_verbose(1, prte_plm_base_framework.framework_output,
                                "%s plm:ssh: unable to be used: SGE indicated but cannot find path "
                                "or execution permissions not set for launching agent qrsh",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            free(tmp);
            *module = NULL;
            return PRTE_ERROR;
        }
        prte_mca_plm_ssh_component.agent = tmp;
        prte_mca_plm_ssh_component.using_qrsh = true;
        goto success;
    }

    /* otherwise, check for LoadLeveler */
    if (!prte_mca_plm_ssh_component.disable_llspawn && NULL != getenv("LOADL_STEP_ID")) {
        /* Search for llspawn in the users PATH */
        if (PRTE_SUCCESS != ssh_launch_agent_lookup("llspawn", NULL)) {
            pmix_output_verbose(1, prte_plm_base_framework.framework_output,
                                "%s plm:ssh: unable to be used: LoadLeveler "
                                "indicated but cannot find path or execution "
                                "permissions not set for launching agent llspawn",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            *module = NULL;
            return PRTE_ERROR;
        }
        prte_mca_plm_ssh_component.agent = strdup("llspawn");
        prte_mca_plm_ssh_component.using_llspawn = true;
        goto success;
    }

    /* if this isn't an Grid Engine or LoadLeveler environment, or
     * if the user specified a launch agent, look for it */
lookup:
    if (PRTE_SUCCESS != ssh_launch_agent_lookup(NULL, NULL)) {
        /* if the user specified an agent and we couldn't find it,
         * then we want to error out and not continue */
        if (NULL != prte_mca_plm_ssh_component.agent &&
            0 != strcmp(prte_mca_plm_ssh_component.agent, "ssh : rsh")) {
            pmix_show_help("help-plm-ssh.txt", "agent-not-found", true,
                           prte_mca_plm_ssh_component.agent);
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_NEVER_LAUNCHED);
            return PRTE_ERR_FATAL;
        }
        /* this isn't an error - we just cannot be selected */
        PMIX_OUTPUT_VERBOSE((1, prte_plm_base_framework.framework_output,
                             "%s plm:ssh: unable to be used: cannot find path "
                             "for launching agent \"%s\"\n",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), prte_mca_plm_ssh_component.agent));
        *module = NULL;
        return PRTE_ERROR;
    }

success:
    /* we are good - make ourselves available */
    *priority = prte_mca_plm_ssh_component.priority;
    *module = (pmix_mca_base_module_t *) &prte_plm_ssh_module;
    return PRTE_SUCCESS;
}

static int ssh_component_close(void)
{
    return PRTE_SUCCESS;
}

/*
 * Take a colon-delimited list of agents and locate the first one that
 * we are able to find in the PATH.  Split that one into argv and
 * return it.  If nothing found, then return NULL.
 */
char **prte_plm_ssh_search(const char *agent_list, const char *path)
{
    int i, j;
    char *line, **lines;
    char **tokens, *tmp;
    char cwd[PRTE_PATH_MAX];

    if (NULL == agent_list && NULL == prte_mca_plm_ssh_component.agent) {
        return NULL;
    }

    if (NULL == path) {
        getcwd(cwd, PRTE_PATH_MAX);
    } else {
        pmix_string_copy(cwd, path, PRTE_PATH_MAX);
    }
    if (NULL == agent_list) {
        lines = PMIX_ARGV_SPLIT_COMPAT(prte_mca_plm_ssh_component.agent, ':');
    } else {
        lines = PMIX_ARGV_SPLIT_COMPAT(agent_list, ':');
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
        tokens = PMIX_ARGV_SPLIT_COMPAT(line, ' ');

        /* Look for the first token in the PATH */
        tmp = pmix_path_findv(tokens[0], X_OK, environ, cwd);
        if (NULL != tmp) {
            free(tokens[0]);
            tokens[0] = tmp;
            PMIX_ARGV_FREE_COMPAT(lines);
            return tokens;
        }

        /* Didn't find it */
        PMIX_ARGV_FREE_COMPAT(tokens);
    }

    /* Doh -- didn't find anything */
    PMIX_ARGV_FREE_COMPAT(lines);
    return NULL;
}

static int ssh_launch_agent_lookup(const char *agent_list, char *path)
{
    char *bname;
    int i;

    if (NULL == agent_list && NULL == prte_mca_plm_ssh_component.agent) {
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:ssh_lookup on agent (null) path %s - No agent specified.",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), (NULL == path) ? "NULL" : path));
        return PRTE_ERR_NOT_FOUND;
    }

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:ssh_lookup on agent %s path %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         (NULL == agent_list) ? prte_mca_plm_ssh_component.agent : agent_list,
                         (NULL == path) ? "NULL" : path));
    if (NULL == (prte_mca_plm_ssh_component.agent_argv = prte_plm_ssh_search(agent_list, path))) {
        return PRTE_ERR_NOT_FOUND;
    }

    /* if we got here, then one of the given agents could be found - the
     * complete path is in the argv[0] position */
    prte_mca_plm_ssh_component.agent_path = strdup(prte_mca_plm_ssh_component.agent_argv[0]);
    bname = pmix_basename(prte_mca_plm_ssh_component.agent_argv[0]);
    if (NULL == bname) {
        return PRTE_SUCCESS;
    }
    /* replace the initial position with the basename */
    free(prte_mca_plm_ssh_component.agent_argv[0]);
    prte_mca_plm_ssh_component.agent_argv[0] = bname;
    /* see if we need to add an xterm argument */
    if (0 == strcmp(bname, "ssh")) {
        /* if xterm option was given, add '-X', ensuring we don't do it twice */
        if (NULL != prte_xterm) {
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&prte_mca_plm_ssh_component.agent_argv, "-X");
        } else if (0 >= pmix_output_get_verbosity(prte_plm_base_framework.framework_output)) {
            /* if debug was not specified, and the user didn't explicitly
             * specify X11 forwarding/non-forwarding, add "-x" if it
             * isn't already there (check either case)
             */
            for (i = 1; NULL != prte_mca_plm_ssh_component.agent_argv[i]; ++i) {
                if (0 == strcasecmp("-x", prte_mca_plm_ssh_component.agent_argv[i])) {
                    break;
                }
            }
            if (NULL == prte_mca_plm_ssh_component.agent_argv[i]) {
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_mca_plm_ssh_component.agent_argv, "-x");
            }
        }
    }
    return PRTE_SUCCESS;
}
