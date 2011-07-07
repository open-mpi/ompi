/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_PLM_RSH_SUPPORT_H
#define MCA_PLM_RSH_SUPPORT_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/types.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/threads/condition.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/runtime/orte_globals.h"


BEGIN_C_DECLS

typedef enum {
    ORTE_PLM_RSH_SHELL_BASH = 0,
    ORTE_PLM_RSH_SHELL_ZSH,
    ORTE_PLM_RSH_SHELL_TCSH,
    ORTE_PLM_RSH_SHELL_CSH,
    ORTE_PLM_RSH_SHELL_KSH,
    ORTE_PLM_RSH_SHELL_SH,
    ORTE_PLM_RSH_SHELL_UNKNOWN
} orte_plm_rsh_shell_t;

ORTE_DECLSPEC extern const char *orte_plm_rsh_shell_name[7];

/* rsh launch support */
ORTE_DECLSPEC int orte_plm_base_rsh_launch_agent_setup(const char *agent_list, char *path);
ORTE_DECLSPEC int orte_plm_base_rsh_launch_agent_lookup(const char *agent_list, char *path);
ORTE_DECLSPEC int orte_plm_base_rsh_shell_probe(char *nodename, orte_plm_rsh_shell_t *shell);
ORTE_DECLSPEC int orte_plm_base_rsh_setup_shell(orte_plm_rsh_shell_t *rshell,
                                                orte_plm_rsh_shell_t *lshell,
                                                char *nodename, char ***argv);
ORTE_DECLSPEC int orte_plm_base_rsh_setup_launch(int *argcptr, char ***argvptr,
                                                 char *nodename,
                                                 int *node_name_index1,
                                                 int *proc_vpid_index, char *prefix_dir,
                                                 char *nodes);
ORTE_DECLSPEC void orte_plm_base_ssh_child(int argc, char **argv,
                                           orte_vpid_t vpid, int proc_vpid_index);

/**
 * Local slave launch
 */
ORTE_DECLSPEC int orte_plm_base_local_slave_launch(orte_job_t *jdata);
ORTE_DECLSPEC void orte_plm_base_local_slave_finalize(void);
ORTE_DECLSPEC int orte_plm_base_setup_slave_launch(char *nodename, orte_app_context_t *app,
                                                   char *rcmd, char ***argv, char **exec_path);
ORTE_DECLSPEC int orte_plm_base_append_bootproxy_args(orte_app_context_t *app, char ***argv,
                                                      orte_jobid_t jobid, orte_vpid_t vpid,
                                                      int num_nodes, orte_vpid_t num_procs,
                                                      orte_node_rank_t nrank, orte_local_rank_t lrank,
                                                      orte_vpid_t nlocal, int nslots, bool overwrite);

END_C_DECLS

#endif  /* MCA_PLM_RSH_SUPPORT_H */
