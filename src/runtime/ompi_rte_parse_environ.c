/*
 * $HEADER$
 */

/**
 * @file
 *
 * Parse environmental paramater options for the Open MPI Run Time Environment. This function
 * MUST be called BEFORE calling any of the rte command line parsers, AFTER calling
 * rte_init_stage1, and BEFORE calling rte_init_stage2.
 *
 * NOTE: Sets all key structure values to defaults if no environ value provided!!
 *
 */
#include "ompi_config.h"

#include <string.h>

#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"

#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"

#include "runtime/runtime.h"

void ompi_rte_parse_environ(void)
{
    char *enviro_val, *tmp;
    int id;

    /* ensure that sys_info and proc_info have been run */
    ompi_sys_info();
    ompi_proc_info();

    enviro_val = getenv("OMPI_universe_seed");
    if (NULL != enviro_val) {  /* seed flag passed */
	ompi_process_info.seed = true;
    } else {
	ompi_process_info.seed = false;
    }

    enviro_val = getenv("OMPI_universe_contact");
    if (NULL != enviro_val) { /* contact info passed */
	if (NULL != ompi_universe_info.seed_contact_info) {  /* overwrite */
	    free(ompi_universe_info.seed_contact_info);
	    ompi_universe_info.seed_contact_info = NULL;
	}
	ompi_universe_info.seed_contact_info = strdup(enviro_val);
	mca_oob_set_contact_info(ompi_universe_info.seed_contact_info);
    } else {
	if (NULL != ompi_universe_info.seed_contact_info) {
	    free(ompi_universe_info.seed_contact_info);
	    ompi_universe_info.seed_contact_info = NULL;
	}
    }

    id = mca_base_param_register_string("gpr", "base", "replica", NULL, NULL);
    mca_base_param_lookup_string(id, &ompi_universe_info.gpr_replica);
    if (NULL != ompi_universe_info.gpr_replica) {
	mca_oob_set_contact_info(ompi_universe_info.gpr_replica);
	ompi_process_info.gpr_replica = ns_base_create_process_name(0,0,0);
	mca_oob_parse_contact_info(ompi_universe_info.gpr_replica,
				   ompi_process_info.gpr_replica, NULL);
    } else {
	if (NULL != ompi_process_info.gpr_replica) {
	    free(ompi_process_info.gpr_replica);
	    ompi_process_info.gpr_replica = NULL;
	}
    }

    id = mca_base_param_register_string("ns", "base", "replica", NULL, NULL);
    mca_base_param_lookup_string(id, &ompi_universe_info.ns_replica);
    if (NULL != ompi_universe_info.ns_replica) {
	mca_oob_set_contact_info(ompi_universe_info.ns_replica);
	ompi_process_info.ns_replica = ns_base_create_process_name(0,0,0);
	mca_oob_parse_contact_info(ompi_universe_info.ns_replica,
				   ompi_process_info.ns_replica, NULL);
    } else {
	if (NULL != ompi_process_info.ns_replica) {
	    free(ompi_process_info.ns_replica);
	    ompi_process_info.ns_replica = NULL;
	}
    }

    enviro_val = getenv("OMPI_universe_probe");
    if (NULL != enviro_val) {  /* probe flag passed */
	ompi_universe_info.probe = true;
    } else {
	ompi_universe_info.probe = false;
    }

    enviro_val = getenv("OMPI_universe_scope");
    if (NULL != enviro_val) { /* scope passed */
	if (NULL != ompi_universe_info.scope) {  /* overwrite */
	    free(ompi_universe_info.scope);
	    ompi_universe_info.scope = NULL;
	}
	ompi_universe_info.scope = strdup(enviro_val);
    } else {
	if (NULL != ompi_universe_info.scope) {
	    free(ompi_universe_info.scope);
	    ompi_universe_info.scope = NULL;
	}
	ompi_universe_info.scope = strdup("exclusive");
    }

    enviro_val = getenv("OMPI_universe_persistent");
    if (NULL != enviro_val) {  /* persistence flag passed */
	ompi_universe_info.persistence = true;
    } else {
	ompi_universe_info.persistence = false;
    }

    enviro_val = getenv("OMPI_universe_console");
    if (NULL != enviro_val) {  /* console flag passed */
	ompi_universe_info.console = true;
    } else {
	ompi_universe_info.console = false;
    }

    enviro_val = getenv("OMPI_universe_script");
    if (NULL != enviro_val) { /* scriptfile passed */
	if (NULL != ompi_universe_info.scriptfile) {  /* overwrite */
	    free(ompi_universe_info.scriptfile);
	    ompi_universe_info.scriptfile = NULL;
	}
	ompi_universe_info.scriptfile = strdup(enviro_val);
    } else {
	if (NULL != ompi_universe_info.scriptfile) {
	    free(ompi_universe_info.scriptfile);
	    ompi_universe_info.scriptfile = NULL;
	}
    }

    enviro_val = getenv("OMPI_universe_hostfile");
    if (NULL != enviro_val) { /* hostfile passed */
	if (NULL != ompi_universe_info.hostfile) {  /* overwrite */
	    free(ompi_universe_info.hostfile);
	    ompi_universe_info.hostfile = NULL;
	}
	ompi_universe_info.hostfile = strdup(enviro_val);
    } else {
	if (NULL != ompi_universe_info.hostfile) {
	    free(ompi_universe_info.hostfile);
	    ompi_universe_info.hostfile = NULL;
	}
    }

    if (NULL != ompi_universe_info.name) {
	free(ompi_universe_info.name);
	ompi_universe_info.name = NULL;
    }
    ompi_universe_info.name = strdup("default-universe");
    if (NULL != ompi_process_info.my_universe) {
	free(ompi_process_info.my_universe);
	ompi_process_info.my_universe = NULL;
    }
    ompi_process_info.my_universe = strdup("default-universe");
    if (NULL != ompi_universe_info.host) {
	free(ompi_universe_info.host);
	ompi_universe_info.host = NULL;
    }
    ompi_universe_info.host = strdup(ompi_system_info.nodename);
    if (NULL != ompi_universe_info.uid) {
	free(ompi_universe_info.uid);
	ompi_universe_info.uid = NULL;
    }
    ompi_universe_info.uid = strdup(ompi_system_info.user);

    enviro_val = getenv("OMPI_universe_name");
    if (NULL != enviro_val) {  /* universe name passed in environment */
	if (NULL != ompi_universe_info.name) {  /* got something in it - overwrite */
	    free(ompi_universe_info.name);
	    ompi_universe_info.name = NULL;
	}
	ompi_universe_info.name = strdup(enviro_val);
	if (NULL != ompi_process_info.my_universe) {
	    free(ompi_process_info.my_universe);
	    ompi_process_info.my_universe = NULL;
	}
	ompi_process_info.my_universe = strdup(enviro_val);
    }

    enviro_val = getenv("OMPI_tmpdir_base");
    if (NULL != enviro_val) {  /* tmpdir base passed in environment */
	if (NULL != ompi_process_info.tmpdir_base) {  /* overwrite it */
	    free(ompi_process_info.tmpdir_base);
	    ompi_process_info.tmpdir_base = NULL;
	}
	ompi_process_info.tmpdir_base = strdup(enviro_val);
    } else {
	if (NULL != ompi_process_info.tmpdir_base) {
	    free(ompi_process_info.tmpdir_base);
	    ompi_process_info.tmpdir_base = NULL;
	}
    }

    ompi_universe_info.pid = ompi_process_info.pid;

}
