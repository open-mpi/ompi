/*
 * $HEADER$
 */

/**
 * @file
 *
 * Parse environmental paramater options for the Open MPI Run Time Environment. This function
 * MUST be called BEFORE calling any of the rte command line parsers.
 *
 */
#include "ompi_config.h"

#include <string.h>

#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"

#include "runtime/runtime.h"

void ompi_rte_parse_environ(void)
{
    char *enviro_val;

    enviro_val = getenv("OMPI_universe_seed");
    if (NULL != enviro_val) {  /* seed flag passed */
	ompi_process_info.seed = true;
    } else {
	ompi_process_info.seed = false;
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
	}
	ompi_universe_info.scope = strdup(enviro_val);
    } else {
	if (NULL != ompi_universe_info.scope) {
	    free(ompi_universe_info.scope);
	}
    }

    enviro_val = getenv("OMPI_universe_persistent");
    if (NULL != enviro_val) {  /* persistence flag passed */
	ompi_universe_info.persistence = true;
    } else {
	ompi_universe_info.persistence = false;
    }

    enviro_val = getenv("OMPI_universe_silent");
    if (NULL != enviro_val) {  /* silent flag passed */
	ompi_universe_info.silent_mode = true;
    } else {
	ompi_universe_info.silent_mode = false;
    }

    enviro_val = getenv("OMPI_universe_webserver");
    if (NULL != enviro_val) {  /* webserver flag passed */
	ompi_universe_info.web_server = true;
    } else {
	ompi_universe_info.web_server = false;
    }

    enviro_val = getenv("OMPI_universe_script");
    if (NULL != enviro_val) { /* scriptfile passed */
	if (NULL != ompi_universe_info.scriptfile) {  /* overwrite */
	    free(ompi_universe_info.scriptfile);
	}
	ompi_universe_info.scriptfile = strdup(enviro_val);
    } else {
	if (NULL != ompi_universe_info.scriptfile) {
	    free(ompi_universe_info.scriptfile);
	}
    }

    enviro_val = getenv("OMPI_universe_hostfile");
    if (NULL != enviro_val) { /* hostfile passed */
	if (NULL != ompi_universe_info.hostfile) {  /* overwrite */
	    free(ompi_universe_info.hostfile);
	}
	ompi_universe_info.hostfile = strdup(enviro_val);
    } else {
	if (NULL != ompi_universe_info.hostfile) {
	    free(ompi_universe_info.hostfile);
	}
    }

    enviro_val = getenv("OMPI_universe_name");
    if (NULL != enviro_val) {  /* universe name passed in environment */
	if (NULL != ompi_universe_info.name) {  /* got something in it - overwrite */
	    free(ompi_universe_info.name);
	}
	ompi_universe_info.name = strdup(enviro_val);
	if (NULL != ompi_process_info.my_universe) {
	    free(ompi_process_info.my_universe);
	}
	ompi_process_info.my_universe = strdup(enviro_val);
    } else {
	if (NULL != ompi_universe_info.name) {
	    free(ompi_universe_info.name);
	}
	ompi_universe_info.name = strdup("default-universe");
	if (NULL != ompi_process_info.my_universe) {
	    free(ompi_process_info.my_universe);
	}
	ompi_process_info.my_universe = strdup("default-universe");
    }

    enviro_val = getenv("OMPI_tmpdir_base");
    if (NULL != enviro_val) {  /* tmpdir base passed in environment */
	if (NULL != ompi_process_info.tmpdir_base) {  /* overwrite it */
	    free(ompi_process_info.tmpdir_base);
	}
	ompi_process_info.tmpdir_base = strdup(enviro_val);
    } else {
	if (NULL != ompi_process_info.tmpdir_base) {
	    free(ompi_process_info.tmpdir_base);
	}
    }
}
