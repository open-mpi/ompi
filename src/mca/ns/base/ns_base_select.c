/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <types.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/ns/base/base.h"
#include "mca/ns/replica/src/ns_replica.h"


/**
 * Function for selecting one module from all those that are
 * available.
 *
 * Decide whether or not to be a replica, then call appropriate
 * component to init module.
 */
int mca_ns_base_select(bool *allow_multi_user_threads, 
                        bool *have_hidden_threads)
{
    /*
     * check to see if I'm supposed to be a replica
     * definitely 'yes' if I'm the seed daemon
     *
     */

    if (ompi_process_info.seed) {  /* true if I'm the seed daemon */
	ompi_name_server = *mca_ns_replica_init(allow_multi_user_threads, have_hidden_threads);
    }

    return OMPI_SUCCESS;
}
