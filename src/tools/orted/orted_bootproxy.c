/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file **/

#include "orte_config.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "include/orte_constants.h"
#include "runtime/runtime.h"
#include "util/daemon_init.h"
#include "util/univ_info.h"
#include "mca/pls/pls.h"
#include "mca/pls/base/base.h"
#include "orted.h"



int orte_daemon_bootproxy(void)
{
    orte_pls_base_module_t* pls;

    /* lookup launcher */
    pls = orte_pls_base_select("fork");
    if(NULL == pls) {
        return ORTE_ERR_NOT_AVAILABLE;
    }

    /* launch the requested procs */
    return pls->launch(orted_globals.bootproxy);
}

