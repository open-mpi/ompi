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

#include "ompi_config.h"

#include "include/constants.h"

#include "runtime/runtime.h"
#include "mca/oob/oob.h"
#include "mca/pcmclient/base/base.h"

int ompi_rte_init_cleanup(void)
{
    int ret;

    /*
     * Call back into OOB to allow do any final initialization
     * (e.g. put contact info in register).
     */
    if (OMPI_SUCCESS != (ret = mca_oob_base_module_init())) {
       ompi_output(0, "ompi_rte_init: failed in mca_oob_base_module_init()\n");
       return ret;
    }

    /*
     * Let the pcmclient finish up
     */
    mca_pcmclient.pcmclient_init_cleanup();

    return OMPI_SUCCESS;
}
