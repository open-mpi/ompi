/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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


#include "ompi_config.h"

#include "opal/util/output.h"
#include "runtime/runtime.h"
#include "mca/ns/ns.h"

#include "mca/errmgr/base/base.h"


void mca_errmgr_base_log(char *msg, char *filename, int line)
{
    opal_output(0, "[%lu,%lu,%lu] ORTE_ERROR_LOG: %s in file %s at line %d",
                ORTE_NAME_ARGS(orte_process_info.my_name), 
                msg, filename, line);
}
