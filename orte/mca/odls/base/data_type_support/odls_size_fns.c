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

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/mca/odls/base/odls_private.h"

/*
 * STANDARD SIZE FUNCTION - WORKS FOR EVERYTHING NON-STRUCTURED
 */
int orte_odls_size_daemon_cmd(size_t *size, orte_daemon_cmd_flag_t *src, orte_data_type_t type)
{
    *size = sizeof(orte_daemon_cmd_flag_t);

    return ORTE_SUCCESS;
}
