/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "gpr_replica_api.h"

/* COMPOUND COMMANDS ARE NOT USED ON THE REPLICA
 * Any process co-located with the replica will "drive" the registry
 * directly
 */
int orte_gpr_replica_begin_compound_cmd(void)
{
    return ORTE_SUCCESS;
}


int orte_gpr_replica_stop_compound_cmd(void)
{
    return ORTE_SUCCESS;
}


int orte_gpr_replica_exec_compound_cmd(void)
{
    return ORTE_SUCCESS;
}
