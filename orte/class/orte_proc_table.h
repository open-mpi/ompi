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
 *
 */

/** @file 
 *
 *  A hash table indexed by orte_process_name_t.
 */

#ifndef ORTE_PROC_TABLE_H
#define ORTE_PROC_TABLE_H

#include "orte_config.h"
#include "orte/types.h"

#include "opal/class/opal_hash_table.h"

#include "orte/util/name_fns.h"

BEGIN_C_DECLS

/**
 *  Retrieve value via orte_process_name_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  The value associated with the key or NULL if the item is not found.
 *
 */

ORTE_DECLSPEC void *orte_hash_table_get_proc(
    opal_hash_table_t* table, 
    const orte_process_name_t* key);

/**
 *  Set value based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The value to be associated with the key (IN).
 *  @return  ORTE return code.
 *
 */

ORTE_DECLSPEC int orte_hash_table_set_proc(
    opal_hash_table_t* table, 
    const orte_process_name_t*, 
    void* value);

/**
 *  Remove value based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  ORTE return code.
 *
 */

ORTE_DECLSPEC int orte_hash_table_remove_proc(
    opal_hash_table_t* table, 
    const orte_process_name_t* key);


/**
*  Retrieve process name via orte_process_name_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  The process name associated with the key or
 *           ORTE_NAME_INVALID if the item is not found.
 *
 */

ORTE_DECLSPEC orte_process_name_t orte_hash_table_get_proc_name(
    opal_hash_table_t* table, 
    const orte_process_name_t* key,
    orte_ns_cmp_bitmask_t mask);

/**
*  Set process name based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @param   value   The process name to be associated with the key (IN).
 *  @return  ORTE return code.
 *
 */

ORTE_DECLSPEC int orte_hash_table_set_proc_name(
    opal_hash_table_t* table, 
    const orte_process_name_t*, 
    const orte_process_name_t*,
    orte_ns_cmp_bitmask_t mask);

/**
*  Remove process name based on uint32_t key.
 *
 *  @param   table   The input hash table (IN).
 *  @param   key     The input key (IN).
 *  @return  ORTE return code.
 *
 */

ORTE_DECLSPEC int orte_hash_table_remove_proc_name(
    opal_hash_table_t* table, 
    const orte_process_name_t* key,
    orte_ns_cmp_bitmask_t mask);

END_C_DECLS


#endif  /* OMPI_HASH_TABLE_H */
