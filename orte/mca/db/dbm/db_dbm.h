/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORTE_DB_DBM_H
#define ORTE_DB_DBM_H

#include "orte/mca/db/db.h"

BEGIN_C_DECLS

/*
 * Module open / close
 */
int orte_db_dbm_component_open(void);
int orte_db_dbm_component_close(void);
int orte_db_dbm_component_query(mca_base_module_t **module, int *priority);


ORTE_MODULE_DECLSPEC extern orte_db_base_component_t mca_db_dbm_component;
ORTE_DECLSPEC extern orte_db_base_module_t orte_db_dbm_module;
extern char *orte_db_dbm_directory;

END_C_DECLS

#endif /* ORTE_DB_DBM_H */
