/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/db/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * dbments and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/db/base/static-components.h"

opal_list_t orte_db_base_components_available;

/* provide "NULL" functions */
static int init(void);
static int finalize(void);
static int store(char *key, void *object, opal_data_type_t type);
static int set_source(orte_process_name_t *name);
static int fetch(char *key, void *object, opal_data_type_t type);
static int update(char *key, void *object, opal_data_type_t type);
static int remove_data(char *key);

orte_db_base_module_t orte_db = {
    init,
    finalize,
    store,
    set_source,
    fetch,
    update,
    remove_data
};
int orte_db_base_output;

int
orte_db_base_open(void)
{
    orte_db_base_output = opal_output_open(NULL);
    
    OBJ_CONSTRUCT(&orte_db_base_components_available, opal_list_t);

    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("db", orte_db_base_output, mca_db_base_static_components, 
                                 &orte_db_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static int init(void)
{
    return ORTE_SUCCESS;
}
static int finalize(void)
{
    return ORTE_SUCCESS;
}
static int store(char *key, void *object, opal_data_type_t type)
{
    return ORTE_SUCCESS;
}
static int set_source(orte_process_name_t *name)
{
    return ORTE_SUCCESS;
}
static int fetch(char *key, void *object, opal_data_type_t type)
{
    return ORTE_SUCCESS;
}
static int update(char *key, void *object, opal_data_type_t type)
{
    return ORTE_SUCCESS;
}
static int remove_data(char *key)
{
    return ORTE_SUCCESS;
}
