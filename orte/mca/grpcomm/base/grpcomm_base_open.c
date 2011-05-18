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


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"


#include "orte/mca/grpcomm/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/grpcomm/base/static-components.h"

/*
 * Global variables
 */
orte_grpcomm_base_t orte_grpcomm_base;

orte_grpcomm_base_module_t orte_grpcomm = {0};

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_grpcomm_base_open(void)
{
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_grpcomm_base.output = opal_output_open(NULL);
    
    /* define the default daemon collective fn */
#if ORTE_DISABLE_FULL_SUPPORT
    orte_grpcomm_base.daemon_coll = NULL;
#else
    orte_grpcomm_base.daemon_coll = orte_grpcomm_base_daemon_collective;
#endif
    
    /* Open up all available components */

    if (ORTE_SUCCESS !=
        mca_base_components_open("grpcomm", orte_grpcomm_base.output,
                                 mca_grpcomm_base_static_components,
                                 &orte_grpcomm_base.components_available, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    return ORTE_SUCCESS;
}

/* local objects */
static void collective_constructor(orte_grpcomm_collective_t *ptr)
{
    OBJ_CONSTRUCT(&ptr->lock, opal_mutex_t);
    OBJ_CONSTRUCT(&ptr->cond, opal_condition_t);
    OBJ_CONSTRUCT(&ptr->results, opal_buffer_t);
    ptr->recvd = 0;
}
static void collective_destructor(orte_grpcomm_collective_t *ptr)
{
    OBJ_DESTRUCT(&ptr->lock);
    OBJ_DESTRUCT(&ptr->cond);
    OBJ_DESTRUCT(&ptr->results);
}
OBJ_CLASS_INSTANCE(orte_grpcomm_collective_t,
                   opal_object_t,
                   collective_constructor,
                   collective_destructor);
