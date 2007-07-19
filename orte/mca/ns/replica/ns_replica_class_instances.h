/* -*- C -*-
* 
* Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
*                         University Research and Technology
*                         Corporation.  All rights reserved.
* Copyright (c) 2004-2006 The University of Tennessee and The University
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
#ifndef NS_REPLICA_CLASS_INSTANCES_H
#define NS_REPLICA_CLASS_INSTANCES_H

#include "orte_config.h"
#include "orte/orte_types.h"
#include "orte/orte_constants.h"
#include "opal/threads/mutex.h"
#include "opal/class/opal_object.h"
#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/ns/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    
/***   JOBITEM   ***/   
/* constructor - used to initialize state of jobitem instance */
static void orte_ns_replica_jobitem_construct(orte_ns_replica_jobitem_t *ptr)
{
    ptr->jobid = ORTE_JOBID_INVALID;
    ptr->next_vpid = 0;
    OBJ_CONSTRUCT(&ptr->children, opal_list_t);
}

/* destructor - used to free any resources held by instance */
static void orte_ns_replica_jobitem_destructor(orte_ns_replica_jobitem_t *ptr){
    opal_list_item_t *item;
    
    while (NULL != (item = opal_list_remove_first(&ptr->children))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->children);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(orte_ns_replica_jobitem_t,  /* type name */
                   opal_list_item_t, /* parent "class" name */
                   orte_ns_replica_jobitem_construct, /* constructor */
                   orte_ns_replica_jobitem_destructor); /* destructor */


/***   RML TAG    ***/
/* constructor - used to initialize state of taglist instance */
static void orte_ns_replica_tagitem_construct(orte_ns_replica_tagitem_t* tagitem)
{
    tagitem->tag = ORTE_RML_TAG_MAX;
    tagitem->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_ns_replica_tagitem_destructor(orte_ns_replica_tagitem_t* tagitem)
{
    if (NULL != tagitem->name) {
        free(tagitem->name);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(orte_ns_replica_tagitem_t,  /* type name */
                   opal_object_t, /* parent "class" name */
                   orte_ns_replica_tagitem_construct, /* constructor */
                   orte_ns_replica_tagitem_destructor); /* destructor */


/***   DATA TYPE   ***/
/* constructor - used to initialize state of dtilist instance */
static void orte_ns_replica_dti_construct(orte_ns_replica_dti_t* dti)
{
    dti->id = ORTE_DSS_ID_MAX;
    dti->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_ns_replica_dti_destructor(orte_ns_replica_dti_t* dti)
{
    if (NULL != dti->name) {
        free(dti->name);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(orte_ns_replica_dti_t,  /* type name */
                   opal_object_t, /* parent "class" name */
                   orte_ns_replica_dti_construct, /* constructor */
                   orte_ns_replica_dti_destructor); /* destructor */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
