/* -*- C -*-
*
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
* The Open MPI Name Server
*
* The Open MPI Name Server provides unique name ranges for processes
* within the universe. Each universe will have one name server
* running within the seed daemon.  This is done to prevent the
* inadvertent duplication of names.
*/

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/threads/mutex.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/ns/base/ns_private.h"
#include "ns_replica.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_ns_base_component_t mca_ns_replica_component = {
{
    MCA_NS_BASE_VERSION_2_0_0,
    
    "replica", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_ns_replica_open,  /* module open */
    orte_ns_replica_close /* module close */
},
{
    false /* checkpoint / restart */
},
orte_ns_replica_init,    /* module init */
orte_ns_replica_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static mca_ns_base_module_t orte_ns_replica_module = {
    /* init */
    orte_ns_replica_module_init,
    /* cell functions */
    orte_ns_replica_create_cellid,
    orte_ns_replica_get_cell_info,
    orte_ns_base_get_cellid_string,
    orte_ns_base_convert_cellid_to_string,
    orte_ns_base_convert_string_to_cellid,
    /** node functions */
    orte_ns_replica_create_nodeids,
    orte_ns_replica_get_node_info,
    orte_ns_base_convert_nodeid_to_string,
    orte_ns_base_convert_string_to_nodeid,
    /* jobid functions */
    orte_ns_replica_create_jobid,
    orte_ns_replica_get_job_descendants,
    orte_ns_replica_get_job_children,
    orte_ns_replica_get_root_job,
    orte_ns_replica_get_parent_job,
    orte_ns_base_get_jobid_string,
    orte_ns_base_convert_jobid_to_string,
    orte_ns_base_convert_string_to_jobid,
    orte_ns_replica_reserve_range,
    /* vpid functions */
    orte_ns_base_get_vpid_string,
    orte_ns_base_convert_vpid_to_string,
    orte_ns_base_convert_string_to_vpid,
    /* name functions */
    orte_ns_base_create_process_name,
    orte_ns_replica_create_my_name,
    orte_ns_base_convert_string_to_process_name,
    orte_ns_base_get_proc_name_string,
    orte_ns_base_compare_fields,
    /* peer functions */
    orte_ns_replica_get_peers,
    /* tag server functions */
    orte_ns_replica_assign_rml_tag,
    /* data type functions */
    orte_ns_replica_define_data_type,
    /* diagnostic functions */
    orte_ns_replica_dump_cells,
    orte_ns_replica_dump_jobs,
    orte_ns_replica_dump_tags,
    orte_ns_replica_dump_datatypes
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/*
 * class instantiations
 */
#include "ns_replica_class_instances.h"

/*
 * globals needed within replica component
 */
orte_ns_replica_globals_t orte_ns_replica;

/*
 * don't really need this function - could just put NULL in the above structure
 * Just holding the place in case we decide there is something we need to do
 */
int orte_ns_replica_open(void)
{
    int id, param;
    
    id = mca_base_param_register_int("ns", "replica", "debug", NULL, (int)false);
    mca_base_param_lookup_int(id, &orte_ns_replica.debug);
    
    id = mca_base_param_register_int("ns", "replica", "isolate", NULL, (int)false);
    mca_base_param_lookup_int(id, &param);
    if (param) {
        orte_ns_replica.isolate = true;
    } else {
        orte_ns_replica.isolate = false;
    }
    
    id = mca_base_param_register_int("ns", "replica", "maxsize", NULL,
                                     ORTE_NS_ARRAY_MAX_SIZE);
    mca_base_param_lookup_int(id, &param);
    orte_ns_replica.max_size = (size_t)param;
    
    id = mca_base_param_register_int("ns", "replica", "blocksize", NULL,
                                     ORTE_NS_ARRAY_BLOCK_SIZE);
    mca_base_param_lookup_int(id, &param);
    orte_ns_replica.block_size = (size_t)param;
    
    return ORTE_SUCCESS;
}

/*
 * ditto for this one
 */
int orte_ns_replica_close(void)
{
    return ORTE_SUCCESS;
}

mca_ns_base_module_t* orte_ns_replica_init(int *priority)
{
    int rc;
    
    /* If we are to host a replica, then we want to be selected, so do all the
    setup and return the module */
    
    if (NULL == orte_process_info.ns_replica_uri) {
        
        /* Return a module (choose an arbitrary, positive priority --
        it's only relevant compared to other ns components).  If
        we're not the seed, then we don't want to be selected, so
        return NULL. */
        
        *priority = 50;
        
        /* initialize the cell info tracker */
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_replica.cells),
                                                          (orte_std_cntr_t)orte_ns_replica.block_size,
                                                          (orte_std_cntr_t)orte_ns_replica.max_size,
                                                          (orte_std_cntr_t)orte_ns_replica.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_ns_replica.num_cells = 0;
        
        /* initialize the job tracking system */
        OBJ_CONSTRUCT(&orte_ns_replica.jobs, opal_list_t);
        orte_ns_replica.num_jobids = 0;
        
        /* initialize the taglist */
        
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_replica.tags),
                                                          (orte_std_cntr_t)orte_ns_replica.block_size,
                                                          (orte_std_cntr_t)orte_ns_replica.max_size,
                                                          (orte_std_cntr_t)orte_ns_replica.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_ns_replica.num_tags = 0;
        
        /* initialize the dtlist */
        
        if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_replica.dts),
                                                          (orte_std_cntr_t)orte_ns_replica.block_size,
                                                          (orte_std_cntr_t)orte_ns_replica.max_size,
                                                          (orte_std_cntr_t)orte_ns_replica.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_ns_replica.num_dts = 0;
        
        /* setup the thread lock */
        OBJ_CONSTRUCT(&orte_ns_replica.mutex, opal_mutex_t);
        
        /* Return the module */
        
        initialized = true;
        return &orte_ns_replica_module;
    } else {
        return NULL;
    }
}

int orte_ns_replica_module_init(void)
{
    int rc;
    if (orte_ns_replica.isolate) {
        return ORTE_SUCCESS;
    }
    
    /* issue non-blocking receive for call_back function */
    rc = orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, ORTE_RML_TAG_NS, ORTE_RML_PERSISTENT, orte_ns_replica_recv, NULL);
    if(rc < 0) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}


/*
 * finalize routine
 */
int orte_ns_replica_finalize(void)
{
    orte_ns_replica_cell_tracker_t **cptr;
    opal_list_item_t *item;
    orte_ns_replica_tagitem_t **tag;
    orte_ns_replica_dti_t **dti;
    orte_std_cntr_t i;
    
    /* free all tracking storage, but only if this component was initialized */
    
    if (initialized) {
        cptr = (orte_ns_replica_cell_tracker_t**)(orte_ns_replica.cells)->addr;
        for (i=0; i < (orte_ns_replica.cells)->size; i++) {
            if (NULL != cptr[i]) {
                OBJ_RELEASE(cptr[i]);
            }
        }
        OBJ_RELEASE(orte_ns_replica.cells);
        
        while (NULL != (item = opal_list_remove_first(&orte_ns_replica.jobs))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&orte_ns_replica.jobs);
        
        tag = (orte_ns_replica_tagitem_t**)(orte_ns_replica.tags)->addr;
        for (i=0; i < (orte_ns_replica.tags)->size; i++) {
            if (NULL != tag[i]) OBJ_RELEASE(tag[i]);
        }
        OBJ_RELEASE(orte_ns_replica.tags);
        
        dti = (orte_ns_replica_dti_t**)(orte_ns_replica.dts)->addr;
        for (i=0; i < (orte_ns_replica.dts)->size; i++) {
            if (NULL != dti[i]) OBJ_RELEASE(dti[i]);
        }
        OBJ_RELEASE(orte_ns_replica.dts);
        
        initialized = false;
    }
    
    /* All done */
    if (orte_ns_replica.isolate) {
        return ORTE_SUCCESS;
    }
    
    orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_NS);
    return ORTE_SUCCESS;
}

