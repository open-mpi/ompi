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

#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "opal/threads/mutex.h"
#include "util/proc_info.h"
#include "opal/util/output.h"

#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"
#include "ns_replica.h"


/*
 * Struct of function pointers that need to be initialized
 */
OMPI_COMP_EXPORT mca_ns_base_component_t mca_ns_replica_component = {
  {
    MCA_NS_BASE_VERSION_1_0_0,

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
    orte_ns_base_get_cellid,
    orte_ns_replica_get_cell_info,
    orte_ns_base_assign_cellid_to_process,
    orte_ns_base_get_cellid_string,
    orte_ns_base_convert_cellid_to_string,
    orte_ns_base_convert_string_to_cellid,
    /* jobid functions */
    orte_ns_replica_create_jobid,
    orte_ns_base_get_jobid,
    orte_ns_base_get_jobid_string,
    orte_ns_base_convert_jobid_to_string,
    orte_ns_base_convert_string_to_jobid,
    /* vpid functions */
    orte_ns_replica_reserve_range,
    orte_ns_base_get_vpid,
    orte_ns_base_get_vpid_string,
    orte_ns_base_convert_vpid_to_string,
    orte_ns_base_convert_string_to_vpid,
    /* name functions */
    orte_ns_base_create_process_name,
    orte_ns_replica_create_my_name,
    orte_ns_base_copy_process_name,
    orte_ns_base_convert_string_to_process_name,
    orte_ns_base_free_name,
    orte_ns_base_get_proc_name_string,
    orte_ns_base_compare,
    /* peer functions */
    orte_ns_base_get_peers,
    orte_ns_replica_get_job_peers,
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


/* constructor - used to initialize state of cell_tracker instance */
static void orte_ns_replica_cell_tracker_construct(orte_ns_replica_cell_tracker_t* cell_tracker)
{
    cell_tracker->cell = 0;
    cell_tracker->site = NULL;
    cell_tracker->resource = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_ns_replica_cell_tracker_destructor(orte_ns_replica_cell_tracker_t* cell_tracker)
{
    if (NULL != cell_tracker->site) free(cell_tracker->site);
    if (NULL != cell_tracker->resource) free(cell_tracker->resource);
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
    orte_ns_replica_cell_tracker_t,  /* type name */
    opal_object_t, /* parent "class" name */
    orte_ns_replica_cell_tracker_construct, /* constructor */
    orte_ns_replica_cell_tracker_destructor); /* destructor */


/* constructor - used to initialize state of jobid_tracker instance */
static void orte_ns_replica_jobid_tracker_construct(orte_ns_replica_jobid_tracker_t* jobid_tracker)
{
    jobid_tracker->jobid = ORTE_JOBID_MAX;
    jobid_tracker->next_vpid = 0;
}

/* destructor - used to free any resources held by instance */
static void orte_ns_replica_jobid_tracker_destructor(orte_ns_replica_jobid_tracker_t* jobid_tracker){
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
		   orte_ns_replica_jobid_tracker_t,  /* type name */
           opal_object_t, /* parent "class" name */
           orte_ns_replica_jobid_tracker_construct, /* constructor */
           orte_ns_replica_jobid_tracker_destructor); /* destructor */


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
OBJ_CLASS_INSTANCE(
        orte_ns_replica_tagitem_t,  /* type name */
        opal_object_t, /* parent "class" name */
        orte_ns_replica_tagitem_construct, /* constructor */
        orte_ns_replica_tagitem_destructor); /* destructor */


/* constructor - used to initialize state of dtilist instance */
static void orte_ns_replica_dti_construct(orte_ns_replica_dti_t* dti)
{
    dti->id = ORTE_DPS_ID_MAX;
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
OBJ_CLASS_INSTANCE(
        orte_ns_replica_dti_t,  /* type name */
        opal_object_t, /* parent "class" name */
        orte_ns_replica_dti_construct, /* constructor */
        orte_ns_replica_dti_destructor); /* destructor */

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
                                orte_ns_replica.block_size,
                                orte_ns_replica.max_size,
                                orte_ns_replica.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_ns_replica.num_cells = 0;
        
      /* initialize the job id tracker */
      if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_replica.jobids),
                                orte_ns_replica.block_size,
                                orte_ns_replica.max_size,
                                orte_ns_replica.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_ns_replica.num_jobids = 0;

      /* initialize the taglist */

      if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_replica.tags),
                                orte_ns_replica.block_size,
                                orte_ns_replica.max_size,
                                orte_ns_replica.block_size))) {
            ORTE_ERROR_LOG(rc);
            return NULL;
        }
        orte_ns_replica.num_tags = 0;

      /* initialize the dtlist */

      if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_replica.dts),
                                orte_ns_replica.block_size,
                                orte_ns_replica.max_size,
                                orte_ns_replica.block_size))) {
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
    rc = orte_rml.recv_buffer_nb(ORTE_RML_NAME_ANY, ORTE_RML_TAG_NS, ORTE_RML_PERSISTENT, orte_ns_replica_recv, NULL);
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
    orte_ns_replica_jobid_tracker_t **jptr;
    orte_ns_replica_tagitem_t **tag;
    orte_ns_replica_dti_t **dti;
    size_t i;
    
  /* free all tracking storage, but only if this component was initialized */

    if (initialized) {
        cptr = (orte_ns_replica_cell_tracker_t**)(orte_ns_replica.cells)->addr;
        for (i=0; i < (orte_ns_replica.cells)->size; i++) {
            if (NULL != cptr[i]) {
                OBJ_RELEASE(cptr[i]);
            }
        }
        OBJ_RELEASE(orte_ns_replica.cells);

        jptr = (orte_ns_replica_jobid_tracker_t**)(orte_ns_replica.jobids)->addr;
        for (i=0; i < (orte_ns_replica.jobids)->size; i++) {
            if (NULL != jptr[i]) {
                OBJ_RELEASE(jptr[i]);
            }
        }
        OBJ_RELEASE(orte_ns_replica.jobids);
        
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
    
    orte_rml.recv_cancel(ORTE_RML_NAME_ANY, ORTE_RML_TAG_NS);
    return ORTE_SUCCESS;
}


/* 
 * handle message from proxies
 * NOTE: The incoming buffer "buffer" is OBJ_RELEASED by the calling program.
 * DO NOT RELEASE THIS BUFFER IN THIS CODE
 */

void orte_ns_replica_recv(int status, orte_process_name_t* sender,
			             orte_buffer_t* buffer, orte_rml_tag_t tag,
			             void* cbdata)
{
    orte_buffer_t answer, error_answer;
    orte_ns_cmd_flag_t command;
    orte_cellid_t cell;
    orte_jobid_t job;
    orte_vpid_t startvpid, range;
    char *tagname, *site, *resource;
    orte_rml_tag_t oob_tag;
    orte_data_type_t type;
    size_t count;
    int rc=ORTE_SUCCESS, ret;

    count = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &command, &count, ORTE_NS_CMD))) {
        ORTE_ERROR_LOG(rc);
        rc = ORTE_ERR_BAD_PARAM;
	    goto RETURN_ERROR;
    }

    OBJ_CONSTRUCT(&answer, orte_buffer_t);
    
    switch (command) {
        case ORTE_NS_CREATE_CELLID_CMD:
            if (ORTE_SUCCESS != (rc = orte_dps.pack(&answer, &command, 1, ORTE_NS_CMD))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &site, &count, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                rc = ORTE_ERR_BAD_PARAM;
                goto RETURN_ERROR;
            }
        
            count = 1;
            if (ORTE_SUCCESS != (rc = orte_dps.unpack(buffer, &resource, &count, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                rc = ORTE_ERR_BAD_PARAM;
                goto RETURN_ERROR;
            }

        	   rc = orte_ns_replica_create_cellid(&cell, site, resource);
               
        	   if (ORTE_SUCCESS != (ret = orte_dps.pack(&answer, &cell, 1, ORTE_CELLID))) {
                ORTE_ERROR_LOG(ret);
                goto RETURN_ERROR;
        	   }
        	   if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                goto RETURN_ERROR;
            }
            break;
     
        case ORTE_NS_CREATE_JOBID_CMD:
    	   if (ORTE_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&command, 1, ORTE_NS_CMD))) {
               ORTE_ERROR_LOG(rc);
    	       goto RETURN_ERROR;
    	   }
         
    	   if (ORTE_SUCCESS != (rc = orte_ns_replica_create_jobid(&job))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
           }
           
    	   if (OMPI_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&job, 1, ORTE_JOBID))) {
               ORTE_ERROR_LOG(rc);
    	       goto RETURN_ERROR;
    	   }
         
    	   if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
               ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
               goto RETURN_ERROR;
    	   }
           break;
     
         case ORTE_NS_RESERVE_RANGE_CMD:
               count = 1;
        	   if (OMPI_SUCCESS != (rc = orte_dps.unpack(buffer, (void*)&job, &count, ORTE_JOBID))) {
                       ORTE_ERROR_LOG(rc);
        	       goto RETURN_ERROR;
        	   }
         
               count = 1;
        	   if (OMPI_SUCCESS != (rc = orte_dps.unpack(buffer, (void*)&range, &count, ORTE_VPID))) {
                       ORTE_ERROR_LOG(rc);
        	       goto RETURN_ERROR;
        	   }
        
        	   if (ORTE_SUCCESS != (rc = orte_ns_replica_reserve_range(job, range, &startvpid))) {
                       ORTE_ERROR_LOG(rc);
                       goto RETURN_ERROR;
                   }
               
        	   if (OMPI_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&command, 1, ORTE_NS_CMD))) {
                       ORTE_ERROR_LOG(rc);
        	       goto RETURN_ERROR;
        	   }
        
        	   if (OMPI_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&startvpid, 1, ORTE_VPID))) {
                       ORTE_ERROR_LOG(rc);
        	       goto RETURN_ERROR;
        	   }
             
        	   if (0 > (rc = orte_rml.send_buffer(sender, &answer, tag, 0))) {
                   ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                   goto RETURN_ERROR;
        	   }
               break;
    
        case ORTE_NS_ASSIGN_OOB_TAG_CMD:
               count = 1;
               if (0 > orte_dps.unpack(buffer, &tagname, &count, ORTE_STRING)) {
                 rc = ORTE_ERR_UNPACK_FAILURE;
                 goto RETURN_ERROR;
               }
        
               if (0 == strncmp(tagname, "NULL", 4)) {
                    if (ORTE_SUCCESS != (rc = orte_ns_replica_assign_rml_tag(&oob_tag, NULL))) {
                        goto RETURN_ERROR;
                    }
               } else {
                    if (ORTE_SUCCESS != (rc = orte_ns_replica_assign_rml_tag(&oob_tag, tagname))) {
                        goto RETURN_ERROR;
                    }
               }
               
              if (OMPI_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&command, 1, ORTE_NS_CMD))) {
                 goto RETURN_ERROR;
              }
        
              if (OMPI_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&oob_tag, 1, ORTE_UINT32))) {
                  goto RETURN_ERROR;
              }
             
              if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                   ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                   goto RETURN_ERROR;
              }
              break;

        case ORTE_NS_DEFINE_DATA_TYPE_CMD:
               count = 1;
               if (0 > orte_dps.unpack(buffer, &tagname, &count, ORTE_STRING)) {
                 rc = ORTE_ERR_UNPACK_FAILURE;
                 goto RETURN_ERROR;
               }
        
                if (ORTE_SUCCESS != (rc = orte_ns_replica_define_data_type(tagname, &type))) {
                    goto RETURN_ERROR;
                }
               
              if (OMPI_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&command, 1, ORTE_NS_CMD))) {
                 goto RETURN_ERROR;
              }
        
              if (OMPI_SUCCESS != (rc = orte_dps.pack(&answer, (void*)&type, 1, ORTE_DATA_TYPE))) {
                  goto RETURN_ERROR;
              }
             
              if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                   ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                   goto RETURN_ERROR;
              }
              break;
            
        case ORTE_NS_CREATE_MY_NAME_CMD:
            /* ignore this command */
            break;
            
        case ORTE_NS_DUMP_CELLS_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_cells_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                 ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                 goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DUMP_JOBIDS_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_jobs_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                 ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                 goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DUMP_TAGS_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_tags_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                 ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                 goto RETURN_ERROR;
            }
            break;
            
        case ORTE_NS_DUMP_DATATYPES_CMD:
            if (ORTE_SUCCESS != (rc = orte_ns_replica_dump_datatypes_fn(&answer))) {
                ORTE_ERROR_LOG(rc);
                goto RETURN_ERROR;
            }
            if (0 > orte_rml.send_buffer(sender, &answer, tag, 0)) {
                 ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
                 goto RETURN_ERROR;
            }
            break;
            
        default:
            goto RETURN_ERROR;
    }
    goto CLEANUP;
    
RETURN_ERROR:
    OBJ_CONSTRUCT(&error_answer, orte_buffer_t);
    orte_dps.pack(&error_answer, (void*)&command, 1, ORTE_NS_CMD);
    orte_dps.pack(&error_answer, (void*)&rc, 1, ORTE_INT32);
    orte_rml.send_buffer(sender, &error_answer, tag, 0);
    OBJ_DESTRUCT(&error_answer);

CLEANUP:
    /* cleanup */
    OBJ_DESTRUCT(&answer);
}

