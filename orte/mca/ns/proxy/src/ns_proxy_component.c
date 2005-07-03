/* -*- C -*-
 *
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
#include "util/proc_info.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"

#include "ns_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
OMPI_COMP_EXPORT mca_ns_base_component_t mca_ns_proxy_component = {
  {
    MCA_NS_BASE_VERSION_1_0_0,

    "proxy", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    orte_ns_proxy_open,  /* module open */
    orte_ns_proxy_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  orte_ns_proxy_init,    /* module init */
  orte_ns_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static mca_ns_base_module_t orte_ns_proxy = {
    orte_ns_proxy_module_init,
    orte_ns_proxy_create_cellid,
    orte_ns_proxy_get_cell_info,
    orte_ns_base_assign_cellid_to_process,
    orte_ns_proxy_create_jobid,
    orte_ns_base_create_process_name,
    orte_ns_proxy_create_my_name,
    orte_ns_base_copy_process_name,
    orte_ns_base_convert_string_to_process_name,
    orte_ns_proxy_reserve_range,
    orte_ns_base_free_name,
    orte_ns_base_get_proc_name_string,
    orte_ns_base_get_vpid_string,
    orte_ns_base_convert_vpid_to_string,
    orte_ns_base_convert_string_to_vpid,
    orte_ns_base_get_jobid_string,
    orte_ns_base_convert_jobid_to_string,
    orte_ns_base_convert_string_to_jobid,
    orte_ns_base_get_cellid_string,
    orte_ns_base_convert_cellid_to_string,
    orte_ns_base_convert_string_to_cellid,
    orte_ns_base_get_vpid,
    orte_ns_base_get_jobid,
    orte_ns_base_get_cellid,
    orte_ns_base_compare,
    orte_ns_base_derive_vpid,
    orte_ns_proxy_assign_rml_tag,
    orte_ns_proxy_define_data_type,
    orte_ns_base_set_my_name,
    orte_ns_base_get_peers
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/* constructor - used to initialize state of cell info list instance */
static void orte_ns_proxy_cell_info_construct(orte_ns_proxy_cell_info_t* ptr)
{
    ptr->resource = NULL;
    ptr->site = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_ns_proxy_cell_info_destructor(orte_ns_proxy_cell_info_t* ptr)
{
    if (NULL != ptr->resource) {
       free(ptr->resource);
    }
    if (NULL != ptr->site) {
       free(ptr->site);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
        orte_ns_proxy_cell_info_t,  /* type name */
        opal_list_item_t, /* parent "class" name */
        orte_ns_proxy_cell_info_construct, /* constructor */
        orte_ns_proxy_cell_info_destructor); /* destructor */

/* constructor - used to initialize state of taglist instance */
static void orte_ns_proxy_tagitem_construct(orte_ns_proxy_tagitem_t* tagitem)
{
    tagitem->tag = ORTE_RML_TAG_MAX;
    tagitem->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_ns_proxy_tagitem_destructor(orte_ns_proxy_tagitem_t* tagitem)
{
    if (NULL != tagitem->name) {
       free(tagitem->name);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
        orte_ns_proxy_tagitem_t,  /* type name */
        opal_list_item_t, /* parent "class" name */
        orte_ns_proxy_tagitem_construct, /* constructor */
        orte_ns_proxy_tagitem_destructor); /* destructor */

/* constructor - used to initialize state of dtilist instance */
static void orte_ns_proxy_dti_construct(orte_ns_proxy_dti_t* dti)
{
    dti->id = ORTE_DPS_ID_MAX;
    dti->name = NULL;
}

/* destructor - used to free any resources held by instance */
static void orte_ns_proxy_dti_destructor(orte_ns_proxy_dti_t* dti)
{
    if (NULL != dti->name) {
       free(dti->name);
    }
}

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
        orte_ns_proxy_dti_t,  /* type name */
        opal_list_item_t, /* parent "class" name */
        orte_ns_proxy_dti_construct, /* constructor */
        orte_ns_proxy_dti_destructor); /* destructor */

/*
 * globals needed within proxy component
 */

orte_process_name_t* orte_ns_my_replica=NULL;
int orte_ns_proxy_debug=0;
opal_list_t orte_ns_proxy_cell_info_list;
opal_list_t orte_ns_proxy_taglist;
opal_list_t orte_ns_proxy_dtlist;
ompi_mutex_t orte_ns_proxy_mutex;

/*
 * Open the proxy component and obtain the name of my replica.
 */
int orte_ns_proxy_open(void)
{
    int id;

    id = mca_base_param_register_int("ns", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &orte_ns_proxy_debug);

    return ORTE_SUCCESS;
}

/*
 * ditto for this one
 */
int orte_ns_proxy_close(void)
{
    return ORTE_SUCCESS;
}

mca_ns_base_module_t* orte_ns_proxy_init(int *priority)
{
    orte_process_name_t name;
    int ret;
    
    /* If we are NOT to host a replica, then we want to be selected, so do all
       the setup and return the module */
    /*    ompi_output(mca_ns_base_output, "ns_proxy: entered init\n"); */
    if (NULL != orte_process_info.ns_replica_uri) {

        	/* Return a module (choose an arbitrary, positive priority --
        	   it's only relevant compared to other ns components).  If
        	   we're not the seed, then we don't want to be selected, so
        	   return NULL. */
        
        	*priority = 10;
        
        	/* define the replica for us to use */
           if(ORTE_SUCCESS != (ret = orte_rml.parse_uris(orte_process_info.ns_replica_uri, &name, NULL))) {
               ORTE_ERROR_LOG(ret);
               return NULL;
           }
           if(ORTE_SUCCESS != (ret = orte_ns.copy_process_name(&orte_process_info.ns_replica, &name))) {
               ORTE_ERROR_LOG(ret);
               return NULL;
           }
        	if (ORTE_SUCCESS != orte_ns_base_copy_process_name(&orte_ns_my_replica,
                                orte_process_info.ns_replica)) {  /* can't operate */
        	    return NULL;
        	}
        
        /* initialize the cell info list */
        OBJ_CONSTRUCT(&orte_ns_proxy_cell_info_list, opal_list_t);
    
        /* initialize the taglist */
        OBJ_CONSTRUCT(&orte_ns_proxy_taglist, opal_list_t);
    
        /* initialize the dtlist */
        OBJ_CONSTRUCT(&orte_ns_proxy_dtlist, opal_list_t);
    
    	    /* Return the module */
    
    	   initialized = true;
    	   return &orte_ns_proxy;
        
    } else {
	   return NULL;
    }
}


/*
 * module init function
 */
int orte_ns_proxy_module_init(void)
{
    return ORTE_SUCCESS;
}


/*
 * finalize routine
 */
int orte_ns_proxy_finalize(void)
{
    orte_ns_proxy_tagitem_t *tagitem;
    orte_ns_proxy_cell_info_t *cptr;
    
    if (orte_ns_proxy_debug) {
	   ompi_output(0, "finalizing ns proxy");
    }

    /* free the storage, but only if this component was initialized */

    if (initialized) {
        while (NULL != (cptr = (orte_ns_proxy_cell_info_t*)opal_list_remove_first(&orte_ns_proxy_cell_info_list))) {
            OBJ_RELEASE(cptr);
        }
        OBJ_DESTRUCT(&orte_ns_proxy_cell_info_list);
        while (NULL != (tagitem = (orte_ns_proxy_tagitem_t*)opal_list_remove_first(&orte_ns_proxy_taglist))) {
            OBJ_RELEASE(tagitem);
        }
        OBJ_DESTRUCT(&orte_ns_proxy_taglist);
        initialized = false;
    }

    /* All done */

    return ORTE_SUCCESS;
}
