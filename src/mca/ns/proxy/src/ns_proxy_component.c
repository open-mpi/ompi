/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
    orte_ns_base_assign_cellid_to_process,
    orte_ns_proxy_create_jobid,
    orte_ns_base_create_process_name,
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
    orte_ns_base_set_my_name,
    orte_ns_base_get_peers
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

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

/* define instance of ompi_class_t */
OBJ_CLASS_INSTANCE(
        orte_ns_proxy_tagitem_t,  /* type name */
        ompi_list_item_t, /* parent "class" name */
        orte_ns_proxy_tagitem_construct, /* constructor */
        orte_ns_proxy_tagitem_destructor); /* destructor */

/*
 * globals needed within proxy component
 */

orte_process_name_t* orte_ns_my_replica=NULL;
int orte_ns_proxy_debug=0;
ompi_list_t orte_ns_proxy_taglist;
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
    /* If we are NOT to host a replica, then we want to be selected, so do all
       the setup and return the module */
    /*    ompi_output(mca_ns_base_output, "ns_proxy: entered init\n"); */
    if (NULL != orte_process_info.ns_replica) {

        	/* Return a module (choose an arbitrary, positive priority --
        	   it's only relevant compared to other ns components).  If
        	   we're not the seed, then we don't want to be selected, so
        	   return NULL. */
        
        	*priority = 10;
        
        	/* define the replica for us to use */
        	/* default to seed for now */
        	if (ORTE_SUCCESS != orte_ns_base_copy_process_name(&orte_ns_my_replica,
                                orte_process_info.ns_replica)) {  /* can't operate */
        	    return NULL;
        	}
        
        /* initialize the taglist */
    
        OBJ_CONSTRUCT(&orte_ns_proxy_taglist, ompi_list_t);
    
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
    
    if (orte_ns_proxy_debug) {
	   ompi_output(0, "finalizing ns proxy");
    }

    /* free the taglist storage, but only if this component was initialized */

    if (initialized) {
        while (NULL != (tagitem = (orte_ns_proxy_tagitem_t*)ompi_list_remove_first(&orte_ns_proxy_taglist))) {
            OBJ_RELEASE(tagitem);
        }
        OBJ_DESTRUCT(&orte_ns_proxy_taglist);
        initialized = false;
    }

    /* All done */

    return ORTE_SUCCESS;
}
