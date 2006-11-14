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
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "ns_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_ns_base_component_t mca_ns_proxy_component = {
  {
    MCA_NS_BASE_VERSION_2_0_0,

    "proxy", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
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
static mca_ns_base_module_t orte_ns_proxy_module = {
    /* init */
    orte_ns_proxy_module_init,
    /* cell functions */
    orte_ns_proxy_create_cellid,
    orte_ns_proxy_get_cell_info,
    orte_ns_base_get_cellid_string,
    orte_ns_base_convert_cellid_to_string,
    orte_ns_base_convert_string_to_cellid,
    /** node functions */
    orte_ns_proxy_create_nodeids,
    orte_ns_proxy_get_node_info,
    orte_ns_base_convert_nodeid_to_string,
    orte_ns_base_convert_string_to_nodeid,
    /* jobid functions */
    orte_ns_proxy_create_jobid,
    orte_ns_proxy_get_job_descendants,
    orte_ns_proxy_get_job_children,
    orte_ns_proxy_get_root_job,
    orte_ns_proxy_get_parent_job,
    orte_ns_base_get_jobid_string,
    orte_ns_base_convert_jobid_to_string,
    orte_ns_base_convert_string_to_jobid,
    orte_ns_proxy_reserve_range,
    /* vpid functions */
    orte_ns_base_get_vpid_string,
    orte_ns_base_convert_vpid_to_string,
    orte_ns_base_convert_string_to_vpid,
    /* name functions */
    orte_ns_base_create_process_name,
    orte_ns_proxy_create_my_name,
    orte_ns_base_convert_string_to_process_name,
    orte_ns_base_get_proc_name_string,
    orte_ns_base_compare_fields,
    /* peer functions */
    orte_ns_proxy_get_peers,
    /* tag server functions */
    orte_ns_proxy_assign_rml_tag,
    /* data type functions */
    orte_ns_proxy_define_data_type,
    /* diagnostic functions */
    orte_ns_proxy_dump_cells,
    orte_ns_proxy_dump_jobs,
    orte_ns_proxy_dump_tags,
    orte_ns_proxy_dump_datatypes
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

/* define instance of opal_class_t */
OBJ_CLASS_INSTANCE(
        orte_ns_proxy_tagitem_t,  /* type name */
        opal_object_t, /* parent "class" name */
        orte_ns_proxy_tagitem_construct, /* constructor */
        orte_ns_proxy_tagitem_destructor); /* destructor */

/* constructor - used to initialize state of dtilist instance */
static void orte_ns_proxy_dti_construct(orte_ns_proxy_dti_t* dti)
{
    dti->id = ORTE_DSS_ID_MAX;
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
        opal_object_t, /* parent "class" name */
        orte_ns_proxy_dti_construct, /* constructor */
        orte_ns_proxy_dti_destructor); /* destructor */

/*
 * globals needed within proxy component
 */

orte_ns_proxy_globals_t orte_ns_proxy;


/*
 * Open the proxy component and obtain the name of my proxy.
 */
int orte_ns_proxy_open(void)
{
    int id, param;

    id = mca_base_param_register_int("ns", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &orte_ns_proxy.debug);

    id = mca_base_param_register_int("ns", "proxy", "maxsize", NULL,
                                     ORTE_NS_ARRAY_MAX_SIZE);
    mca_base_param_lookup_int(id, &param);
    orte_ns_proxy.max_size = (size_t)param;

    id = mca_base_param_register_int("ns", "proxy", "blocksize", NULL,
                                     ORTE_NS_ARRAY_BLOCK_SIZE);
    mca_base_param_lookup_int(id, &param);
    orte_ns_proxy.block_size = (size_t)param;

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
    int ret, rc;

    /* If we are NOT to host a proxy, then we want to be selected, so do all
       the setup and return the module */
    /*    opal_output(mca_ns_base_output, "ns_proxy: entered init\n"); */
    if (NULL != orte_process_info.ns_replica_uri) {

            /* Return a module (choose an arbitrary, positive priority --
               it's only relevant compared to other ns components).  If
               we're not the seed, then we don't want to be selected, so
               return NULL. */

            *priority = 10;

            /* define the proxy for us to use */
           if(ORTE_SUCCESS != (ret = orte_rml.parse_uris(orte_process_info.ns_replica_uri, &name, NULL))) {
               ORTE_ERROR_LOG(ret);
               return NULL;
           }
           if(ORTE_SUCCESS != (ret = orte_dss.copy((void**)&orte_process_info.ns_replica, &name, ORTE_NAME))) {
               ORTE_ERROR_LOG(ret);
               return NULL;
           }

          /* initialize the cell info tracker */
          if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_proxy.cells),
                                    (orte_std_cntr_t)orte_ns_proxy.block_size,
                                    (orte_std_cntr_t)orte_ns_proxy.max_size,
                                    (orte_std_cntr_t)orte_ns_proxy.block_size))) {
                ORTE_ERROR_LOG(rc);
                return NULL;
            }
            orte_ns_proxy.num_cells = 0;


          /* initialize the taglist */

          if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_proxy.tags),
                                    (orte_std_cntr_t)orte_ns_proxy.block_size,
                                    (orte_std_cntr_t)orte_ns_proxy.max_size,
                                    (orte_std_cntr_t)orte_ns_proxy.block_size))) {
                ORTE_ERROR_LOG(rc);
                return NULL;
            }
            orte_ns_proxy.num_tags = 0;

          /* initialize the dtlist */

          if (ORTE_SUCCESS != (rc = orte_pointer_array_init(&(orte_ns_proxy.dts),
                                    (orte_std_cntr_t)orte_ns_proxy.block_size,
                                    (orte_std_cntr_t)orte_ns_proxy.max_size,
                                    (orte_std_cntr_t)orte_ns_proxy.block_size))) {
                ORTE_ERROR_LOG(rc);
                return NULL;
            }
            orte_ns_proxy.num_dts = 0;

          /* setup the thread lock */
          OBJ_CONSTRUCT(&orte_ns_proxy.mutex, opal_mutex_t);

            /* Return the module */

           initialized = true;
           return &orte_ns_proxy_module;

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
    orte_ns_proxy_tagitem_t **tag;
    orte_ns_proxy_dti_t **dti;
    orte_std_cntr_t i;

  /* free all tracking storage, but only if this component was initialized */

    if (initialized) {
        tag = (orte_ns_proxy_tagitem_t**)(orte_ns_proxy.tags)->addr;
        for (i=0; i < (orte_ns_proxy.tags)->size; i++) {
            if (NULL != tag[i]) OBJ_RELEASE(tag[i]);
        }
        OBJ_RELEASE(orte_ns_proxy.tags);

        dti = (orte_ns_proxy_dti_t**)(orte_ns_proxy.dts)->addr;
        for (i=0; i < (orte_ns_proxy.dts)->size; i++) {
            if (NULL != dti[i]) OBJ_RELEASE(dti[i]);
        }
        OBJ_RELEASE(orte_ns_proxy.dts);

        initialized = false;
    }

    /* All done */

    return ORTE_SUCCESS;
}
