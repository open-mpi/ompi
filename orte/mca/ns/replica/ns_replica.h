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
#ifndef NS_REPLICA_H
#define NS_REPLICA_H

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

/*
 * globals
 */
#define NS_REPLICA_MAX_STRING_SIZE  256
    
    
/* class for tracking cellid's */
struct orte_ns_replica_cell_tracker_t {
    opal_object_t super;
    orte_cellid_t cell;
    char *site;
    char *resource;
    orte_nodeid_t next_nodeid;
    orte_pointer_array_t *nodeids;
};
typedef struct orte_ns_replica_cell_tracker_t orte_ns_replica_cell_tracker_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_cell_tracker_t);

/* object for tracking nodeid's */
struct orte_ns_replica_nodeid_tracker_t {
    opal_object_t super;
    char *nodename;
    orte_nodeid_t nodeid;
};
typedef struct orte_ns_replica_nodeid_tracker_t orte_ns_replica_nodeid_tracker_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_nodeid_tracker_t);


/*
 * object for tracking vpids and jobids for job families
 * This structure is used to track the parent-child relationship between
 * jobs. The "root" of the family is the initial parent - each child has
 * a record under that parent. Any child that subsequently spawns its own
 * children will form a list of jobids beneath them.
 *
 * each object records the jobid of the job it represents, and the next vpid
 * that will be assigned when a range is requested.
 */
typedef struct  {
    opal_list_item_t super;
    orte_jobid_t jobid;
    orte_vpid_t next_vpid;
    opal_list_t children;
} orte_ns_replica_jobitem_t;
OBJ_CLASS_DECLARATION(orte_ns_replica_jobitem_t);


struct orte_ns_replica_tagitem_t {
    opal_object_t super;
    orte_rml_tag_t tag;  /**< OOB tag */
    char *name;      /**< Name associated with tag */
};
typedef struct orte_ns_replica_tagitem_t orte_ns_replica_tagitem_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_tagitem_t);

struct orte_ns_replica_dti_t {
    opal_object_t super;
    orte_data_type_t id;  /**< data type id */
    char *name;      /**< Name associated with data type */
};
typedef struct orte_ns_replica_dti_t orte_ns_replica_dti_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_dti_t);

/*
 * globals needed within component
 */
typedef struct {
    size_t max_size, block_size;
    orte_cellid_t num_cells;
    orte_pointer_array_t *cells;
    orte_jobid_t num_jobids;
    opal_list_t jobs;
    orte_pointer_array_t *tags;
    orte_rml_tag_t num_tags;
    orte_pointer_array_t *dts;
    orte_data_type_t num_dts;
    int debug;
    bool isolate;
    opal_mutex_t mutex;
} orte_ns_replica_globals_t;
 
extern orte_ns_replica_globals_t orte_ns_replica;

/*
 * Module open / close
 */
int orte_ns_replica_open(void);
int orte_ns_replica_close(void);


/*
 * Startup / Shutdown
 */
mca_ns_base_module_t* orte_ns_replica_init(int *priority);
int orte_ns_replica_module_init(void);
int orte_ns_replica_finalize(void);

/*
 * oob interface
 */

void orte_ns_replica_recv(int status, orte_process_name_t* sender,
                          orte_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);

/*
 * CELL FUNCTIONS
 */
int orte_ns_replica_create_cellid(orte_cellid_t *cellid, char *site, char *resource);

int orte_ns_replica_get_cell_info(orte_cellid_t cellid,
                                char **site, char **resource);

int orte_ns_replica_create_nodeids(orte_nodeid_t **nodeids, orte_std_cntr_t *nnodes,
                                   orte_cellid_t cellid, char **nodenames);

int orte_ns_replica_get_node_info(char ***nodenames, orte_cellid_t cellid, orte_std_cntr_t num_nodes, orte_nodeid_t *nodeids);

/*
 * JOB FUNCTIONS
 */
int orte_ns_replica_create_jobid(orte_jobid_t *jobid, opal_list_t *attrs);

int orte_ns_replica_get_job_descendants(orte_jobid_t **descendants, orte_std_cntr_t *num_desc, orte_jobid_t job);

int orte_ns_replica_get_job_children(orte_jobid_t **descendants, orte_std_cntr_t *num_desc, orte_jobid_t job);

int orte_ns_replica_get_root_job(orte_jobid_t *root_job, orte_jobid_t job);

int orte_ns_replica_get_parent_job(orte_jobid_t *parent, orte_jobid_t job);

int orte_ns_replica_reserve_range(orte_jobid_t job,
                                  orte_vpid_t range,
                                  orte_vpid_t *startvpid);

/*
 * GENERAL FUNCTIONS
 */
int orte_ns_replica_get_peers(orte_process_name_t **procs, 
                              orte_std_cntr_t *num_procs, opal_list_t *attrs);

int orte_ns_replica_assign_rml_tag(orte_rml_tag_t *tag,
                                   char *name);


int orte_ns_replica_define_data_type(const char *name,
                                     orte_data_type_t *type);

int orte_ns_replica_create_my_name(void);


/*
 * DIAGNOSTIC FUNCTIONS
 */
int orte_ns_replica_dump_cells(void);
int orte_ns_replica_dump_cells_fn(orte_buffer_t *buffer);

int orte_ns_replica_dump_jobs(void);
int orte_ns_replica_dump_jobs_fn(orte_buffer_t *buffer);

int orte_ns_replica_dump_tags(void);
int orte_ns_replica_dump_tags_fn(orte_buffer_t *buffer);

int orte_ns_replica_dump_datatypes(void);
int orte_ns_replica_dump_datatypes_fn(orte_buffer_t *buffer);


/*
 * INTERNAL SUPPORT FUNCTIONS
 */

/* find a job's record, wherever it may be located on the list of job families.
 * this function searches the entire list of job families, traversing the list
 * of all jobs in each family, until it finds the specified job. It then returns
 * a pointer to the that job's info structure. It returns
 * NULL (without error_logging an error) if no record is found
 */
orte_ns_replica_jobitem_t* orte_ns_replica_find_job(orte_jobid_t job);

/* find the root job for the specified job.
 * this function searches the entire list of job families, traversing the list
 * of all jobs in each family, until it finds the specified job. It then returns
 * a pointer to the root job's info structure for that job family. It returns
 * NULL (without error_logging an error) if no record is found
 */
orte_ns_replica_jobitem_t* orte_ns_replica_find_root_job(orte_jobid_t job);

/* find a job's record on a specified root's family tree.
 * this function finds the family record for the specified root job. It then
 * traverses the children of that root until it finds the specified job, and then
 * returns a pointer to that job's info structure. If root=jobid, then it will
 * return a pointer to the root job's info structure. It returns
 * NULL (without error_logging an error) if no record is found
 */
orte_ns_replica_jobitem_t* orte_ns_replica_search_job_family_tree(orte_jobid_t root, orte_jobid_t jobid);

/* given a job's record, create a flattened list of descendants below it */
void orte_ns_replica_construct_flattened_tree(opal_list_t *tree, orte_ns_replica_jobitem_t *ptr);

/* search down a tree, following all the children's branches, to find the specified
 * job. Return a pointer to that object, and a pointer to the parent object
 * This function is called recursively, so it passes into it the ptr to the
 * current object being looked at
 */
orte_ns_replica_jobitem_t *down_search(orte_ns_replica_jobitem_t *ptr,
                                       orte_ns_replica_jobitem_t **parent_ptr,
                                       orte_jobid_t job);

ORTE_MODULE_DECLSPEC extern mca_ns_base_component_t mca_ns_replica_component;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
