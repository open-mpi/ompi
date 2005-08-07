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
 *
 */
#ifndef NS_REPLICA_H
#define NS_REPLICA_H

#include "orte_config.h"
#include "orte/include/orte_types.h"
#include "include/orte_constants.h"
#include "opal/threads/mutex.h"
#include "opal/class/opal_object.h"
#include "orte/class/orte_pointer_array.h"
#include "orte/dps/dps.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/ns/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* list class for tracking cellid's
 */
struct orte_ns_replica_cell_tracker_t {
    opal_object_t super;
    orte_cellid_t cell;
    char *site;
    char *resource;
};
typedef struct orte_ns_replica_cell_tracker_t orte_ns_replica_cell_tracker_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_cell_tracker_t);


/*
 * object for tracking vpids/jobids
 * This structure is used to track jobid-max vpid pairs. Basically, we
 * are tracking the max used vpid for each jobid that has been created.
 */
struct orte_ns_replica_jobid_tracker_t {
    opal_object_t super;
    orte_jobid_t jobid;  /**< Job id */
    orte_vpid_t next_vpid;
};
typedef struct orte_ns_replica_jobid_tracker_t orte_ns_replica_jobid_tracker_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_jobid_tracker_t);

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
#if 0
    orte_jobgrp_t num_jobgrps;
    orte_pointer_array_t *jobgrps;
#endif
    orte_jobid_t num_jobids;
    orte_pointer_array_t *jobids;
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
 * Implementation of create_cellid().
 */
int orte_ns_replica_create_cellid(orte_cellid_t *cellid, char *site, char *resource);

/*
 * Implementation of get_cell_info()
 */
int orte_ns_replica_get_cell_info(orte_cellid_t cellid,
                                char **site, char **resource);

/*
 * Implementation of create_jobid().
 */
int orte_ns_replica_create_jobid(orte_jobid_t *jobid);


/*
 * Implementation of reserve_range()
 */
int orte_ns_replica_reserve_range(orte_jobid_t job,
                                  orte_vpid_t range,
                                  orte_vpid_t *startvpid);

/*
 * Peer functions
 */
int orte_ns_replica_get_job_peers(orte_process_name_t **procs, 
                                  size_t *num_procs, orte_jobid_t job);


/*
 * Diagnostic functions
 */
int orte_ns_replica_dump_cells(int output_id);
int orte_ns_replica_dump_cells_fn(orte_buffer_t *buffer);

int orte_ns_replica_dump_jobs(int output_id);
int orte_ns_replica_dump_jobs_fn(orte_buffer_t *buffer);

int orte_ns_replica_dump_tags(int output_id);
int orte_ns_replica_dump_tags_fn(orte_buffer_t *buffer);

int orte_ns_replica_dump_datatypes(int output_id);
int orte_ns_replica_dump_datatypes_fn(orte_buffer_t *buffer);


/*
 * Implementation of assign rml tag
 */
int orte_ns_replica_assign_rml_tag(orte_rml_tag_t *tag,
                                   char *name);


int orte_ns_replica_define_data_type(const char *name,
                                     orte_data_type_t *type);

int orte_ns_replica_create_my_name(void);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
