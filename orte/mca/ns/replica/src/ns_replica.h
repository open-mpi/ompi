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
#include "include/types.h"
#include "include/orte_constants.h"
#include "opal/threads/mutex.h"
#include "opal/class/opal_list.h"
#include "dps/dps.h"
#include "mca/oob/oob_types.h"
#include "mca/ns/base/base.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* list class for tracking cellid's
 */
struct orte_ns_replica_cell_tracker_t {
    opal_list_item_t item;
    orte_cellid_t cell;
    char *site;
    char *resource;
};
typedef struct orte_ns_replica_cell_tracker_t orte_ns_replica_cell_tracker_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_cell_tracker_t);


/*
 * list class for tracking vpids/jobid
 * This structure is used to create a linked list of jobid-max vpid pairs. Basically, we
 * are tracking the max used vpid for each jobid that has been created.
 */
struct orte_ns_replica_name_tracker_t {
    opal_list_item_t item;  /**< Allows this item to be placed on a list */
    orte_jobid_t job;  /**< Job id */
    orte_vpid_t last_used_vpid;      /**< Tracks the vpid last given out */
};
typedef struct orte_ns_replica_name_tracker_t orte_ns_replica_name_tracker_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_name_tracker_t);

struct orte_ns_replica_tagitem_t {
    opal_list_item_t item;  /**< Allows this item to be placed on a list */
    orte_rml_tag_t tag;  /**< OOB tag */
    char *name;      /**< Name associated with tag */
};
typedef struct orte_ns_replica_tagitem_t orte_ns_replica_tagitem_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_tagitem_t);

struct orte_ns_replica_dti_t {
    opal_list_item_t item;  /**< Allows this item to be placed on a list */
    orte_data_type_t id;  /**< data type id */
    char *name;      /**< Name associated with data type */
};
typedef struct orte_ns_replica_dti_t orte_ns_replica_dti_t;

OBJ_CLASS_DECLARATION(orte_ns_replica_dti_t);

/*
 * globals needed within component
 */
extern orte_cellid_t orte_ns_replica_next_cellid;
extern orte_jobid_t orte_ns_replica_next_jobid;
extern opal_list_t orte_ns_replica_cell_tracker;
extern opal_list_t orte_ns_replica_name_tracker;
extern orte_rml_tag_t orte_ns_replica_next_rml_tag;
extern orte_data_type_t orte_ns_replica_next_dti;
extern opal_list_t orte_ns_replica_taglist;
extern opal_list_t orte_ns_replica_dtlist;
extern int orte_ns_replica_debug;
extern opal_mutex_t orte_ns_replica_mutex;

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
