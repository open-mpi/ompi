/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#ifndef NS_REPLICA_H
#define NS_REPLICA_H


#include "ompi_config.h"
#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "mca/ns/ns.h"

/*
 * list class for tracking vpids/jobid
 * This structure is used to create a linked list of jobid-max vpid pairs. Basically, we
 * are tracking the max used vpid for each jobid that has been created.
 */
struct ompi_name_tracker_t {
    ompi_list_item_t item;  /**< Allows this item to be placed on a list */
    ompi_process_id_t job;  /**< Job id */
    ompi_process_id_t last_used_vpid;      /**< Tracks the vpid last given out */
};
typedef struct ompi_name_tracker_t ompi_name_tracker_t;

OBJ_CLASS_DECLARATION(ompi_name_tracker_t);

/*
 * globals needed within component
 */
extern ompi_process_id_t max_used_cellid;
extern ompi_process_id_t max_used_jobid;
extern ompi_name_tracker_t ompi_name_tracker;

/*
 * Module open / close
 */
int mca_ns_replica_open(void);
int mca_ns_replica_close(void);


/*
 * Startup / Shutdown
 */
mca_ns_t* mca_ns_replica_init(bool *allow_multi_user_threads, bool *have_hidden_threads);
int mca_ns_replica_finalize(void);

/*
 * Implementation of create_cellid().
 */
ompi_process_id_t ns_replica_create_cellid(void);

/*
 * Implementation of create_jobid().
 */
ompi_process_id_t ns_replica_create_jobid(void);

/*
 * Implementation of create_process_name()
 */
ompi_process_name_t* ns_replica_create_process_name(
			    ompi_process_id_t cell,
			    ompi_process_id_t job,
			    ompi_process_id_t vpid);

/*
 * Implementation of reserve_range()
 */
ompi_process_id_t ns_replica_reserve_range(
					   ompi_process_id_t job,
					   ompi_process_id_t range);

/*
 * Implementation of free_name()
 */
int ns_replica_free_name(ompi_process_name_t* name);

/*
 * Implementation of get_proc_name_string()
 */
char* ns_replica_get_proc_name_string(const ompi_process_name_t* name);

/*
 * Implementation of get_vpid_string()
 */
char* ns_replica_get_vpid_string(const ompi_process_name_t* name);

/*
 * Implementation of get_jobid_string()
 */
char* ns_replica_get_jobid_string(const ompi_process_name_t* name);

/*
 * Implementation of get_cellid_string()
 */
char* ns_replica_get_cellid_string(const ompi_process_name_t* name);

/*
 * Implementation of get_vpid()
 */
ompi_process_id_t ns_replica_get_vpid(const ompi_process_name_t* name);

/*
 * Implementation of get_jobid()
 */
ompi_process_id_t ns_replica_get_jobid(const ompi_process_name_t* name);

/*
 * Implementation of get_cellid()
 */
ompi_process_id_t ns_replica_get_cellid(const ompi_process_name_t* name);

/*
 * Implementation of compare()
 */
int ns_replica_compare(ompi_ns_cmp_bitmask_t fields,
		       const ompi_process_name_t* name1,
		       const ompi_process_name_t* name2);

#endif
