/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#ifndef NS_REPLICA_H
#define NS_REPLICA_H

#include "include/types.h"

#include "ompi_config.h"
#include "mca/ns/ns.h"

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
char* ns_replica_get_proc_name_string(ompi_process_name_t* name);

/*
 * Implementation of get_vpid_string()
 */
char* ns_replica_get_vpid_string(ompi_process_name_t* name);

/*
 * Implementation of get_jobid_string()
 */
char* ns_replica_get_jobid_string(ompi_process_name_t* name);

/*
 * Implementation of get_cellid_string()
 */
char* ns_replica_get_cellid_string(ompi_process_name_t* name);

/*
 * Implementation of get_vpid()
 */
ompi_process_id_t ns_replica_get_vpid(ompi_process_name_t* name);

/*
 * Implementation of get_jobid()
 */
ompi_process_id_t ns_replica_get_jobid(ompi_process_name_t* name);

/*
 * Implementation of get_cellid()
 */
ompi_process_id_t ns_replica_get_cellid(ompi_process_name_t* name);

/*
 * Implementation of compare()
 */
int ns_replica_compare(ompi_ns_cmp_bitmask_t fields,
		       ompi_process_name_t* name1,
		       ompi_process_name_t* name2);

#endif
