/* -*- C -*-
 * 
 * $HEADER$
 *
 */
#ifndef NS_PROXY_H
#define NS_PROXY_H


#include "ompi_config.h"
#include "include/types.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "mca/ns/ns.h"

/*
 * Module open / close
 */
int mca_ns_proxy_open(void);
int mca_ns_proxy_close(void);


/*
 * Startup / Shutdown
 */
mca_ns_t* mca_ns_proxy_init(bool *allow_multi_user_threads, bool *have_hidden_threads, int *priority);
int mca_ns_proxy_finalize(void);

/*
 * Implementation of create_cellid().
 */
ompi_process_id_t ns_proxy_create_cellid(void);

/*
 * Implementation of create_jobid().
 */
ompi_process_id_t ns_proxy_create_jobid(void);

/*
 * Implementation of create_process_name()
 */
ompi_process_name_t* ns_proxy_create_process_name(
			    ompi_process_id_t cell,
			    ompi_process_id_t job,
			    ompi_process_id_t vpid);

/*
 * Implementation of reserve_range()
 */
ompi_process_id_t ns_proxy_reserve_range(
					   ompi_process_id_t job,
					   ompi_process_id_t range);

/*
 * Implementation of free_name()
 */
int ns_proxy_free_name(ompi_process_name_t* name);

/*
 * Implementation of get_proc_name_string()
 */
char* ns_proxy_get_proc_name_string(const ompi_process_name_t* name);

/*
 * Implementation of get_vpid_string()
 */
char* ns_proxy_get_vpid_string(const ompi_process_name_t* name);

/*
 * Implementation of get_jobid_string()
 */
char* ns_proxy_get_jobid_string(const ompi_process_name_t* name);

/*
 * Implementation of get_cellid_string()
 */
char* ns_proxy_get_cellid_string(const ompi_process_name_t* name);

/*
 * Implementation of get_vpid()
 */
ompi_process_id_t ns_proxy_get_vpid(const ompi_process_name_t* name);

/*
 * Implementation of get_jobid()
 */
ompi_process_id_t ns_proxy_get_jobid(const ompi_process_name_t* name);

/*
 * Implementation of get_cellid()
 */
ompi_process_id_t ns_proxy_get_cellid(const ompi_process_name_t* name);

/*
 * Implementation of compare()
 */
int ns_proxy_compare(ompi_ns_cmp_bitmask_t fields,
		       const ompi_process_name_t* name1,
		       const ompi_process_name_t* name2);

#endif
