/*
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_NS_BASE_H
#define MCA_NS_BASE_H

/*
 * includes
 */
#include "ompi_config.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/ns/ns.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
  int mca_ns_base_open(void);
  int mca_ns_base_select(bool *allow_multi_user_threads,
                         bool *have_hidden_threads);
  int mca_ns_base_close(void);

/*
 * Base functions that are common to all implementations - can be overridden
 */

  ompi_process_name_t* ns_base_create_process_name(ompi_process_id_t cell,
                                                   ompi_process_id_t job,
                                                   ompi_process_id_t vpid);

  char* ns_base_get_proc_name_string(const ompi_process_name_t* name);

  char* ns_base_get_vpid_string(const ompi_process_name_t* name);

  char* ns_base_get_jobid_string(const ompi_process_name_t* name);

  char* ns_base_get_cellid_string(const ompi_process_name_t* name);

  ompi_process_id_t ns_base_get_vpid(const ompi_process_name_t* name);

  ompi_process_id_t ns_base_get_jobid(const ompi_process_name_t* name);

  ompi_process_id_t ns_base_get_cellid(const ompi_process_name_t* name);

  int ns_base_compare(ompi_ns_cmp_bitmask_t fields,
                      const ompi_process_name_t* name1,
                      const ompi_process_name_t* name2);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


/*
 * globals that might be needed
 */

extern int mca_ns_base_output;
extern mca_ns_t ompi_name_server;  /* holds selected module's function pointers */
extern bool mca_ns_base_selected;
extern ompi_list_t mca_ns_base_components_available;
extern mca_ns_base_component_t mca_ns_base_selected_component;

/*
 * external API functions will be documented in the mca/ns/ns.h file
 */

#endif
