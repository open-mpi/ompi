/*
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


#define OMPI_NAME_ARGS(n)  (n).cellid,(n).jobid,(n).vpid


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
    /** List of names for general use
     */
    struct ompi_name_server_namelist_t {
	ompi_list_item_t item;     /**< Allows this item to be placed on a list */
	ompi_process_name_t *name;  /**< Name of a process */
    };
    typedef struct ompi_name_server_namelist_t ompi_name_server_namelist_t;

OMPI_DECLSPEC    OBJ_CLASS_DECLARATION(ompi_name_server_namelist_t);

OMPI_DECLSPEC    int mca_ns_base_open(void);
OMPI_DECLSPEC    int mca_ns_base_select(bool *allow_multi_user_threads,
			   bool *have_hidden_threads);
OMPI_DECLSPEC    int mca_ns_base_close(void);

    /*
     * Base functions that are common to all implementations - can be overridden
     */

OMPI_DECLSPEC    int mca_ns_base_assign_cellid_to_process(ompi_process_name_t* name);

OMPI_DECLSPEC    ompi_process_name_t* mca_ns_base_create_process_name(mca_ns_base_cellid_t cell,
						     mca_ns_base_jobid_t job,
						     mca_ns_base_vpid_t vpid);

OMPI_DECLSPEC    ompi_process_name_t* mca_ns_base_copy_process_name(ompi_process_name_t* name);

OMPI_DECLSPEC    ompi_process_name_t* mca_ns_base_convert_string_to_process_name(const char* name);

OMPI_DECLSPEC    char* mca_ns_base_get_proc_name_string(const ompi_process_name_t* name);

OMPI_DECLSPEC    char* mca_ns_base_get_vpid_string(const ompi_process_name_t* name);

OMPI_DECLSPEC    char* mca_ns_base_get_jobid_string(const ompi_process_name_t* name);

OMPI_DECLSPEC    char* mca_ns_base_convert_jobid_to_string(const mca_ns_base_jobid_t jobid);

OMPI_DECLSPEC    mca_ns_base_jobid_t mca_ns_base_convert_string_to_jobid(const char* jobid_string);

OMPI_DECLSPEC    char* mca_ns_base_get_cellid_string(const ompi_process_name_t* name);

OMPI_DECLSPEC    mca_ns_base_vpid_t mca_ns_base_get_vpid(const ompi_process_name_t* name);

OMPI_DECLSPEC    mca_ns_base_jobid_t mca_ns_base_get_jobid(const ompi_process_name_t* name);

OMPI_DECLSPEC    mca_ns_base_cellid_t mca_ns_base_get_cellid(const ompi_process_name_t* name);

OMPI_DECLSPEC    int mca_ns_base_compare(ompi_ns_cmp_bitmask_t fields,
			const ompi_process_name_t* name1,
			const ompi_process_name_t* name2);

OMPI_DECLSPEC    int mca_ns_base_pack_name(void *dest, void *src, int n);

OMPI_DECLSPEC    int mca_ns_base_unpack_name(void *dest, void *src, int n);

OMPI_DECLSPEC    int mca_ns_base_pack_jobid(void *dest, void *src, int n);

OMPI_DECLSPEC    int mca_ns_base_unpack_jobid(void *dest, void *src, int n);

OMPI_DECLSPEC    mca_ns_base_cellid_t mca_ns_base_create_cellid(void);

OMPI_DECLSPEC    mca_ns_base_jobid_t mca_ns_base_create_jobid(void);

OMPI_DECLSPEC    mca_ns_base_vpid_t mca_ns_base_reserve_range(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range);

OMPI_DECLSPEC    int mca_ns_base_free_name(ompi_process_name_t* name);



/*
 * globals that might be needed
 */

OMPI_DECLSPEC extern int mca_ns_base_output;
OMPI_DECLSPEC extern mca_ns_base_module_t ompi_name_server;  /* holds selected module's function pointers */
OMPI_DECLSPEC extern bool mca_ns_base_selected;
OMPI_DECLSPEC extern ompi_list_t mca_ns_base_components_available;
OMPI_DECLSPEC extern mca_ns_base_component_t mca_ns_base_selected_component;

/*
 * external API functions will be documented in the mca/ns/ns.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
