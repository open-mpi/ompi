/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_SPML_BASE_H
#define MCA_SPML_BASE_H

#include "oshmem_config.h"
#include "opal/mca/mca.h"
#include "oshmem/mca/spml/spml.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

/*
 * Global functions for the PML
 */

BEGIN_C_DECLS

/*
 * This is the base priority for a SPML wrapper component
 * If there exists more than one then it is undefined 
 * which one is picked.
 */
#define SPML_SELECT_WRAPPER_PRIORITY -128

OSHMEM_DECLSPEC int mca_spml_base_open(void);
OSHMEM_DECLSPEC int mca_spml_base_close(void);
OSHMEM_DECLSPEC int mca_spml_base_finalize(void);
OSHMEM_DECLSPEC int mca_spml_base_select(bool, bool);

/* share in modex the name of the selected component */
OSHMEM_DECLSPEC int mca_spml_base_spml_selected(const char *name);

/* TODO: Re-write for spml */
/* verify that all new procs are using the currently selected component */
OSHMEM_DECLSPEC int mca_spml_base_spml_check_selected(const char *my_spml,
                                                    oshmem_proc_t **procs,
                                                    size_t nprocs);


OSHMEM_DECLSPEC int mca_spml_base_wait(void* addr, int cmp, void* value, int datatype);
OSHMEM_DECLSPEC int mca_spml_base_wait_nb(void* handle);
OSHMEM_DECLSPEC int mca_spml_base_oob_get_mkeys(int pe, uint32_t seg, mca_spml_mkey_t *mkeys);
/*
 * Globals
 */
OSHMEM_DECLSPEC extern int mca_spml_base_output;
OSHMEM_DECLSPEC extern opal_list_t mca_spml_base_components_available;
OSHMEM_DECLSPEC extern mca_spml_base_component_t mca_spml_base_selected_component;
OSHMEM_DECLSPEC extern mca_spml_base_module_t mca_spml;
OSHMEM_DECLSPEC extern opal_pointer_array_t mca_spml_base_spml;
/*----------------------------------------------------------------------------------*/
/*logger macros*/

#ifdef __BASE_FILE__
#define __SPML_FILE__ __BASE_FILE__
#else
#define __SPML_FILE__ __FILE__
#endif

#define SPML_VERBOSE(level, format, ...) \
	    opal_output_verbose(level, mca_spml_base_output, "%s:%d - %s() " format, \
				                        __SPML_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)

#define SPML_ERROR(format, ... ) \
	    opal_output_verbose(0, mca_spml_base_output, "Error: %s:%d - %s() " format, \
				                        __SPML_FILE__, __LINE__, __FUNCTION__, ## __VA_ARGS__)


END_C_DECLS

#endif /* MCA_SPML_BASE_H */
