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
#include "orte_config.h"
#include "include/orte_constants.h"

#include "class/ompi_list.h"
#include "mca/mca.h"
#include "dps/dps_types.h"

#include "mca/ns/ns.h"


/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Internal definitions
 */
/*
 * define the command names/ids for use in OOB buffers.
 * only needed for remotely executed commands.
 */
#define ORTE_NS_CREATE_CELLID     0x01
#define ORTE_NS_CREATE_JOBID      0x02
#define ORTE_NS_RESERVE_RANGE     0x04
#define ORTE_NS_FREE_NAME         0x08
#define ORTE_NS_GET_MY_CELLID     0x10

typedef uint8_t orte_ns_cmd_bitmask_t;

/*
 * packing type definitions
 */
/* CAUTION - any changes here must also change corresponding
 * typedefs above
 */
#define ORTE_NS_OOB_PACK_JOBID   ORTE_INT32
#define ORTE_NS_OOB_PACK_CELLID  ORTE_INT32
#define ORTE_NS_OOB_PACK_VPID    ORTE_INT32
#define ORTE_NS_OOB_PACK_CMD     ORTE_INT16
#define ORTE_NS_OOB_PACK_OOB_TAG ORTE_INT32

/*
 * define flag values for remote commands - only used internally
 */
#define ORTE_NS_CREATE_CELLID_CMD   0x01
#define ORTE_NS_CREATE_JOBID_CMD    0x02
#define ORTE_NS_RESERVE_RANGE_CMD   0x04
#define ORTE_NS_ASSIGN_OOB_TAG_CMD  0x08

/*
 * function definitions
 */
OMPI_DECLSPEC    int orte_ns_base_open(void);
OMPI_DECLSPEC    int orte_ns_base_select(void);
OMPI_DECLSPEC    int orte_ns_base_close(void);

    /*
     * Base functions that are common to all implementations - can be overridden
     */

OMPI_DECLSPEC    int orte_ns_base_assign_cellid_to_process(orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_create_process_name(orte_process_name_t **name,
                                  orte_cellid_t cell,
                                  orte_jobid_t job,
                                  orte_vpid_t vpid);

OMPI_DECLSPEC    int orte_ns_base_copy_process_name(orte_process_name_t **dest,
                                orte_process_name_t* src);

OMPI_DECLSPEC    int orte_ns_base_convert_string_to_process_name(orte_process_name_t **name,
                                             const char* name_string);

OMPI_DECLSPEC    int orte_ns_base_get_proc_name_string(char **name_string,
                                   const orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_get_vpid_string(char **vpid_string, const orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_convert_vpid_to_string(char **vpid_string, const orte_vpid_t vpid);

OMPI_DECLSPEC    int orte_ns_base_convert_string_to_vpid(orte_vpid_t *vpid, const char* vpidstring);

OMPI_DECLSPEC    int orte_ns_base_get_jobid_string(char **jobid_string, const orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_convert_jobid_to_string(char **jobid_string, const orte_jobid_t jobid);

OMPI_DECLSPEC    int orte_ns_base_convert_string_to_jobid(orte_jobid_t *jobid, const char* jobidstring);

OMPI_DECLSPEC    int orte_ns_base_get_cellid_string(char **cellid_string, const orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_convert_string_to_cellid(orte_cellid_t *cellid, const char *cellidstring);

OMPI_DECLSPEC    int orte_ns_base_convert_cellid_to_string(char **cellid_string, const orte_cellid_t cellid);

OMPI_DECLSPEC    int orte_ns_base_get_vpid(orte_vpid_t *vpid, const orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_get_jobid(orte_jobid_t *jobid, const orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_get_cellid(orte_cellid_t *cellid, const orte_process_name_t* name);

OMPI_DECLSPEC    int orte_ns_base_compare(orte_ns_cmp_bitmask_t fields,
			const orte_process_name_t* name1,
			const orte_process_name_t* name2);

OMPI_DECLSPEC    int orte_ns_base_free_name(orte_process_name_t **name);

OMPI_DECLSPEC    int orte_ns_base_module_init_not_available(void);

OMPI_DECLSPEC    int orte_ns_base_create_cellid_not_available(orte_cellid_t *cellid);

OMPI_DECLSPEC    int orte_ns_base_create_jobid_not_available(orte_jobid_t *jobid);

OMPI_DECLSPEC    int orte_ns_base_get_vpid_range_not_available(orte_jobid_t job,
                                                               orte_vpid_t range,
                                                               orte_vpid_t *startvpid);

OMPI_DECLSPEC    int orte_ns_base_derive_vpid(orte_vpid_t *vpid,
                                             orte_vpid_t base_vpid,
                                             int offset);

OMPI_DECLSPEC    int orte_ns_base_assign_rml_tag_not_available(orte_rml_tag_t *tag, char *name);

OMPI_DECLSPEC    int orte_ns_base_set_my_name(void);

OMPI_DECLSPEC    int orte_ns_base_get_peers(orte_process_name_t **procs, 
                                  size_t *num_procs, size_t *self);
/*
 * globals that might be needed
 */

OMPI_DECLSPEC extern int mca_ns_base_output;
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
