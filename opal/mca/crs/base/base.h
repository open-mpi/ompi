/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OPAL_CRS_BASE_H
#define OPAL_CRS_BASE_H

#include "opal_config.h"
#include "opal/mca/crs/crs.h"
#include "opal/util/opal_environ.h"

/*
 * Global functions for MCA overall CRS
 */

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Initialize the CRS MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR   Upon failures
     * 
     * This function is invoked during opal_init();
     */
    OPAL_DECLSPEC int opal_crs_base_open(void);
    
    /**
     * Select an available component.
     *
     * @retval OPAL_SUCCESS Upon Success
     * @retval OPAL_NOT_FOUND If no component can be selected
     * @retval OPAL_ERROR Upon other failure
     *
     */
    OPAL_DECLSPEC int opal_crs_base_select(void);
    
    /**
     * Finalize the CRS MCA framework
     *
     * @retval OPAL_SUCCESS Upon success
     * @retval OPAL_ERROR   Upon failures
     * 
     * This function is invoked during opal_finalize();
     */
    OPAL_DECLSPEC int opal_crs_base_close(void);

    /**
     * Globals
     */
#define opal_crs_base_metadata_filename (strdup("snapshot_meta.data"))

    OPAL_DECLSPEC extern int  opal_crs_base_output;
    OPAL_DECLSPEC extern opal_list_t opal_crs_base_components_available;
    OPAL_DECLSPEC extern opal_crs_base_component_t opal_crs_base_selected_component;
    OPAL_DECLSPEC extern opal_crs_base_module_t opal_crs;
    OPAL_DECLSPEC extern char * opal_crs_base_snapshot_dir;

    /**
     * 'None' component functions
     * These are to be used when no component is selected.
     * They just return success, and empty strings as necessary.
     */
    int opal_crs_base_none_open(void);
    int opal_crs_base_none_close(void);
    int opal_crs_base_none_query(mca_base_module_t **module, int *priority);

    int opal_crs_base_none_module_init(void);
    int opal_crs_base_none_module_finalize(void);

    int opal_crs_base_none_checkpoint(    pid_t pid, opal_crs_base_snapshot_t *sanpshot, opal_crs_state_type_t *state);

    int opal_crs_base_none_restart(    opal_crs_base_snapshot_t *snapshot, bool spawn_child, pid_t *child_pid);

    int opal_crs_base_none_disable_checkpoint(void);
    int opal_crs_base_none_enable_checkpoint(void);
    
    OPAL_DECLSPEC int opal_crs_base_none_prelaunch(int32_t rank,
                                                   char *base_snapshot_dir,
                                                   char **app,
                                                   char **cwd,
                                                   char ***argv,
                                                   char ***env);
    OPAL_DECLSPEC int opal_crs_base_none_reg_thread(void);

    /**
     * Some utility functions
     */
    OPAL_DECLSPEC char * opal_crs_base_state_str(opal_crs_state_type_t state);

    OPAL_DECLSPEC char * opal_crs_base_unique_snapshot_name(pid_t pid);
    OPAL_DECLSPEC char * opal_crs_base_extract_expected_component(char *snapshot_loc, int *prev_pid);
    OPAL_DECLSPEC int    opal_crs_base_init_snapshot_directory(opal_crs_base_snapshot_t *snapshot);
    OPAL_DECLSPEC char * opal_crs_base_get_snapshot_directory(char *uniq_snapshot_name);

    /* Opens the metadata file and places all the base information in the file.
     * Options:
     *  'w' = Open for writing
     *  'a' = Open for writing and appending information
     */
    OPAL_DECLSPEC FILE *opal_crs_base_open_metadata(opal_crs_base_snapshot_t *snapshot, char mode );

    /* Open the metadata file, read off the base information and 
     * return the component and previous pid to the caller.
     * Note: component is allocated inside this function, it is the
     *       callers responsibility to free this memory.
     */
    OPAL_DECLSPEC FILE * opal_crs_base_open_read_metadata(char *location, char **component, int *prev_pid);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OPAL_CRS_BASE_H */
