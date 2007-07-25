/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_RML_BASE_H
#define MCA_RML_BASE_H

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "orte/mca/rml/rml.h"
#include "orte/dss/dss_types.h"

BEGIN_C_DECLS

/*
 * Global functions for the RML
 */

ORTE_DECLSPEC int orte_rml_base_open(void);
ORTE_DECLSPEC int orte_rml_base_select(void);
ORTE_DECLSPEC int orte_rml_base_close(void);


/*
 * Data type support
 */
ORTE_DECLSPEC int orte_rml_base_compare_tags(orte_rml_tag_t *value1, 
                                             orte_rml_tag_t *value2, 
                                             orte_data_type_t type);
ORTE_DECLSPEC int orte_rml_base_copy_tag(orte_rml_tag_t **dest, 
                                         orte_rml_tag_t *src, 
                                         orte_data_type_t type);
ORTE_DECLSPEC int orte_rml_base_pack_tag(orte_buffer_t *buffer, 
                                         const void *src,
                                         orte_std_cntr_t num_vals, 
                                         orte_data_type_t type);
ORTE_DECLSPEC int orte_rml_base_print_tag(char **output,
                                          char *prefix, 
                                          orte_rml_tag_t *src, 
                                          orte_data_type_t type);
ORTE_DECLSPEC void orte_rml_base_std_obj_release(orte_data_value_t *value);
ORTE_DECLSPEC int orte_rml_base_size_tag(size_t *size, 
                                         orte_rml_tag_t *src,
                                         orte_data_type_t type);
ORTE_DECLSPEC int orte_rml_base_unpack_tag(orte_buffer_t *buffer,
                                           void *dest,
                                           orte_std_cntr_t *num_vals,
                                           orte_data_type_t type);
/*
 * Internal functions
 */
int orte_rml_base_comm_start(void);
int orte_rml_base_comm_stop(void);
void orte_rml_base_recv(int status, orte_process_name_t* sender,
                        orte_buffer_t* buffer, orte_rml_tag_t tag,
                        void* cbdata);

ORTE_DECLSPEC extern int orte_rml_base_output;
ORTE_DECLSPEC extern opal_list_t orte_rml_base_components;

/* For FT only, please don't use */
ORTE_DECLSPEC extern orte_rml_component_t *orte_rml_component;

/*
 * This is the base priority for a RML wrapper component
 * If there exists more than one wrapper, then the one with 
 * the lowest priority wins.
 */
#define RML_SELECT_WRAPPER_PRIORITY -128

END_C_DECLS

#endif /* MCA_RML_BASE_H */
