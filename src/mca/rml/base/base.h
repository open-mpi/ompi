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

#ifndef MCA_RML_BASE_H
#define MCA_RML_BASE_H

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/rml/rml.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/*
 * Global functions for the RML
 */

OMPI_DECLSPEC int orte_rml_base_open(void);
OMPI_DECLSPEC int orte_rml_base_select(void);
OMPI_DECLSPEC int orte_rml_base_close(void);

/*
 * Global struct holding the base parameters.
 */
struct orte_rml_base_t {
    int rml_output;
    int rml_debug;
    ompi_list_t rml_components;
};
typedef struct orte_rml_base_t orte_rml_base_t;

OMPI_DECLSPEC extern orte_rml_base_t orte_rml_base;
OMPI_DECLSPEC extern orte_rml_module_t orte_rml;
OMPI_DECLSPEC extern orte_process_name_t orte_rml_name_any;
OMPI_DECLSPEC extern orte_process_name_t orte_rml_name_seed;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_RML_BASE_H */
