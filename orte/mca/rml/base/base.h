/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/*
 * Global functions for the RML
 */

ORTE_DECLSPEC int orte_rml_base_open(void);
ORTE_DECLSPEC int orte_rml_base_select(void);
ORTE_DECLSPEC int orte_rml_base_close(void);

/*
 * Global struct holding the base parameters.
 */
struct orte_rml_base_t {
    int rml_output;
    int rml_debug;
    opal_list_t rml_components;
};
typedef struct orte_rml_base_t orte_rml_base_t;

ORTE_DECLSPEC extern orte_rml_base_t orte_rml_base;
ORTE_DECLSPEC extern orte_rml_module_t orte_rml;
ORTE_DECLSPEC extern orte_process_name_t orte_rml_name_any;
ORTE_DECLSPEC extern orte_process_name_t orte_rml_name_seed;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_RML_BASE_H */
