/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_SBGP_BASE_H
#define MCA_SBGP_BASE_H

#include "ompi_config.h"

#include "opal/mca/mca.h"

/*
 * Global functions for SBGP
 */

/* components found */
OMPI_MODULE_DECLSPEC extern opal_list_t mca_sbgp_base_components_opened;
/* components in use */
OMPI_MODULE_DECLSPEC extern opal_list_t mca_sbgp_base_components_in_use;
OMPI_MODULE_DECLSPEC extern int mca_sbgp_base_components_in_use_inited;
OMPI_DECLSPEC extern char *ompi_sbgp_subgroups_string;

extern int mca_sbgp_base_output;

BEGIN_C_DECLS

OMPI_DECLSPEC int mca_sbgp_base_open(void);

OMPI_DECLSPEC int mca_sbgp_base_init(bool, bool);

OMPI_DECLSPEC int mca_sbgp_base_close(void);

/* subgrouping component and key value */
struct sbgp_base_component_keyval_t {
    mca_base_component_list_item_t component;
    char *key_value;
};
typedef struct sbgp_base_component_keyval_t sbgp_base_component_keyval_t;
OBJ_CLASS_DECLARATION(sbgp_base_component_keyval_t);

END_C_DECLS

#endif /* MCA_SBGP_BASE_H */
