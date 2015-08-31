/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_TOPO_UNTIY_H
#define MCA_TOPO_UNTIY_H

#include "ompi_config.h"
#include "ompi/mca/topo/topo.h"

/*
 * ******************************************************************
 * ******** functions which provide MCA interface comppliance *******
 * ******************************************************************
 * These functions are:
 *       - mca_topo_example_module_open
 *       - mca_topo_example_module_close
 *       - mca_topo_example_module_query
 *       - mca_topo_example_module_finalize
 * These functions are always found on the mca_topo_example_module
 * structure. They are the "meta" functions to ensure smooth op.
 * ******************************************************************
 */
BEGIN_C_DECLS

/*
 * Public component instance
 */
OMPI_MODULE_DECLSPEC extern mca_topo_base_component_2_2_0_t
    mca_topo_example_component;

/*
 * A unique module class for the module so that we can both cache
 * module-specific information on the module and have a
 * module-specific constructor and destructor.
 */
typedef struct {
    mca_topo_base_module_t super;

    /* Modules can add their own information here */
    int example_module_specific_data;
} mca_topo_example_module_t;

OBJ_CLASS_DECLARATION(mca_topo_example_module_t);


/*
 * Module functions
 */

int mca_topo_example_cart_map(struct ompi_communicator_t *comm,
                              int ndims,
                              int *dims,
                              int *periods,
                              int *newrank);

int mca_topo_example_graph_map(struct ompi_communicator_t *comm,
                               int nnodes,
                               int *index,
                               int *edges,
                               int *newrank);
/*
 * ******************************************************************
 * ************ functions implemented in this module end ************
 * ******************************************************************
 */

END_C_DECLS

#endif /* MCA_TOPO_EXAMPLE_H */
