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

extern const mca_base_component_t mca_ptl_tcp_component;
extern const mca_base_component_t mca_ptl_self_component;

const mca_base_component_t *mca_ptl_base_static_components[] = {
  &mca_ptl_self_component, 
  &mca_ptl_tcp_component, 
  NULL
};
