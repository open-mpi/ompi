/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_V_H_HAS_BEEN_INCLUDED
#define PML_V_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/request/request.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_pml_v_t {
    mca_pml_base_component_t            host_pml_component;
    mca_pml_base_module_t               host_pml;
    ompi_request_fns_t                  host_request_fns;
    size_t                              host_pml_req_recv_size;
    size_t                              host_pml_req_send_size;
};
typedef struct mca_pml_v_t mca_pml_v_t;

OMPI_DECLSPEC extern mca_pml_v_t mca_pml_v;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#include "pml_v_output.h"

#endif  /* PML_V_H_HAS_BEEN_INCLUDED */
