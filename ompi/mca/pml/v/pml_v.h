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
#include "pml_v_protocol.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/* TODO: Fix that crappy variadic stuff for non GNU C compilers */ 
#ifdef __GNUC__
# define V_OUTPUT(...) OPAL_OUTPUT((mca_pml_v.output, __VA_ARGS__))
# define V_OUTPUT_VERBOSE(V, ...) OPAL_OUTPUT_VERBOSE((V, mca_pml_v.output, __VA_ARGS__))
#else 
static inline void V_OUTPUT(char *format, ...) { OPAL_OUTPUT((mca_pml_v.output, "%s", format)); }
static inline void V_OUTPUT_VERBOSE(int V, char * format, ...) {OPAL_OUTPUT_VERBOSE((V, mca_pml_v.output, "%s", format)); }
#endif 

struct mca_pml_v_t {
  int output;
  mca_pml_v_protocol_base_component_t     protocol_component;
  mca_pml_v_protocol_base_module_t        protocol;
  mca_pml_base_component_t                host_pml_component;
  mca_pml_base_module_t                   host_pml;
  size_t                                  host_pml_req_recv_size;
  size_t                                  host_pml_req_send_size;
};
typedef struct mca_pml_v_t mca_pml_v_t;

OMPI_DECLSPEC extern mca_pml_v_t mca_pml_v;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif  /* PML_V_H_HAS_BEEN_INCLUDED */
