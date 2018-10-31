/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#include "ompi_config.h"

OMPI_DECLSPEC int ompi_mpiext_init(void);

typedef int (*ompi_mpiext_init_fn_t)(void);
typedef int (*ompi_mpiext_fini_fn_t)(void);

struct ompi_mpiext_component_t {
    ompi_mpiext_init_fn_t init;
    ompi_mpiext_fini_fn_t fini;
};
typedef struct ompi_mpiext_component_t ompi_mpiext_component_t;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
