/*
 * $HEADER$
 */

/**
 * @file
 *
 * Internal Run-Time interface functionality
 */
#ifndef OMPI_RUNTIME_INTERNAL_H
#define OMPI_RUNTIME_INTERNAL_H

#include "ompi_config.h"

#include "class/ompi_object.h"

int ompi_rte_internal_init_spawn(void);
int ompi_rte_internal_fini_spawn(void);

#endif
