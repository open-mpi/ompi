/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_SIMPLE_H
#define PMIX_SIMPLE_H

#include <src/include/pmix_config.h>


#include "src/mca/psec/psec.h"

BEGIN_C_DECLS

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_psec_base_component_t mca_psec_dummy_handshake_component;
extern pmix_psec_module_t pmix_dummy_handshake_module;

END_C_DECLS

#endif
