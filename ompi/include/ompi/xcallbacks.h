/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

/**
 * @file
 *
 * Shared internal type definitions for extended callback support.
 *
 * This header provides the internal destructor typedef used by the
 * MPIX_*_x ("extended callback") APIs across op, errhandler, and
 * attribute back ends.
 */

#ifndef OMPI_XCALLBACKS_H
#define OMPI_XCALLBACKS_H

#include "ompi_config.h"

BEGIN_C_DECLS

/**
 * User destructor function type for extended callback APIs.
 *
 * This destructor is invoked exactly once when the owning object
 * (op, errhandler, or keyval) is destroyed. The destructor receives
 * the extra_state pointer that was provided at object creation time.
 *
 * @param extra_state   Extra state pointer provided at creation
 */
typedef void (ompi_user_destructor_fn_t)(void *extra_state);

END_C_DECLS

#endif /* OMPI_XCALLBACKS_H */
