/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OMPI_CONDITION_H
#define OMPI_CONDITION_H

#include "ompi_config.h"

#if OMPI_HAVE_POSIX_THREADS
#include "condition_pthread.h"
#else
#include "condition_spinlock.h"
#endif

#endif
