/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2008-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2010-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014      Hochschule Esslingen.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#if !defined(OPAL_PARAMS_H)
#define OPAL_PARAMS_H

extern char *opal_signal_string;
extern char *opal_net_private_ipv4;
extern char *opal_set_max_sys_limits;

#if OPAL_ENABLE_TIMING
extern char *opal_timing_sync_file;
extern char *opal_timing_output;
extern bool opal_timing_overhead;
#endif

OPAL_DECLSPEC extern int opal_initialized;
OPAL_DECLSPEC extern bool opal_built_with_cuda_support;

/**
 *  * Whether we want to enable CUDA GPU buffer send and receive support.
 *   */
OPAL_DECLSPEC extern bool opal_cuda_support;

/**
 * Whether to use the "leave pinned" protocol or not (0 = no, 1 = yes,
 * -1 = determine at runtime).
 */
OPAL_DECLSPEC extern int opal_leave_pinned;

/**
 * Whether to use the "leave pinned pipeline" protocol or not.
 */
OPAL_DECLSPEC extern bool opal_leave_pinned_pipeline;

#if OPAL_ENABLE_DEBUG
extern bool opal_progress_debug;
#endif

#if OPAL_ENABLE_FT_CR == 1
extern bool opal_base_distill_checkpoint_ready;
#endif

#endif
