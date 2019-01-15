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
 * Copyright (c) 2018      UT-Battelle, LLC. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MEMPROF_H
#define OPAL_MEMPROF_H

BEGIN_C_DECLS

#if OPAL_ENABLE_MEM_PROFILE

  void __attribute__((weak)) Tau_track_class_allocation(const char * name, size_t size);
  void __attribute__((weak)) Tau_track_class_deallocation(const char * name, size_t size);
  void __attribute__((weak)) Tau_start_class_allocation(const char * name, size_t size, int include_in_parent);
  void __attribute__((weak)) Tau_stop_class_allocation(const char * name, int record);
  void __attribute__((weak)) Tau_start_class_deallocation(const char * name, size_t size, int include_in_parent);
  void __attribute__((weak)) Tau_stop_class_deallocation(const char * name, int record);

#endif /* OPAL_ENABLE_MEM_PROFILE */

END_C_DECLS

#endif /* OPAL_MEMPROF_H */
