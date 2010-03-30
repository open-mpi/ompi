/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_PTMALLOC2_MUNMAP_H
#define OPAL_PTMALLOC2_MUNMAP_H

#include "opal_config.h"

BEGIN_C_DECLS

OPAL_DECLSPEC int opal_mem_free_ptmalloc2_munmap(void *start, size_t length, int from_alloc, int call_hooks);

OPAL_DECLSPEC int munmap(void* addr, size_t len);


END_C_DECLS

#endif /* !OPAL_PTMALLOC2_MUNMAP_H */
