/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* NOTE: This framework is really for build system only.  There is no
   set of function pointers that must be called or set interface or
   any of that.  The only two functions that a component must call
   (note: call, not implement) are defined in
   opal/memory/memory_internal.h.  Other than that, to each his
   own.. */
