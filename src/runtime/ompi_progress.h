/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _OMPI_PROGRESS_H_
#define _OMPI_PROGRESS_H_
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

OMPI_DECLSPEC extern int ompi_progress_init(void);

OMPI_DECLSPEC extern void ompi_progress_events(int);

OMPI_DECLSPEC extern void ompi_progress(void);

typedef int (*ompi_progress_callback_t)(void);

OMPI_DECLSPEC int ompi_progress_register(ompi_progress_callback_t cb);

OMPI_DECLSPEC int ompi_progress_unregister(ompi_progress_callback_t cb);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif

