/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
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
/** @file:
 */

#ifndef OMPI_MCA_OSC_BASE_H
#define OMPI_MCA_OSC_BASE_H

#include "ompi/datatype/datatype.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "opal/class/opal_list.h"

/*
 * Global functions for MCA overall collective open and close
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * function definitions
 */
OMPI_DECLSPEC int ompi_osc_base_open(void);
OMPI_DECLSPEC int ompi_osc_base_find_available(bool enable_progress_threads,
                                              bool enable_mpi_threads);

OMPI_DECLSPEC int ompi_osc_base_select(ompi_win_t *win,
                                      ompi_info_t *info,
                                      ompi_communicator_t *comm);

OMPI_DECLSPEC int ompi_osc_base_finalize(void);
OMPI_DECLSPEC int ompi_osc_base_close(void);

OMPI_DECLSPEC extern opal_list_t ompi_osc_base_open_components;
OMPI_DECLSPEC extern opal_list_t ompi_osc_base_avail_components;
OMPI_DECLSPEC extern int ompi_osc_base_output;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
