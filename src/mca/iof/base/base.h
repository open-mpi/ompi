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
/**
 * @file
 *
 * I/O Forwarding Service
 */
                                                                                         
#ifndef MCA_IOF_BASE_H
#define MCA_IOF_BASE_H

#include "ompi_config.h"
#include "include/ompi.h"
#include "mca/mca.h"
#include "mca/iof/iof.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


int mca_iof_base_open(void);
int mca_iof_base_close(void);
int mca_iof_base_select(bool* allow_multi_user_threads);

extern int mca_iof_base_output;
extern ompi_list_t mca_iof_base_components_opened;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif /* MCA_PML_H */
