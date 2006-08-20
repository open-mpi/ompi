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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PML_PROC_H
#define MCA_PML_PROC_H

#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/**
 *  Structure associated w/ ompi_proc_t that contains data specific
 *  to the PML. Note that this name is not PML specific.
 */
struct mca_pml_ob1_proc_t {
    mca_pml_proc_t base; 
};
typedef struct mca_pml_ob1_proc_t mca_pml_ob1_proc_t;
OMPI_DECLSPEC extern opal_class_t mca_pml_ob1_proc_t_class; 

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

