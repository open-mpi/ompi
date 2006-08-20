/*
 * Copyright (c) 2004-2006 The Regents of the University of California.
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
#ifndef MCA_PML_CM_PROC_H
#define MCA_PML_CM_PROC_H

#include "opal/threads/mutex.h"
#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/mtl.h"

struct mca_mtl_base_procinfo_t;

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

struct mca_pml_cm_proc_t {
    mca_pml_proc_t base;
};
typedef struct mca_pml_cm_proc_t mca_pml_cm_proc_t;
OMPI_DECLSPEC extern opal_class_t mca_pml_cm_proc_t_class; 

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

