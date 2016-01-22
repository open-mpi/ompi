/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016 IBM Corp.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_INFO_H
#define OMPI_INFO_H

#include "ompi_config.h"
#include <string.h>

#include "mpi.h"
#include "opal/util/info.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/threads/mutex.h"

#include "opal/mca/base/mca_base_var_enum.h"


struct ompi_info_t {
    struct opal_info_t super;
  /**< generic list pointer which is the container for (key,value)
       pairs */
  int i_f_to_c_index;
  /**< fortran handle for info. This is needed for translation from
       fortran to C and vice versa */
  /**< Mutex for thread safety */
  bool i_freed;
  /**< Whether this info has been freed or not */
};
typedef struct ompi_info_t ompi_info_t;

/**
 * Padded struct to maintain back compatibiltiy.
 * See ompi/communicator/communicator.h comments with struct ompi_communicator_t
 * for full explanation why we chose the following padding construct for predefines.
 */
#define PREDEFINED_INFO_PAD (sizeof(void*) * 32)

struct ompi_predefined_info_t {
    struct ompi_info_t info;
    char padding[PREDEFINED_INFO_PAD - sizeof(ompi_info_t)];
};
typedef struct ompi_predefined_info_t ompi_predefined_info_t;

BEGIN_C_DECLS

/**
 * Global instance for MPI_INFO_NULL
 */
OMPI_DECLSPEC extern ompi_predefined_info_t ompi_mpi_info_null;

/**
 * Symbol for Fortran 03 bindings to bind to
 */
OMPI_DECLSPEC extern ompi_predefined_info_t *ompi_mpi_info_null_addr;

/**
 * \internal
 * Some declarations needed to use OBJ_NEW and OBJ_DESTRUCT macros
 */
OMPI_DECLSPEC OBJ_CLASS_DECLARATION(ompi_info_t);

/**
 * This function is invoked during ompi_mpi_init() and sets up
 * MPI_Info handling.
 */
int ompi_mpiinfo_init(void);

/**
 * This function is used to free a ompi level info
 */
int ompi_info_free (ompi_info_t **info);


/**
 * This functions is called during ompi_mpi_finalize() and shuts
 * down MPI_Info handling.
 */
int ompi_mpiinfo_finalize(void);

END_C_DECLS

/**
 * Return whether this info has been freed already or not.
 *
 * @param info Pointer to opal_info_t object.
 *
 * @retval true If the info has already been freed
 * @retval false If the info has not yet been freed
 *
 * If the info has been freed, return true.  This will likely only
 * happen in a reliable manner if opal_debug_handle_never_free is
 * true, in which case an extra OBJ_RETAIN is set on the object during
 * OBJ_NEW, meaning that the user will never be able to actually free
 * the underlying object.  It's a good way to find out if a process is
 * unintentionally using a freed handle.
 */
static inline bool ompi_info_is_freed(ompi_info_t *info)
{
  return info->i_freed;
}



#endif /* OMPI_INFO_H */
