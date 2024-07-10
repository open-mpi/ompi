/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2019 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/tool/mpit-internal.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_event_get_info = PMPI_T_event_get_info
#endif
#define MPI_T_event_get_info PMPI_T_event_get_info
#endif

int MPI_T_event_get_info (int event_index, char *name, int *name_len,
                          int *verbosity, MPI_Datatype *array_of_datatypes,
                          MPI_Aint *array_of_displacements, int *num_elements,
                          MPI_T_enum *enumtype, MPI_Info *info,
                          char *desc, int *desc_len, int *bind)
{
    mca_base_event_t * const event;
    int ret, max_datatypes = 0, current_displacement = 0;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    /* Find the performance variable. mca_base_event_get() handles the
       bounds checking. */
    ret = mca_base_event_get_by_index (event_index, (mca_base_event_t **) &event);
    if (OMPI_SUCCESS != ret) {
        goto fn_fail;
    }

    /* Check the variable binding is something sane */
    if (event->event_bind > MPI_T_BIND_MPI_INFO || event->event_bind < MPI_T_BIND_NO_OBJECT) {
        /* This variable specified an invalid binding (not an MPI object). */
        ret = MPI_T_ERR_INVALID_INDEX;
        goto fn_fail;
    }

    /* Copy name and description */
    mpit_copy_string (name, name_len, event->event_name);
    mpit_copy_string (desc, desc_len, event->event_description);

    // num_elements is INOUT
    //
    // Can query number of datatypes, returned in num_elements
    //   if array_of_datatypes or displacements are NULL, just return data_type_count.
    // Otherwise, if array_of_datatypes or displacements are not NULL, use num_elements
    //   as maximum datatypes or displacements returned.
    //
    // Unless the user passes the NULL pointer for num_elements, 
    //   the function returns the number of elements required for this event type.
    //
    // If the number of elements used by the event type is larger than the value of num_elements 
    //   provided by the user, the number of datatype handles and displacements returned in the 
    //   corresponding arrays is truncated to the value of num_elements passed in by the user.

    max_datatypes = 0;
    if (num_elements) {
        if (NULL != array_of_datatypes || NULL != array_of_displacements) {
            if (*num_elements < (int) (event->event_datatype_count)) {
                max_datatypes = *num_elements;
            } else {
                max_datatypes = event->event_datatype_count;
            }
        }
        *num_elements = event->event_datatype_count;
    }

    if (max_datatypes) {
        if (array_of_datatypes) {
            for (int i = 0 ; i < max_datatypes ; i++) {

                array_of_datatypes[i] = ompi_datatype_lookup_by_opal_id(event->event_datatypes[i]->id);
            }
        }

        if (array_of_displacements) {
            for (int i = 0 ; i < max_datatypes ; i++) {
                array_of_displacements[i] = (MPI_Aint) current_displacement;
                current_displacement += event->event_datatypes[i]->size;
            }
        }

        *num_elements = max_datatypes;
    }

    if (NULL != verbosity) {
        *verbosity = event->event_verbosity;
    }

    if (NULL != enumtype) {
        *enumtype = event->event_enumerator ? (MPI_T_enum) event->event_enumerator : MPI_T_ENUM_NULL;
    }

    if (NULL != bind) {
        *bind = event->event_bind;
    }

    if (NULL != info) {
        *info = OBJ_NEW(ompi_info_t);
    }

fn_fail:
    ompi_mpit_unlock ();

    return ret;
}
