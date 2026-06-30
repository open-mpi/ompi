/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * Copyright (c) 2018-2025 Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mpi/tool/mpit-internal.h"
#include "ompi/info/info.h"

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
    mca_base_event_t *event;
    int rc;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        rc = mca_base_event_get_by_index (event_index, &event);
        if (OPAL_SUCCESS != rc) {
            rc = MPI_T_ERR_INVALID_INDEX;
            break;
        }

        mpit_copy_string (name, name_len, event->name);
        mpit_copy_string (desc, desc_len, event->description);

        if (NULL != verbosity) {
            *verbosity = event->verbosity;
        }

        if (NULL != bind) {
            *bind = event->bind;
        }

        if (NULL != enumtype) {
            *enumtype = event->enumerator ? (MPI_T_enum) event->enumerator : MPI_T_ENUM_NULL;
        }

        /* The caller passes the array capacity in *num_elements on input; fill
           at most that many datatype/displacement entries, then report the true
           element count.  NULL array pointers and a NULL num_elements pointer
           are all tolerated (the standard allows the tool to skip these). */
        if (NULL != num_elements) {
            int capacity = *num_elements;

            for (int i = 0; i < capacity && i < event->num_elements; ++i) {
                if (NULL != array_of_datatypes) {
                    ompit_var_type_to_datatype (event->element_types[i],
                                                array_of_datatypes + i);
                }

                if (NULL != array_of_displacements) {
                    array_of_displacements[i] = (MPI_Aint) event->element_offsets[i];
                }
            }

            *num_elements = event->num_elements;
        }

        /* The standard requires a fresh, user-freeable MPI_Info.  An event type
           carries no info hints in this implementation, so return an empty one
           (ompi_info_allocate yields a valid, user-freeable handle). */
        rc = MPI_SUCCESS;
        if (NULL != info) {
            *info = ompi_info_allocate ();
            if (NULL == *info) {
                rc = MPI_T_ERR_MEMORY;
                break;
            }
        }
    } while (0);

    ompi_mpit_unlock ();

    return rc;
}
