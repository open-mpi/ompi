/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2012-2018 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi/mpi/tool/mpit-internal.h"

/* needed to convert between opal and ompi datatypes until a function is provided */
#include "ompi/datatype/ompi_datatype_internal.h"

#if OMPI_PROFILING_DEFINES

#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_T_event_get_info = PMPI_T_event_get_info
#endif

#include "ompi/mpi/tool/profile/defines.h"
#endif

int MPI_T_event_get_info (int event_index, char *name, int *name_len,
                          int *verbosity, MPI_Datatype *array_of_datatypes,
                          MPI_Aint *array_of_displacements, int *num_datatypes,
                          MPI_T_enum *enumtype, int *extent, char *desc, int *desc_len,
                          int *bind)
{
    mca_base_event_t * const event;
    int ret, max_datatypes = 0;

    if (!mpit_is_initialized ()) {
        return MPI_T_ERR_NOT_INITIALIZED;
    }

    ompi_mpit_lock ();

    do {
        /* Find the performance variable. mca_base_event_get() handles the
           bounds checking. */
        ret = mca_base_event_get_by_index (event_index, (mca_base_event_t **) &event);
        if (OMPI_SUCCESS != ret) {
            break;
        }

        /* Check the variable binding is something sane */
        if (event->event_bind > MPI_T_BIND_MPI_INFO || event->event_bind < MPI_T_BIND_NO_OBJECT) {
            /* This variable specified an invalid binding (not an MPI object). */
            ret = MPI_T_ERR_INVALID_INDEX;
            break;
        }

        /* Copy name and description */
        mpit_copy_string (name, name_len, event->event_name);
        mpit_copy_string (desc, desc_len, event->event_description);

        if (num_datatypes) {
            if (array_of_datatypes || array_of_displacements) {
                max_datatypes = (*num_datatypes < (int) event->event_datatype_count) ? *num_datatypes : event->event_datatype_count;
            } else {
                max_datatypes = event->event_datatype_count;
            }
        }

        if (max_datatypes) {
            if (array_of_datatypes) {
                for (int i = 0 ; i < max_datatypes ; i++) {
                    ompi_datatype_t *ompi_datatype = NULL;

                    for (int j = 0 ; j < OMPI_DATATYPE_MPI_MAX_PREDEFINED ; ++j) {
                        if (ompi_datatype_basicDatatypes[j]->super.id == event->event_datatypes[i]->id) {
                            ompi_datatype = (ompi_datatype_t *) ompi_datatype_basicDatatypes[j];
                            break;
                        }
                    }

                    assert (NULL != ompi_datatype);

                    array_of_datatypes[i] = ompi_datatype;
                }
            }

            if (array_of_displacements) {
                for (int i = 0 ; i < max_datatypes ; i++) {
                    array_of_displacements[i] = (MPI_Aint) event->event_offsets[i];
                }
            }

            *num_datatypes = max_datatypes;
        }

        if (verbosity) {
            *verbosity = event->event_verbosity;
        }

        if (NULL != enumtype) {
            *enumtype = event->event_enumerator ? (MPI_T_enum) event->event_enumerator : MPI_T_ENUM_NULL;
        }

        if (NULL != bind) {
            *bind = event->event_bind;
        }

        if (NULL != extent) {
            *extent = event->event_extent;
        }
    } while (0);

    ompi_mpit_unlock ();

    return ret;
}
