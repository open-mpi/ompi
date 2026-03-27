/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"


ompi_datatype_t* ompi_datatype_lookup_by_opal_id( uint16_t opal_id )
{
    int32_t i;
    const ompi_datatype_t* datatype = NULL;

    for (int j = 0 ; j < OMPI_DATATYPE_MPI_MAX_PREDEFINED ; ++j) {
        if (ompi_datatype_basicDatatypes[j]->super.id == opal_id) {
            datatype = (ompi_datatype_t *) ompi_datatype_basicDatatypes[j];
            break;
        }
    }

    return datatype;
}
