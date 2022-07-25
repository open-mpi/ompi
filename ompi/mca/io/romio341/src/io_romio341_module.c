/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017-2019 IBM Corporation. All rights reserved.
 * Copyright (c) 2017-2021 Research Organization for Information Science
 *                         and Technology  (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "mpi.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/mca/io/io.h"
#include "io_romio341.h"


/*
 * Global functions that do not need to be prototyped in a header
 * because ROMIO just expects these functions to exist.
 */
int MPIR_Status_set_bytes(ompi_status_public_t *status,
                          struct ompi_datatype_t *datatype, MPI_Count size);
void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag);


/*
 * The ROMIO module operations
 */
mca_io_base_module_2_0_0_t mca_io_romio341_module = {
    /* Back end to MPI API calls (pretty much a 1-to-1 mapping) */

    mca_io_romio341_file_open,
    mca_io_romio341_file_close,

    mca_io_romio341_file_set_size,
    mca_io_romio341_file_preallocate,
    mca_io_romio341_file_get_size,
    mca_io_romio341_file_get_amode,
    mca_io_romio341_file_set_info,
    mca_io_romio341_file_get_info,
    mca_io_romio341_file_set_view,
    mca_io_romio341_file_get_view,

    /* Index IO operations */
    mca_io_romio341_file_read_at,
    mca_io_romio341_file_read_at_all,
    mca_io_romio341_file_write_at,
    mca_io_romio341_file_write_at_all,
    mca_io_romio341_file_iread_at,
    mca_io_romio341_file_iwrite_at,
    mca_io_romio341_file_iread_at_all,
    mca_io_romio341_file_iwrite_at_all,

    /* non-indexed IO operations */
    mca_io_romio341_file_read,
    mca_io_romio341_file_read_all,
    mca_io_romio341_file_write,
    mca_io_romio341_file_write_all,
    mca_io_romio341_file_iread,
    mca_io_romio341_file_iwrite,
    mca_io_romio341_file_iread_all,
    mca_io_romio341_file_iwrite_all,

    mca_io_romio341_file_seek,
    mca_io_romio341_file_get_position,
    mca_io_romio341_file_get_byte_offset,

    mca_io_romio341_file_read_shared,
    mca_io_romio341_file_write_shared,
    mca_io_romio341_file_iread_shared,
    mca_io_romio341_file_iwrite_shared,
    mca_io_romio341_file_read_ordered,
    mca_io_romio341_file_write_ordered,
    mca_io_romio341_file_seek_shared,
    mca_io_romio341_file_get_position_shared,

    /* Split IO operations */
    mca_io_romio341_file_read_at_all_begin,
    mca_io_romio341_file_read_at_all_end,
    mca_io_romio341_file_write_at_all_begin,
    mca_io_romio341_file_write_at_all_end,
    mca_io_romio341_file_read_all_begin,
    mca_io_romio341_file_read_all_end,
    mca_io_romio341_file_write_all_begin,
    mca_io_romio341_file_write_all_end,
    mca_io_romio341_file_read_ordered_begin,
    mca_io_romio341_file_read_ordered_end,
    mca_io_romio341_file_write_ordered_begin,
    mca_io_romio341_file_write_ordered_end,

    mca_io_romio341_file_get_type_extent,

    /* Sync/atomic IO operations */
    mca_io_romio341_file_set_atomicity,
    mca_io_romio341_file_get_atomicity,
    mca_io_romio341_file_sync
};


void ADIOI_Datatype_iscontig(MPI_Datatype datatype, int *flag)
{
    /*
     * Open MPI contiguous check return true for datatype with
     * gaps in the beginning and at the end. We have to provide
     * a count of 2 in order to get these gaps taken into account.
     * In addition, if the data is contiguous but true_lb differs
     * from zero, ROMIO will ignore the displacement. Thus, lie!
     */
    *flag = ompi_datatype_is_contiguous_memory_layout(datatype, 2);
    if (*flag) {
        MPI_Aint true_extent, true_lb;

        ompi_datatype_get_true_extent(datatype, &true_lb, &true_extent);

        if (true_lb > 0)
            *flag = 0;
    }
}
