/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "adio_extern.h"
#include "adioi.h"

/* returns the current position of the individual file pointer
   in etype units relative to the current view. */

void ADIOI_Get_position(ADIO_File fd, ADIO_Offset * offset)
{
    ADIOI_Flatlist_node *flat_file;
    int i, flag;
    MPI_Count filetype_size, etype_size;
    int filetype_is_contig;
    MPI_Aint lb, filetype_extent;
    ADIO_Offset disp, byte_offset, sum = 0, size_in_file, n_filetypes, frd_size;

    ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
    etype_size = fd->etype_size;

    if (filetype_is_contig)
        *offset = (fd->fp_ind - fd->disp) / etype_size;
    else {
        flat_file = ADIOI_Flatten_and_find(fd->filetype);

        MPI_Type_size_x(fd->filetype, &filetype_size);
        MPI_Type_get_extent(fd->filetype, &lb, &filetype_extent);

        disp = fd->disp;
        byte_offset = fd->fp_ind;
        n_filetypes = -1;
        flag = 0;
        while (!flag) {
            sum = 0;
            n_filetypes++;
            for (i = 0; i < flat_file->count; i++) {
                sum += flat_file->blocklens[i];
                if (disp + flat_file->indices[i] +
                    n_filetypes * ADIOI_AINT_CAST_TO_OFFSET filetype_extent +
                    flat_file->blocklens[i]
                    >= byte_offset) {
                    frd_size = disp + flat_file->indices[i] +
                        n_filetypes * ADIOI_AINT_CAST_TO_OFFSET filetype_extent
                        + flat_file->blocklens[i] - byte_offset;
                    sum -= frd_size;
                    flag = 1;
                    break;
                }
            }
        }
        size_in_file = n_filetypes * (ADIO_Offset) filetype_size + sum;
        *offset = size_in_file / etype_size;
    }
}
