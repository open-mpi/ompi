/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "adio_extern.h"

static
int check_type(ADIOI_Flatlist_node * flat_type,
               int access_mode, const char *caller, const char *type_kind, int *error_code)
{
    char err_msg[128];

    err_msg[0] = '\0';

    /* MPI standard requires the displacements of etype and filetype be
     * non-negative */
    if (flat_type->flag & ADIOI_TYPE_NEGATIVE) {
        sprintf(err_msg, "displacements of %s must be non-negative", type_kind);
        goto err_check;
    }

    /* MPI standard requires the displacements of etype and filetype be in a
     * monotonically nondecreasing order */
    if (flat_type->flag & ADIOI_TYPE_DECREASE) {
        sprintf(err_msg, "displacements of %s must be in a monotonically nondecreasing order",
                type_kind);
        goto err_check;
    }

    /* If the file is opened for writing, neither the etype nor the
     * filetype is permitted to contain overlapping regions.
     */
    if (((access_mode & ADIO_WRONLY) || (access_mode & ADIO_RDWR)) &&
        (flat_type->flag & ADIOI_TYPE_OVERLAP)) {
        sprintf(err_msg, "%s is not permitted to contain overlapping regions", type_kind);
        goto err_check;
    }

    return 1;

  err_check:
    *error_code = MPIO_Err_create_code(*error_code,
                                       MPIR_ERR_RECOVERABLE, caller,
                                       __LINE__, MPI_ERR_IO, "**iobadoverlap", " **iobadoverlap %s",
                                       err_msg);
    return 0;
}

/* this used to be implemented in every file system as an fcntl.  It makes
 * deferred open easier if we know ADIO_Fcntl will always need a file to really
 * be open. set_view doesn't modify anything related to the open files.
 */
void ADIO_Set_view(ADIO_File fd, ADIO_Offset disp, MPI_Datatype etype,
                   MPI_Datatype filetype, MPI_Info info, int *error_code)
{
    static char myname[] = "ADIO_Set_view";
    int combiner, i, j, k, err, etype_is_contig, filetype_is_contig;
    MPI_Datatype copy_etype, copy_filetype;
    ADIOI_Flatlist_node *flat_file, *flat_etype;
    /* free copies of old etypes and filetypes and delete flattened
     * version of filetype if necessary */

    MPI_Type_get_envelope(fd->etype, &i, &j, &k, &combiner);
    if (combiner != MPI_COMBINER_NAMED)
        MPI_Type_free(&(fd->etype));

    MPI_Type_get_envelope(fd->filetype, &i, &j, &k, &combiner);
    if (combiner != MPI_COMBINER_NAMED)
        MPI_Type_free(&(fd->filetype));

    /* set new info */
    ADIO_SetInfo(fd, info, &err);

    /* set new etypes and filetypes */

    MPI_Type_get_envelope(etype, &i, &j, &k, &combiner);
    if (combiner == MPI_COMBINER_NAMED) {
        fd->etype = etype;
        etype_is_contig = 1;
    } else {
        MPI_Type_contiguous(1, etype, &copy_etype);
        MPI_Type_commit(&copy_etype);
        fd->etype = copy_etype;
        ADIOI_Datatype_iscontig(fd->etype, &etype_is_contig);
    }
    flat_etype = ADIOI_Flatten_and_find(fd->etype);
    if (0 == check_type(flat_etype, fd->orig_access_mode, myname, "etype", error_code))
        return;

    MPI_Type_get_envelope(filetype, &i, &j, &k, &combiner);
    if (combiner == MPI_COMBINER_NAMED) {
        fd->filetype = filetype;
        filetype_is_contig = 1;
    } else {
        MPI_Type_contiguous(1, filetype, &copy_filetype);
        MPI_Type_commit(&copy_filetype);
        fd->filetype = copy_filetype;
        ADIOI_Datatype_iscontig(fd->filetype, &filetype_is_contig);
    }

    flat_file = ADIOI_Flatten_and_find(fd->filetype);
    if (0 == check_type(flat_file, fd->orig_access_mode, myname, "filetype", error_code))
        return;

    MPI_Type_size_x(fd->etype, &(fd->etype_size));
    fd->disp = disp;

    /* reset MPI-IO file pointer to point to the first byte that can
     * be accessed in this view. */

    if (filetype_is_contig)
        fd->fp_ind = disp;
    else {
        for (i = 0; i < flat_file->count; i++) {
            if (flat_file->blocklens[i]) {
                fd->fp_ind = disp + flat_file->indices[i];
                break;
            }
        }
    }
    *error_code = MPI_SUCCESS;
}
