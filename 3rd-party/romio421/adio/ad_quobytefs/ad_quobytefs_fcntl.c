/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"

#include "ad_quobytefs.h"
#include "ad_quobytefs_internal.h"

void ADIOI_QUOBYTEFS_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t * fcntl_struct, int *error_code)
{
    static char myname[] = "ADIOI_QUOBYTEFS_FCNTL";
    struct stat file_stat;
    const char *filepath = ADIOI_QUOBYTEFSI_GetVolumeAndPath(fd->filename);
    if (!(filepath > fd->filename)) {
        *error_code = ADIOI_Err_create_code(myname, fd->filename, EINVAL);
        return;
    }

    switch (flag) {
        case ADIO_FCNTL_GET_FSIZE:
            if (quobyte_getattr(filepath, &file_stat)) {
                *error_code = ADIOI_Err_create_code(myname, fd->filename, errno);
                return;
            }
            fcntl_struct->fsize = file_stat.st_size;
            if (fcntl_struct->fsize == -1) {
                *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                                   MPIR_ERR_RECOVERABLE,
                                                   myname, __LINE__,
                                                   MPI_ERR_IO, "**io", "**io %s", strerror(errno));
            } else {
                *error_code = MPI_SUCCESS;
            }
            break;

        case ADIO_FCNTL_SET_DISKSPACE:
            ADIOI_GEN_Prealloc(fd, fcntl_struct->diskspace, error_code);
            break;

        case ADIO_FCNTL_SET_ATOMICITY:
            fd->atomicity = (fcntl_struct->atomicity == 0) ? 0 : 1;
            *error_code = MPI_SUCCESS;
            break;

        default:
            *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                               MPIR_ERR_RECOVERABLE,
                                               myname, __LINE__,
                                               MPI_ERR_ARG, "**flag", "**flag %d", flag);
    }
}
