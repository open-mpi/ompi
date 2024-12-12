/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_ime.h"
#include "adio_extern.h"
#include "ad_ime_common.h"
#include <assert.h>

void ADIOI_IME_Fcntl(ADIO_File fd, int flag, ADIO_Fcntl_t * fcntl_struct, int *error_code)
{
    int ret;
    static char myname[] = "ADIOI_IME_FCNTL";

    switch (flag) {
        case ADIO_FCNTL_GET_FSIZE:
            {
                struct stat stbuf;

                stbuf.st_size = 0;
                struct ADIOI_IME_fs_s *ime_fs = (ADIOI_IME_fs *) fd->fs_ptr;
                assert(ime_fs);
                ret = ime_native_stat(ime_fs->ime_filename, &stbuf);

                if (ret) {
                    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                                       MPIR_ERR_RECOVERABLE,
                                                       myname, __LINE__,
                                                       MPI_ERR_FILE, "Error in ime_native_stat", 0);
                    return;
                }

                fcntl_struct->fsize = stbuf.st_size;
                *error_code = MPI_SUCCESS;
                break;
            }
        case ADIO_FCNTL_SET_DISKSPACE:
            ADIOI_GEN_Prealloc(fd, fcntl_struct->diskspace, error_code);
            break;

        case ADIO_FCNTL_SET_ATOMICITY:
        default:
            *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                               MPIR_ERR_RECOVERABLE,
                                               myname, __LINE__,
                                               MPI_ERR_ARG, "**flag", "**flag %d", flag);
            break;
    };
}
