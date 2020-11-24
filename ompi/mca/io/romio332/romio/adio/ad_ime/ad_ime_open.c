/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*-
 *
 *
 *   Copyright (C) 1997 University of Chicago.
 *   Copyright (C) 2017 DataDirect Networks.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ime.h"
#include "ad_ime_common.h"

#include <assert.h>

void ADIOI_IME_Open(ADIO_File fd, int *error_code)
{
    static char myname[] = "ADIOI_IME_OPEN";
    struct ADIOI_IME_fs_s *ime_fs;
    int perm;
    int amode = 0;
    int ret;
    int rank = 0;
    mode_t old_mask;

    /* validate input args */
    if (!fd) {
        *error_code = MPI_ERR_FILE;
        return;
    }
    if (!error_code) {
        *error_code = MPI_ERR_FILE;
        return;
    }

    /* setup file permissions */
    if (fd->perm == ADIO_PERM_NULL) {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    } else
        perm = fd->perm;

    /* setup the file access mode */
    if (fd->access_mode & ADIO_CREATE)
        amode = amode | O_CREAT;
    if (fd->access_mode & ADIO_RDONLY)
        amode = amode | O_RDONLY;
    if (fd->access_mode & ADIO_WRONLY)
        amode = amode | O_WRONLY;
    if (fd->access_mode & ADIO_RDWR)
        amode = amode | O_RDWR;
    if (fd->access_mode & ADIO_EXCL)
        amode = amode | O_EXCL;

    /* XXX no O_APPEND support */
    assert((fd->access_mode & ADIO_APPEND) == 0);

    /* init IME */
    MPI_Comm_rank(fd->comm, &rank);
    ADIOI_IME_Init(rank, error_code);
    if (*error_code != MPI_SUCCESS)
        return;

    ime_fs = (ADIOI_IME_fs *) ADIOI_Malloc(sizeof(ADIOI_IME_fs));

    /* --BEGIN ERROR HANDLING-- */
    if (ime_fs == NULL) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                           MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__,
                                           MPI_ERR_UNKNOWN, "Error allocating memory", 0);
        return;
    }

    ime_fs->ime_filename = ADIOI_IME_Add_prefix(fd->filename);

    /* all processes open the file */
    ret = ime_native_open(ime_fs->ime_filename, amode, perm);
    if (ret < 0) {
        *error_code = MPI_ERR_FILE;
        ADIOI_Free(ime_fs->ime_filename);
        ADIOI_Free(ime_fs);
        return;
    }

    fd->fd_sys = ret;
    fd->fd_direct = -1;
    fd->fs_ptr = ime_fs;

    *error_code = MPI_SUCCESS;

    return;
}
