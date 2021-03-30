/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */


#include "adio.h"

#include "ad_quobytefs.h"
#include "ad_quobytefs_internal.h"

void ADIOI_QUOBYTEFS_Open(ADIO_File fd, int *error_code)
{
    ADIOI_QUOBYTEFS_CreateAdapter(fd->filename, error_code);
    int perm, old_mask, amode;
    static char myname[] = "ADIOI_QUOBYTEFS_OPEN";
    /* shortest possible path "//A/B" */
    if (strlen(fd->filename) < 5) {
        *error_code = ADIOI_Err_create_code(myname, fd->filename, EINVAL);
        return;
    }
    const char *filepath = ADIOI_QUOBYTEFSI_GetVolumeAndPath(fd->filename);
    if (strlen(filepath) == 0) {
        *error_code = ADIOI_Err_create_code(myname, fd->filename, EINVAL);
        return;
    }
    if (fd->perm == ADIO_PERM_NULL) {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    } else
        perm = fd->perm;

    amode = 0;
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

    /* romio tests expect write then read without flush */
    amode = amode | O_DIRECT;

#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_open_a, 0, NULL);
#endif
    fd->file_handle = quobyte_open(filepath, amode, perm);
#ifdef ADIOI_MPE_LOGGING
    MPE_Log_event(ADIOI_MPE_open_b, 0, NULL);
#endif
    fd->fd_direct = -1;

    if ((fd->file_handle != NULL) && (fd->access_mode & ADIO_APPEND)) {
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_lseek_a, 0, NULL);
#endif
        struct stat file_stat;
        if (quobyte_fstat(fd->file_handle, &file_stat)) {
            *error_code = ADIOI_Err_create_code(myname, filepath, errno);
            return;
        }
        fd->fp_ind = fd->fp_sys_posn = file_stat.st_size;
#ifdef ADIOI_MPE_LOGGING
        MPE_Log_event(ADIOI_MPE_lseek_b, 0, NULL);
#endif
    }
    if (fd->file_handle == NULL) {
        *error_code = ADIOI_Err_create_code(myname, filepath, errno);
    } else {
        *error_code = MPI_SUCCESS;
    }
}
