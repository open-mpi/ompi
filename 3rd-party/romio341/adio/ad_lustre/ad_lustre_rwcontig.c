/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "ad_lustre.h"

#include <unistd.h>

#include <stdlib.h>
#include <malloc.h>

#define LUSTRE_MEMALIGN (1<<12) /* to use page_shift */

static void ADIOI_LUSTRE_Aligned_Mem_File_Write(ADIO_File fd, const void *buf, MPI_Count len,
                                                ADIO_Offset offset, ssize_t * err);
static void ADIOI_LUSTRE_Aligned_Mem_File_Write(ADIO_File fd, const void *buf, MPI_Count len,
                                                ADIO_Offset offset, ssize_t * err)
{
    ssize_t rem, size, nbytes;
    if (!(len % fd->d_miniosz) && (len >= fd->d_miniosz)) {
        *err = pwrite(fd->fd_direct, buf, len, offset);
    } else if (len < fd->d_miniosz) {
        *err = pwrite(fd->fd_sys, buf, len, offset);
    } else {
        rem = len % fd->d_miniosz;
        size = len - rem;
        *err = pwrite(fd->fd_direct, buf, size, offset);
        if (*err == -1)
            return;
        nbytes = *err;
        *err = pwrite(fd->fd_sys, ((char *) buf) + size, rem, offset + size);
        if (*err == -1)
            return;
        *err += nbytes;
    }
}

static void ADIOI_LUSTRE_Aligned_Mem_File_Read(ADIO_File fd, const void *buf, MPI_Count len,
                                               ADIO_Offset offset, ssize_t * err);
static void ADIOI_LUSTRE_Aligned_Mem_File_Read(ADIO_File fd, const void *buf, MPI_Count len,
                                               ADIO_Offset offset, ssize_t * err)
{
    MPI_Count rem, size;
    ssize_t nbytes;
    if (!(len % fd->d_miniosz) && (len >= fd->d_miniosz))
        *err = pread(fd->fd_direct, (void *) buf, len, offset);
    else if (len < fd->d_miniosz)
        *err = pread(fd->fd_sys, (void *) buf, len, offset);
    else {
        rem = len % fd->d_miniosz;
        size = len - rem;
        *err = pread(fd->fd_direct, (void *) buf, size, offset);
        if (*err == -1)
            return;
        nbytes = *err;
        *err = pread(fd->fd_sys, ((char *) buf) + size, rem, offset + size);
        if (*err == -1)
            return;
        *err += nbytes;
    }
}

static ssize_t ADIOI_LUSTRE_Directio(ADIO_File fd, const void *buf, MPI_Count len,
                                     off_t offset, int rw);
static ssize_t ADIOI_LUSTRE_Directio(ADIO_File fd, const void *buf, MPI_Count len,
                                     off_t offset, int rw)
{
    ssize_t err = -1, diff, nbytes = 0;
    MPI_Count size = len;
    void *newbuf;

    if (offset % fd->d_miniosz) {
        diff = fd->d_miniosz - (offset % fd->d_miniosz);
        diff = MPL_MIN(diff, len);
        if (rw)
            nbytes = pwrite(fd->fd_sys, (void *) buf, diff, offset);
        else
            nbytes = pread(fd->fd_sys, (void *) buf, diff, offset);
        if (nbytes == -1)
            return -1;
        buf = ((char *) buf) + diff;
        offset += diff;
        size = len - diff;
    }

    if (!size) {
        return nbytes;
    }

    if (rw) {   /* direct I/O enabled */
        if (!(((long) buf) % fd->d_mem)) {
            ADIOI_LUSTRE_Aligned_Mem_File_Write(fd, buf, size, offset, &err);
            if (err == -1)
                return -1;
            nbytes += err;
        } else {
            newbuf = (void *) memalign(LUSTRE_MEMALIGN, size);
            if (newbuf) {
                memcpy(newbuf, buf, size);
                ADIOI_LUSTRE_Aligned_Mem_File_Write(fd, newbuf, size, offset, &err);
                if (err == -1)
                    return -1;
                nbytes += err;
                ADIOI_Free(newbuf);
            } else {
                err = pwrite(fd->fd_sys, buf, size, offset);
                if (err == -1)
                    return -1;
                nbytes += err;
            }
        }
    } else {
        if (!(((long) buf) % fd->d_mem)) {
            ADIOI_LUSTRE_Aligned_Mem_File_Read(fd, buf, size, offset, &err);
            if (err == -1)
                return -1;
            nbytes += err;
        } else {
            newbuf = (void *) memalign(LUSTRE_MEMALIGN, size);
            if (newbuf) {
                ADIOI_LUSTRE_Aligned_Mem_File_Read(fd, newbuf, size, offset, &err);
                if (err == -1)
                    return -1;
                if (err > 0)
                    memcpy((void *) buf, newbuf, err);
                nbytes += err;
                ADIOI_Free(newbuf);
            } else {
                err = pread(fd->fd_sys, (void *) buf, size, offset);
                if (err == -1)
                    return -1;
                nbytes += err;
            }
        }
    }
    return nbytes;
}

static void ADIOI_LUSTRE_IOContig(ADIO_File fd, const void *buf, int count,
                                  MPI_Datatype datatype, int file_ptr_type,
                                  ADIO_Offset offset, ADIO_Status * status,
                                  int io_mode, int *error_code);
static void ADIOI_LUSTRE_IOContig(ADIO_File fd, const void *buf, int count,
                                  MPI_Datatype datatype, int file_ptr_type,
                                  ADIO_Offset offset, ADIO_Status * status,
                                  int io_mode, int *error_code)
{
    ssize_t err = 0;
    size_t rw_count;
    ADIO_Offset bytes_xfered = 0;
    MPI_Count datatype_size, len;
    static char myname[] = "ADIOI_LUSTRE_IOCONTIG";
    char *p;

    if (count == 0) {
        err = 0;
        goto fn_exit;
    }

    MPI_Type_size_x(datatype, &datatype_size);
    len = datatype_size * count;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
        offset = fd->fp_ind;
    }

    if ((!io_mode && !fd->direct_read) || (io_mode && !fd->direct_write)) {

        p = (char *) buf;
        if (io_mode) {
            while (bytes_xfered < len) {
#ifdef ADIOI_MPE_LOGGING
                MPE_Log_event(ADIOI_MPE_write_a, 0, NULL);
#endif
                rw_count = len - bytes_xfered;
                err = pwrite(fd->fd_sys, p, rw_count, offset + bytes_xfered);
                if (err == -1)
                    goto ioerr;
#ifdef ADIOI_MPE_LOGGING
                MPE_Log_event(ADIOI_MPE_write_b, 0, NULL);
#endif
                if (err == 0)
                    break;
                bytes_xfered += err;
                p += err;
            }
        } else {
            while (bytes_xfered < len) {
#ifdef ADIOI_MPE_LOGGING
                MPE_Log_event(ADIOI_MPE_read_a, 0, NULL);
#endif
                rw_count = len - bytes_xfered;
                err = pread(fd->fd_sys, p, rw_count, offset + bytes_xfered);
                if (err == -1)
                    goto ioerr;
#ifdef ADIOI_MPE_LOGGING
                MPE_Log_event(ADIOI_MPE_read_b, 0, NULL);
#endif
                if (err == 0)
                    break;
                bytes_xfered += err;
                p += err;
            }
        }
    } else {
        err = ADIOI_LUSTRE_Directio(fd, buf, len, offset, io_mode);
        if (err == -1)
            goto ioerr;
        bytes_xfered = err;
    }

    fd->fp_sys_posn = offset + bytes_xfered;

    if (file_ptr_type == ADIO_INDIVIDUAL) {
        fd->fp_ind += bytes_xfered;
    }

  fn_exit:
#ifdef HAVE_STATUS_SET_BYTES
    if (status && err != -1)
        MPIR_Status_set_bytes(status, datatype, bytes_xfered);
#endif
    *error_code = MPI_SUCCESS;

  ioerr:
    /* --BEGIN ERROR HANDLING-- */
    if (err == -1) {
        *error_code = MPIO_Err_create_code(MPI_SUCCESS,
                                           MPIR_ERR_RECOVERABLE,
                                           myname, __LINE__,
                                           MPI_ERR_IO, "**io", "**io %s", strerror(errno));
        fd->fp_sys_posn = -1;
        return;
    }
    /* --END ERROR HANDLING-- */
}

void ADIOI_LUSTRE_WriteContig(ADIO_File fd, const void *buf, int count,
                              MPI_Datatype datatype, int file_ptr_type,
                              ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    ADIOI_LUSTRE_IOContig(fd, buf, count, datatype, file_ptr_type, offset, status, 1, error_code);
}

void ADIOI_LUSTRE_ReadContig(ADIO_File fd, void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Status * status, int *error_code)
{
    ADIOI_LUSTRE_IOContig(fd, buf, count, datatype, file_ptr_type, offset, status, 0, error_code);
}
