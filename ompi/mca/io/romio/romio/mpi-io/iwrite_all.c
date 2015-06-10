/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2014 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIX_File_iwrite_all = PMPIX_File_iwrite_all
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIX_File_iwrite_all MPIX_File_iwrite_all
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIX_File_iwrite_all as PMPIX_File_iwrite_all
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPIX_File_iwrite_all(MPI_File fh, const void *buf, int count, MPI_Datatype datatype,
                        MPI_Request *request)
    __attribute__((weak,alias("PMPIX_File_iwrite_all")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

#ifdef HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

/*@
    MPIX_File_iwrite_all - Nonblocking collective write using individual file pointer

Input Parameters:
. fh - file handle (handle)
. buf - initial address of buffer (choice)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. request - request object (handle)

.N fortran
@*/
int MPIX_File_iwrite_all(MPI_File fh, ROMIO_CONST void *buf, int count,
                        MPI_Datatype datatype, MPI_Request *request)
{
    int error_code;
    static char myname[] = "MPIX_FILE_IWRITE_ALL";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEIWRITEALL, TRDTBLOCK, fh, datatype, count);
#endif /* MPI_hpux */

    error_code = MPIOI_File_iwrite_all(fh, (MPI_Offset) 0,
                      ADIO_INDIVIDUAL, buf,
                      count, datatype, myname, request);

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count);
#endif /* MPI_hpux */

    return error_code;
}

/* Note: MPIOI_File_iwrite_all also used by MPIX_File_iwrite_at_all */
/* prevent multiple definitions of this routine */
#ifdef MPIO_BUILD_PROFILING
int MPIOI_File_iwrite_all(MPI_File fh,
            MPI_Offset offset,
            int file_ptr_type,
            const void *buf,
            int count,
            MPI_Datatype datatype,
            char *myname,
            MPI_Request *request)
{
    int error_code;
    MPI_Count datatype_size;
    ADIO_File adio_fh;
    void *e32buf=NULL;
    const void *xbuf=NULL;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT(adio_fh, count, myname, error_code);
    MPIO_CHECK_DATATYPE(adio_fh, datatype, myname, error_code);

    if (file_ptr_type == ADIO_EXPLICIT_OFFSET && offset < 0)
    {
        error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
                          myname, __LINE__, MPI_ERR_ARG,
                          "**iobadoffset", 0);
        error_code = MPIO_Err_return_file(adio_fh, error_code);
        goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    MPI_Type_size_x(datatype, &datatype_size);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_INTEGRAL_ETYPE(adio_fh, count, datatype_size, myname, error_code);
    MPIO_CHECK_WRITABLE(adio_fh, myname, error_code);
    MPIO_CHECK_NOT_SEQUENTIAL_MODE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT_SIZE(adio_fh, count, datatype_size, myname, error_code);
    /* --END ERROR HANDLING-- */

    xbuf = buf;
    if (adio_fh->is_external32) {
        error_code = MPIU_external32_buffer_setup(buf, count, datatype, &e32buf);
        if (error_code != MPI_SUCCESS)
            goto fn_exit;

        xbuf = e32buf;
    }

    ADIO_IwriteStridedColl(adio_fh, xbuf, count, datatype, file_ptr_type,
                           offset, request, &error_code);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
    error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

fn_exit:
    if (e32buf != NULL) ADIOI_Free(e32buf);
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return error_code;
}
#endif
