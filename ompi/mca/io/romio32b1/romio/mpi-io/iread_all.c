/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2014 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPIX_File_iread_all = PMPIX_File_iread_all
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPIX_File_iread_all MPIX_File_iread_all
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPIX_File_iread_all as PMPIX_File_iread_all
/* end of weak pragmas */
#elif defined(HAVE_WEAK_ATTRIBUTE)
int MPIX_File_iread_all(MPI_File fh, void *buf, int count, MPI_Datatype datatype,
                       MPI_Request *request)
    __attribute__((weak,alias("PMPIX_File_iread_all")));
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

#if HAVE_MPI_GREQUEST
#include "mpiu_greq.h"
#endif

/*@
    MPIX_File_iread_all - Nonblocking collective read using individual file pointer

Input Parameters:
. fh - file handle (handle)
. count - number of elements in buffer (nonnegative integer)
. datatype - datatype of each buffer element (handle)

Output Parameters:
. buf - initial address of buffer (choice)
. request - request object (handle)

.N fortran
@*/
int MPIX_File_iread_all(MPI_File fh, void *buf, int count,
                       MPI_Datatype datatype, MPI_Request *request)
{
    int error_code;
    static char myname[] = "MPIX_FILE_IREAD_ALL";
#ifdef MPI_hpux
    int fl_xmpi;

    HPMP_IO_START(fl_xmpi, BLKMPIFILEREADALL, TRDTBLOCK, fh, datatype, count);
#endif /* MPI_hpux */

    error_code = MPIOI_File_iread_all(fh, (MPI_Offset)0,
                     ADIO_INDIVIDUAL, buf,
                     count, datatype, myname, request);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS) {
        error_code = MPIO_Err_return_file(fh, error_code);
    }
    /* --END ERROR HANDLING-- */

#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, fh, datatype, count);
#endif /* MPI_hpux */

    return error_code;
}

/* Note: MPIOI_File_iread_all also used by MPIX_File_iread_at_all */
/* prevent multiple definitions of this routine */
#ifdef MPIO_BUILD_PROFILING
int MPIOI_File_iread_all(MPI_File fh,
            MPI_Offset offset,
            int file_ptr_type,
            void *buf,
            int count,
            MPI_Datatype datatype,
            char *myname,
            MPI_Request *request)
{
    int error_code;
    MPI_Count datatype_size;
    ADIO_File adio_fh;
    void *xbuf=NULL, *e32_buf=NULL;

    MPIU_THREAD_CS_ENTER(ALLFUNC,);

    adio_fh = MPIO_File_resolve(fh);

    /* --BEGIN ERROR HANDLING-- */
    MPIO_CHECK_FILE_HANDLE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT(adio_fh, count, myname, error_code);
    MPIO_CHECK_DATATYPE(adio_fh, datatype, myname, error_code);

    if (file_ptr_type == ADIO_EXPLICIT_OFFSET && offset < 0) {
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
    MPIO_CHECK_READABLE(adio_fh, myname, error_code);
    MPIO_CHECK_NOT_SEQUENTIAL_MODE(adio_fh, myname, error_code);
    MPIO_CHECK_COUNT_SIZE(adio_fh, count, datatype_size, myname, error_code);
    /* --END ERROR HANDLING-- */

    xbuf = buf;
    if (adio_fh->is_external32) {
        MPI_Aint e32_size = 0;
        error_code = MPIU_datatype_full_size(datatype, &e32_size);
        if (error_code != MPI_SUCCESS)
            goto fn_exit;

        e32_buf = ADIOI_Malloc(e32_size*count);
        xbuf = e32_buf;
    }

    ADIO_IreadStridedColl(adio_fh, xbuf, count, datatype, file_ptr_type,
                          offset, request, &error_code);

    /* --BEGIN ERROR HANDLING-- */
    if (error_code != MPI_SUCCESS)
    error_code = MPIO_Err_return_file(adio_fh, error_code);
    /* --END ERROR HANDLING-- */

    if (e32_buf != NULL) {
        error_code = MPIU_read_external32_conversion_fn(xbuf, datatype,
                                                        count, e32_buf);
        ADIOI_Free(e32_buf);
    }

fn_exit:
    MPIU_THREAD_CS_EXIT(ALLFUNC,);

    return error_code;
}
#endif
