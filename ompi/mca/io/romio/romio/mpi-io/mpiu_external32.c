/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*  (C) 2012 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpioimpl.h"
#include "mpiu_external32.h"

#ifdef HAVE_WEAK_SYMBOLS
/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

int MPIU_write_external32_conversion_fn (const void *userbuf, MPI_Datatype datatype,
        int count, void *filebuf)
{
    int position_i = 0;
    MPI_Aint position = 0;
    MPI_Aint bytes = 0;
    int mpi_errno = MPI_SUCCESS;
    int is_contig = 0;

    ADIOI_Datatype_iscontig(datatype, &is_contig);
    mpi_errno = MPI_Pack_external_size("external32", count, datatype, &bytes);
    if (mpi_errno != MPI_SUCCESS)
        goto fn_exit;

    if (is_contig)
    {
        mpi_errno = MPI_Pack_external("external32", userbuf, count,
                datatype, filebuf, bytes, &position);
        if (mpi_errno != MPI_SUCCESS)
            goto fn_exit;
    }
    else
    {
        void *tmp_buf = NULL;
        tmp_buf = ADIOI_Malloc(bytes);
        if (!tmp_buf)
        {
            mpi_errno = MPI_ERR_NO_MEM;
            goto fn_exit;
        }

        mpi_errno = MPI_Pack_external("external32", userbuf, count,
                datatype, tmp_buf, bytes, &position);
        if (mpi_errno != MPI_SUCCESS)
        {
            ADIOI_Free(tmp_buf);
            goto fn_exit;
        }

        mpi_errno = MPI_Unpack(tmp_buf, bytes, &position_i, filebuf, count,
                datatype, MPI_COMM_WORLD);
        if (mpi_errno != MPI_SUCCESS)
        {
            ADIOI_Free(tmp_buf);
            goto fn_exit;
        }

        ADIOI_Free(tmp_buf);
    }
fn_exit:
    return mpi_errno;
}

int MPIU_read_external32_conversion_fn(void *userbuf, MPI_Datatype datatype,
        int count, void *filebuf)
{
    int position_i = 0;
    MPI_Aint position = 0;
    MPI_Aint bytes = 0;
    int mpi_errno = MPI_SUCCESS;
    int is_contig = 0;

    ADIOI_Datatype_iscontig(datatype, &is_contig);
    mpi_errno = MPI_Pack_external_size("external32", count, datatype, &bytes);
    if (mpi_errno != MPI_SUCCESS)
        goto fn_exit;

    if (is_contig)
    {
        mpi_errno = MPI_Unpack_external("external32", filebuf, bytes,
                &position, userbuf, count,  datatype);
        if (mpi_errno != MPI_SUCCESS)
            goto fn_exit;
    }
    else
    {
        void *tmp_buf = NULL;
        tmp_buf = ADIOI_Malloc(bytes);
        if (!tmp_buf)
        {
            mpi_errno = MPI_ERR_NO_MEM;
            goto fn_exit;
        }

        mpi_errno = MPI_Pack(filebuf, count, datatype, tmp_buf, bytes,
                &position_i, MPI_COMM_WORLD);
        if (mpi_errno != MPI_SUCCESS)
        {
            ADIOI_Free(tmp_buf);
            goto fn_exit;
        }

        mpi_errno = MPI_Unpack_external("external32", tmp_buf, bytes,
                &position, userbuf, count, datatype);
        if (mpi_errno != MPI_SUCCESS)
        {
            ADIOI_Free(tmp_buf);
            goto fn_exit;
        }

        ADIOI_Free(tmp_buf);
    }
fn_exit:
    return mpi_errno;
}
int MPIU_datatype_full_size(MPI_Datatype datatype, MPI_Aint *size)
{
    int error_code = MPI_SUCCESS;
    MPI_Aint extent = 0;
    MPI_Aint true_extent = 0;
    MPI_Aint true_lb = 0;

    error_code = MPI_Type_get_true_extent(datatype, &true_lb, &true_extent);
    if (error_code != MPI_SUCCESS)
        goto fn_exit;

    *size = true_extent;
fn_exit:
    return error_code;
}

/* given a buffer, count, and datatype, return an apropriately allocated, sized
 * and external32-formatted buffer, suitable for handing off to a subsequent
 * write routine.  Caller is responsible for freeing 'newbuf' */
int MPIU_external32_buffer_setup(const void * buf, int count, MPI_Datatype type, void **newbuf)
{

    MPI_Aint datatype_size=0, bytes=0;
    int error_code;

    error_code = MPIU_datatype_full_size(type, &datatype_size);
    if (error_code != MPI_SUCCESS)
	return error_code;

    bytes = datatype_size * count;
    *newbuf = ADIOI_Malloc(bytes);

    error_code = MPIU_write_external32_conversion_fn(buf, type, count, *newbuf);
    if (error_code != MPI_SUCCESS) {
	ADIOI_Free(newbuf);
	return error_code;
    }
    return MPI_SUCCESS;
}


/*
 * vim: ts=8 sts=4 sw=4 noexpandtab
 */
