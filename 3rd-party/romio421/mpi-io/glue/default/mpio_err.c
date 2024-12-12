/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "mpioimpl.h"
#include "adio_extern.h"

#include <stdarg.h>
#include <stdio.h>

/* Default error handling implementation.
 *
 * Note that only MPI_ERRORS_ARE_FATAL and MPI_ERRORS_RETURN are
 * handled correctly; other handlers cause an abort.
 */

int MPIO_Err_create_code(int lastcode, int fatal, const char fcname[],
                         int line, int error_class, const char generic_msg[],
                         const char specific_msg[], ...)
{
    va_list Argp;
    int idx = 0;
    char *buf;

    buf = (char *) ADIOI_Malloc(1024);
    if (buf != NULL) {
        idx += snprintf(buf, 1023, "%s (line %d): ", fcname, line);
        if (specific_msg == NULL) {
            snprintf(&buf[idx], 1023 - idx, "%s\n", generic_msg);
        } else {
            va_start(Argp, specific_msg);
            vsnprintf(&buf[idx], 1023 - idx, specific_msg, Argp);
            va_end(Argp);
        }
        FPRINTF(stderr, "%s", buf);
        ADIOI_Free(buf);
    }

    return error_class;
}

int MPIO_Err_return_file(MPI_File mpi_fh, int error_code)
{
    ADIO_File adio_fh;

    if (mpi_fh == MPI_FILE_NULL) {
        if (ADIOI_DFLT_ERR_HANDLER != MPI_ERRORS_RETURN) {
            MPI_Abort(MPI_COMM_WORLD, 1);
        } else {
            return error_code;
        }
    }

    adio_fh = MPIO_File_resolve(mpi_fh);

    if (adio_fh->err_handler != MPI_ERRORS_RETURN) {
        MPI_Abort(MPI_COMM_WORLD, 1);
    } else {
        return error_code;
    }
}

int MPIO_Err_return_comm(MPI_Comm mpi_comm, int error_code)
{
    MPI_Errhandler errh;

    MPI_Comm_get_errhandler(mpi_comm, &errh);

    if (errh != MPI_ERRORS_RETURN) {
        MPI_Abort(mpi_comm, 1);
    }

    return error_code;
}
