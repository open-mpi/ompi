/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 1997-2001 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

#include <pthread.h>
/* Function for running in another thread for doing the file reading while the
 * main thread is doing data aggregation - useful only when multiple rounds are
 * needed due to file size relative to the read buffer size and number of
 * aggregators */

void *ADIOI_IO_Thread_Func(void *vptr_args) {
    ADIOI_IO_ThreadFuncData *args = (ADIOI_IO_ThreadFuncData*)vptr_args;

    ADIOI_Assert(args->size == (int)(args->size));

    if (args->io_kind == ADIOI_READ) {
	ADIO_ReadContig(args->fd, args->buf, args->size, MPI_BYTE,
		ADIO_EXPLICIT_OFFSET, args->offset,
		&(args->status), &(args->error_code));
    } else {
	ADIO_WriteContig(args->fd, args->buf, args->size, MPI_BYTE,
		ADIO_EXPLICIT_OFFSET, args->offset,
		&(args->status), &(args->error_code));
    }
    pthread_exit(&(args->error_code));
    return NULL;
}
