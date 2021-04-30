/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"
#include "mpiu_greq.h"

/* Generic implementation of IreadContig calls the blocking ReadContig
 * immediately.
 */
void ADIOI_FAKE_IreadContig(ADIO_File fd, void *buf, int count,
                            MPI_Datatype datatype, int file_ptr_type,
                            ADIO_Offset offset, ADIO_Request * request, int *error_code)
{
    ADIO_Status status;
    MPI_Count typesize;
    int read_count;
    MPI_Offset nbytes;

    /* Call the blocking function.  It will create an error code
     * if necessary.
     */
    ADIO_ReadContig(fd, buf, count, datatype, file_ptr_type, offset, &status, error_code);
    if (*error_code == MPI_SUCCESS) {
        MPI_Type_size_x(datatype, &typesize);
        MPI_Get_count(&status, datatype, &read_count);
        nbytes = read_count * typesize;
    } else {
        nbytes = 0;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);
}


/* Generic implementation of IreadStrided calls the blocking ReadStrided
 * immediately.
 */
void ADIOI_FAKE_IreadStrided(ADIO_File fd, void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Request * request, int *error_code)
{
    ADIO_Status status;
    MPI_Count typesize;
    int read_count;
    MPI_Offset nbytes;

    /* Call the blocking function.  It will create an error code
     * if necessary.
     */
    ADIO_ReadStrided(fd, buf, count, datatype, file_ptr_type, offset, &status, error_code);
    if (*error_code == MPI_SUCCESS) {
        MPI_Type_size_x(datatype, &typesize);
        MPI_Get_count(&status, datatype, &read_count);
        nbytes = read_count * typesize;
    } else {
        nbytes = 0;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);
}
