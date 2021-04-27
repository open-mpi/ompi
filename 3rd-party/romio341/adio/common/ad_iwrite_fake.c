/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#include "adio.h"

#include "mpiu_greq.h"

/* Generic implementation of IwriteContig calls the blocking WriteContig
 * immediately.
 */
void ADIOI_FAKE_IwriteContig(ADIO_File fd, const void *buf, int count,
                             MPI_Datatype datatype, int file_ptr_type,
                             ADIO_Offset offset, ADIO_Request * request, int *error_code)
{
    ADIO_Status status;
    MPI_Count typesize;
    int write_count;
    MPI_Offset nbytes;

    /* Call the blocking function.  It will create an error code
     * if necessary.
     */
    ADIO_WriteContig(fd, buf, count, datatype, file_ptr_type, offset, &status, error_code);
    if (*error_code == MPI_SUCCESS) {
        MPI_Type_size_x(datatype, &typesize);
        MPI_Get_count(&status, datatype, &write_count);
        nbytes = write_count * typesize;
    } else {
        nbytes = 0;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);

}


/* Generic implementation of IwriteStrided calls the blocking WriteStrided
 * immediately.
 */
void ADIOI_FAKE_IwriteStrided(ADIO_File fd, const void *buf, int count,
                              MPI_Datatype datatype, int file_ptr_type,
                              ADIO_Offset offset, ADIO_Request * request, int *error_code)
{
    ADIO_Status status;
    MPI_Count typesize;
    int write_count;
    MPI_Offset nbytes;

    /* Call the blocking function.  It will create an error code
     * if necessary.
     */
    ADIO_WriteStrided(fd, buf, count, datatype, file_ptr_type, offset, &status, error_code);
    if (*error_code == MPI_SUCCESS) {
        MPI_Type_size_x(datatype, &typesize);
        MPI_Get_count(&status, datatype, &write_count);
        nbytes = write_count * typesize;
    } else {
        nbytes = 0;
    }
    MPIO_Completed_request_create(&fd, nbytes, error_code, request);
}
