/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ntfs.h"

void ADIOI_NTFS_WriteContig(ADIO_File fd, void *buf, int count, 
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, ADIO_Status *status,
			    int *error_code)
{
    static char myname[] = "ADIOI_NTFS_WriteContig";
    LONG dwTemp;
    DWORD dwNumWritten = 0;
    MPI_Count err=-1, datatype_size, len;
    OVERLAPPED *pOvl;
    
    /* If file pointer type in ADIO_INDIVIDUAL then offset should be
	ignored and the current location of file pointer should be used */
    if(file_ptr_type == ADIO_INDIVIDUAL){
	offset = fd->fp_ind;
    }

    MPI_Type_size_x(datatype, &datatype_size);
    len = datatype_size * count;

    pOvl = (OVERLAPPED *) ADIOI_Calloc(sizeof(OVERLAPPED), 1);
    if (pOvl == NULL)
    {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
	    myname, __LINE__, MPI_ERR_IO,
	    "**nomem", "**nomem %s", "OVERLAPPED");
	return;
    }
    pOvl->hEvent = CreateEvent(NULL, TRUE, TRUE, NULL);
    if (pOvl->hEvent == NULL)
    {
    char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	err = GetLastError();
    ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
	    myname, __LINE__, MPI_ERR_IO,
	    "**io", "**io %s", errMsg);
	ADIOI_Free(pOvl);
	return;
    }
    pOvl->Offset = DWORDLOW(offset);
    pOvl->OffsetHigh = DWORDHIGH(offset);

    if (file_ptr_type == ADIO_EXPLICIT_OFFSET)
    {
	if (fd->fp_sys_posn != offset)
	{
	    dwTemp = DWORDHIGH(offset);
	    if (SetFilePointer(fd->fd_sys, DWORDLOW(offset), &dwTemp, FILE_BEGIN) == INVALID_SET_FILE_POINTER)
	    {
		err = GetLastError();
		if (err != NO_ERROR)
		{
            char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
            ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
		    *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
			myname, __LINE__, MPI_ERR_IO,
			"**io", "**io %s", errMsg);
		    CloseHandle(pOvl->hEvent);
		    ADIOI_Free(pOvl);
		    return;
		}
	    }
	}
	/*printf("WriteFile(%d bytes)\n", len);fflush(stdout);*/
	err = WriteFile(fd->fd_sys, buf, len, &dwNumWritten, pOvl);
	/* --BEGIN ERROR HANDLING-- */
	if (err == FALSE)
	{
	    err = GetLastError();
	    if (err != ERROR_IO_PENDING)
	    {
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
        ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
		*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
		    myname, __LINE__, MPI_ERR_IO,
		    "**io",
		    "**io %s", errMsg);
		CloseHandle(pOvl->hEvent);
		ADIOI_Free(pOvl);
		return;
	    }
	}
	/* --END ERROR HANDLING-- */
	err = GetOverlappedResult(fd->fd_sys, pOvl, &dwNumWritten, TRUE);
	/* --BEGIN ERROR HANDLING-- */
	if (err == FALSE)
	{
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	    err = GetLastError();
        ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
		MPIR_ERR_RECOVERABLE, myname,
		__LINE__, MPI_ERR_IO, "**io",
		"**io %s", errMsg);
	    CloseHandle(pOvl->hEvent);
	    ADIOI_Free(pOvl);
	    return;
	}
	/* --END ERROR HANDLING-- */
	if (!CloseHandle(pOvl->hEvent))
	{
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	    err = GetLastError();
        ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
		myname, __LINE__, MPI_ERR_IO,
		"**io", "**io %s", errMsg);
	    CloseHandle(pOvl->hEvent);
	    ADIOI_Free(pOvl);
	    return;
	}
	ADIOI_Free(pOvl);

	fd->fp_sys_posn = offset + dwNumWritten;
	/* individual file pointer not updated */        
    }
    else
    {
	/* write from curr. location of ind. file pointer */
	if (fd->fp_sys_posn != fd->fp_ind)
	{
	    dwTemp = DWORDHIGH(fd->fp_ind);
	    if (SetFilePointer(fd->fd_sys, DWORDLOW(fd->fp_ind), &dwTemp, FILE_BEGIN) == INVALID_SET_FILE_POINTER)
	    {
		err = GetLastError();
		if (err != NO_ERROR)
		{
            char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
            ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
		    *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
			myname, __LINE__, MPI_ERR_IO,
			"**io", "**io %s", errMsg);
		    CloseHandle(pOvl->hEvent);
		    ADIOI_Free(pOvl);
		    return;
		}
	    }
	}
	/*printf("WriteFile(%d bytes)\n", len);fflush(stdout);*/
	err = WriteFile(fd->fd_sys, buf, len, &dwNumWritten, pOvl);
	/* --BEGIN ERROR HANDLING-- */
	if (err == FALSE)
	{
	    err = GetLastError();
	    if (err != ERROR_IO_PENDING)
	    {
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
        ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
		*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
		    myname, __LINE__, MPI_ERR_IO,
		    "**io",
		    "**io %s", errMsg);
		CloseHandle(pOvl->hEvent);
		ADIOI_Free(pOvl);
		return;
	    }
	}
	/* --END ERROR HANDLING-- */
	err = GetOverlappedResult(fd->fd_sys, pOvl, &dwNumWritten, TRUE);
	/* --BEGIN ERROR HANDLING-- */
	if (err == FALSE)
	{
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	    err = GetLastError();
        ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS,
		MPIR_ERR_RECOVERABLE, myname,
		__LINE__, MPI_ERR_IO, "**io",
		"**io %s", errMsg);
	    CloseHandle(pOvl->hEvent);
	    ADIOI_Free(pOvl);
	    return;
	}
	/* --END ERROR HANDLING-- */
	if (!CloseHandle(pOvl->hEvent))
	{
        char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	    err = GetLastError();
        ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	    *error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
		myname, __LINE__, MPI_ERR_IO,
		"**io", "**io %s", errMsg);
	    ADIOI_Free(pOvl);
	    return;
	}
	ADIOI_Free(pOvl);

	fd->fp_ind = fd->fp_ind + dwNumWritten;
	fd->fp_sys_posn = fd->fp_ind;
    }

#ifdef HAVE_STATUS_SET_BYTES
    if (err != FALSE)
    {
	MPIR_Status_set_bytes(status, datatype, dwNumWritten);
    }
#endif

    /* --BEGIN ERROR HANDLING-- */
    if (err == FALSE)
    {
    char errMsg[ADIOI_NTFS_ERR_MSG_MAX];
	err = GetLastError();
    ADIOI_NTFS_Strerror(err, errMsg, ADIOI_NTFS_ERR_MSG_MAX);
	*error_code = MPIO_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE,
					   myname, __LINE__, MPI_ERR_IO,
					   "**io",
					   "**io %s", errMsg);
	return;
    }
    /* --END ERROR HANDLING-- */
    *error_code = MPI_SUCCESS;
}
