/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: delete.c,v 1.12 2002/10/24 15:54:39 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "mpioimpl.h"

#ifdef HAVE_WEAK_SYMBOLS

#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_File_delete = PMPI_File_delete
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_File_delete MPI_File_delete
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_File_delete as PMPI_File_delete
/* end of weak pragmas */
#endif

/* Include mapping from MPI->PMPI */
#define MPIO_BUILD_PROFILING
#include "mpioprof.h"
#endif

extern int ADIO_Init_keyval;

/*@
    MPI_File_delete - Deletes a file

Input Parameters:
. filename - name of file to delete (string)
. info - info object (handle)

.N fortran
@*/
int MPI_File_delete(char *filename, MPI_Info info)
{
    int flag, error_code, file_system;
    char *tmp;
    ADIOI_Fns *fsops;
#ifndef PRINT_ERR_MSG
    static char myname[] = "MPI_FILE_DELETE";
#endif
#ifdef MPI_hpux
    int fl_xmpi;
  
    HPMP_IO_START(fl_xmpi, BLKMPIFILEDELETE, TRDTBLOCK,
                MPI_FILE_NULL, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */

    /* first check if ADIO has been initialized. If not, initialize it */
    if (ADIO_Init_keyval == MPI_KEYVAL_INVALID) {

   /* check if MPI itself has been initialized. If not, flag an error.
   Can't initialize it here, because don't know argc, argv */
        MPI_Initialized(&flag);
        if (!flag) {
            FPRINTF(stderr, "Error: MPI_Init() must be called before using MPI-IO\n");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }

        MPI_Keyval_create(MPI_NULL_COPY_FN, ADIOI_End_call, &ADIO_Init_keyval,
                          (void *) 0);  

   /* put a dummy attribute on MPI_COMM_WORLD, because we want the delete
   function to be called when MPI_COMM_WORLD is freed. Hopefully the
   MPI library frees MPI_COMM_WORLD when MPI_Finalize is called,
   though the standard does not mandate this. */

        MPI_Attr_put(MPI_COMM_WORLD, ADIO_Init_keyval, (void *) 0);

/* initialize ADIO */

        ADIO_Init( (int *)0, (char ***)0, &error_code);
    }


    /* resolve file system type from file name; this is a collective call */
    ADIO_ResolveFileType(MPI_COMM_SELF, filename, &file_system, &fsops, 
			 &error_code);
    if (error_code != MPI_SUCCESS) {
	/* ADIO_ResolveFileType() will print as informative a message as it
	 * possibly can or call MPIR_Err_setmsg.  We just need to propagate 
	 * the error up.  In the PRINT_ERR_MSG case MPI_Abort has already
	 * been called as well, so we probably didn't even make it this far.
	 */
#ifdef PRINT_ERR_MSG
	MPI_Abort(MPI_COMM_WORLD, 1); /* this is mostly here for clarity */
#else
	return ADIOI_Error(MPI_FILE_NULL, error_code, myname);
#endif
    }

    /* skip prefix on filename if there is one */
    tmp = strchr(filename, ':');
    /* Only skip prefixes greater than length one to allow for windows drive specification (c:\...)*/
    /*if (tmp) filename = tmp + 1;*/
    if (tmp > filename + 1)
	filename = tmp + 1;

    /* call the fs-specific delete function */
    (fsops->ADIOI_xxx_Delete)(filename, &error_code);
	
#ifdef MPI_hpux
    HPMP_IO_END(fl_xmpi, MPI_FILE_NULL, MPI_DATATYPE_NULL, -1);
#endif /* MPI_hpux */
    return error_code;
}
