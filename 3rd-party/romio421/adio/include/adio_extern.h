/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef ADIO_EXTERN_H_INCLUDED
#define ADIO_EXTERN_H_INCLUDED

extern ADIOI_Datarep *ADIOI_Datarep_head;

/* for f2c and c2f conversion */
extern ADIO_File *ADIOI_Ftable;
extern int ADIOI_Ftable_ptr, ADIOI_Ftable_max;
extern ADIO_Request *ADIOI_Reqtable;
extern int ADIOI_Reqtable_ptr, ADIOI_Reqtable_max;
#ifndef HAVE_MPI_INFO
extern MPI_Info *MPIR_Infotable;
extern int MPIR_Infotable_ptr, MPIR_Infotable_max;
#endif
#if defined(ROMIO_XFS) || defined(ROMIO_LUSTRE)
extern int ADIOI_Direct_read, ADIOI_Direct_write;
#endif

extern MPI_Errhandler ADIOI_DFLT_ERR_HANDLER;

extern MPI_Info ADIOI_syshints;

extern MPI_Op ADIO_same_amode;

extern int ADIOI_cb_config_list_keyval;
extern int ADIOI_Flattened_type_keyval;

#endif /* ADIO_EXTERN_H_INCLUDED */
