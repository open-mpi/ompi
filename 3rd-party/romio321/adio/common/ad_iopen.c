/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2002 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"

void ADIO_ImmediateOpen(ADIO_File fd, int *error_code)
{ 
	MPI_Comm tmp_comm;
	tmp_comm = fd->comm;
	/* some file systems might try to be clever inside their open routine.
	 * e.g. Blue Gene does a stat-and-broadcast */
	fd->comm = MPI_COMM_SELF;
	(*(fd->fns->ADIOI_xxx_Open))(fd, error_code);
	fd->is_open = 1;
	fd->comm = tmp_comm;

} 
