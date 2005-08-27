/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ufs_hints.c,v 1.3 2002/10/24 17:01:07 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ufs.h"

void ADIOI_UFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
    ADIOI_GEN_SetInfo(fd, users_info, error_code); 
}
