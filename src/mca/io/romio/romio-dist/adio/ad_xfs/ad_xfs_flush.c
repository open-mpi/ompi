/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_xfs_flush.c,v 1.3 2002/10/24 17:01:09 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_xfs.h"

void ADIOI_XFS_Flush(ADIO_File fd, int *error_code)
{
    ADIOI_GEN_Flush(fd, error_code);
}
