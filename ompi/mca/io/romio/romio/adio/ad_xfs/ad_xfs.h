/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_xfs.h,v 1.6 2002/10/24 17:01:08 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef AD_XFS_INCLUDE
#define AD_XFS_INCLUDE

#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include "adio.h"
#include <aio.h>

int ADIOI_XFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		  int wr, void *handle);

#if (defined(HAVE_PREAD64) && (_ABIO32 == 1))
#  define pread pread64
#  define pwrite pwrite64
#endif
/* above needed for IRIX 6.5 */

#endif
