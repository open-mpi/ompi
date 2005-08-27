/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_nfs.h,v 1.6 2002/10/24 17:00:46 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef AD_NFS_INCLUDE
#define AD_NFS_INCLUDE

#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include "adio.h"

#ifndef NO_AIO
#ifdef AIO_SUN
#include <sys/asynch.h>
#else
#include <aio.h>
#ifdef NEEDS_ADIOCB_T
typedef struct adiocb adiocb_t;
#endif
#endif
#endif

int ADIOI_NFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
                  int wr, void *handle);

#ifdef SX4
#define lseek llseek
#endif

#endif
