/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#ifndef AD_NTFS_INCLUDE
#define AD_NTFS_INCLUDE

#include <sys/types.h>
#include <fcntl.h>
#include "adio.h"

#ifdef HAVE_INT64
#define DWORDLOW(x)        ( (DWORD) ( x & (__int64) 0xFFFFFFFF ) )
#define DWORDHIGH(x)       ( (DWORD) ( (x >> 32) & (__int64) 0xFFFFFFFF ) )
#define DWORDTOINT64(x,y)  ( (__int64) ( ( (__int64 x) << 32 ) + (__int64) y ) )
#else
#define DWORDLOW(x)         x
#define DWORDHIGH(x)        0
#define DWORDTOINT64(x,y)   x
#endif

int ADIOI_NTFS_aio(ADIO_File fd, void *buf, int len, ADIO_Offset offset,
		  int wr, void *handle);

#endif
