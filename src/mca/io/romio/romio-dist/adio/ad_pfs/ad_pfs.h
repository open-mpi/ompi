/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pfs.h,v 1.6 2002/10/24 17:00:50 gropp Exp $    
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

/* contains definitions, declarations, and macros specific to the
   implementation of ADIO on PFS */

#ifndef AD_PFS_INCLUDE
#define AD_PFS_INCLUDE

#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <nx.h>
#include <sys/uio.h>
#include "adio.h"

#ifdef tflops
#define lseek eseek
#define _gopen(n,m,i,p) open(n,m,p)
#endif

#endif
