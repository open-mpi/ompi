/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pvfs.c,v 1.3 2002/10/24 17:00:57 gropp Exp $
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pvfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_PVFS_operations = {
    ADIOI_PVFS_Open, /* Open */
    ADIOI_PVFS_ReadContig, /* ReadContig */
    ADIOI_PVFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_PVFS_SeekIndividual, /* SeekIndividual */
    ADIOI_PVFS_Fcntl, /* Fcntl */
    ADIOI_PVFS_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_PVFS_WriteStrided, /* WriteStrided */
    ADIOI_PVFS_Close, /* Close */
    ADIOI_PVFS_IreadContig, /* IreadContig */
    ADIOI_PVFS_IwriteContig, /* IwriteContig */
    ADIOI_PVFS_ReadDone, /* ReadDone */
    ADIOI_PVFS_WriteDone, /* WriteDone */
    ADIOI_PVFS_ReadComplete, /* ReadComplete */
    ADIOI_PVFS_WriteComplete, /* WriteComplete */
    ADIOI_PVFS_IreadStrided, /* IreadStrided */
    ADIOI_PVFS_IwriteStrided, /* IwriteStrided */
    ADIOI_PVFS_Flush, /* Flush */
    ADIOI_PVFS_Resize, /* Resize */
    ADIOI_PVFS_Delete, /* Delete */
};
