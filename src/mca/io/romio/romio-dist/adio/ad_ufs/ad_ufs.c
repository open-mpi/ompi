/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_ufs.c,v 1.3 2002/10/24 17:01:06 gropp Exp $
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ufs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_UFS_operations = {
    ADIOI_UFS_Open, /* Open */
    ADIOI_UFS_ReadContig, /* ReadContig */
    ADIOI_UFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_UFS_Fcntl, /* Fcntl */
    ADIOI_GEN_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_GEN_WriteStrided, /* WriteStrided */
    ADIOI_UFS_Close, /* Close */
    ADIOI_UFS_IreadContig, /* IreadContig */
    ADIOI_UFS_IwriteContig, /* IwriteContig */
    ADIOI_UFS_ReadDone, /* ReadDone */
    ADIOI_UFS_WriteDone, /* WriteDone */
    ADIOI_UFS_ReadComplete, /* ReadComplete */
    ADIOI_UFS_WriteComplete, /* WriteComplete */
    ADIOI_UFS_IreadStrided, /* IreadStrided */
    ADIOI_UFS_IwriteStrided, /* IwriteStrided */
    ADIOI_GEN_Flush, /* Flush */
    ADIOI_UFS_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
};
