/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_hfs.c,v 1.3 2002/10/24 17:00:43 gropp Exp $
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_hfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_HFS_operations = {
    ADIOI_HFS_Open, /* Open */
    ADIOI_HFS_ReadContig, /* ReadContig */
    ADIOI_HFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_HFS_SeekIndividual, /* SeekIndividual */
    ADIOI_HFS_Fcntl, /* Fcntl */
    ADIOI_GEN_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_GEN_WriteStrided, /* WriteStrided */
    ADIOI_HFS_Close, /* Close */
    ADIOI_HFS_IreadContig, /* IreadContig */
    ADIOI_HFS_IwriteContig, /* IwriteContig */
    ADIOI_HFS_ReadDone, /* ReadDone */
    ADIOI_HFS_WriteDone, /* WriteDone */
    ADIOI_HFS_ReadComplete, /* ReadComplete */
    ADIOI_HFS_WriteComplete, /* WriteComplete */
    ADIOI_HFS_IreadStrided, /* IreadStrided */
    ADIOI_HFS_IwriteStrided, /* IwriteStrided */
    ADIOI_GEN_Flush, /* Flush */
    ADIOI_HFS_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
};
