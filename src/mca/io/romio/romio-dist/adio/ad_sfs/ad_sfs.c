/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_sfs.c,v 1.3 2002/10/24 17:01:00 gropp Exp $
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_sfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_SFS_operations = {
    ADIOI_SFS_Open, /* Open */
    ADIOI_SFS_ReadContig, /* ReadContig */
    ADIOI_SFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_SFS_Fcntl, /* Fcntl */
    ADIOI_GEN_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_GEN_WriteStrided, /* WriteStrided */
    ADIOI_SFS_Close, /* Close */
    ADIOI_SFS_IreadContig, /* IreadContig */
    ADIOI_SFS_IwriteContig, /* IwriteContig */
    ADIOI_SFS_ReadDone, /* ReadDone */
    ADIOI_SFS_WriteDone, /* WriteDone */
    ADIOI_SFS_ReadComplete, /* ReadComplete */
    ADIOI_SFS_WriteComplete, /* WriteComplete */
    ADIOI_SFS_IreadStrided, /* IreadStrided */
    ADIOI_SFS_IwriteStrided, /* IwriteStrided */
    ADIOI_SFS_Flush, /* Flush */
    ADIOI_SFS_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
};
