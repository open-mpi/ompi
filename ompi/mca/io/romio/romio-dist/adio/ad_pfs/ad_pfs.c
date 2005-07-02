/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   $Id: ad_pfs.c,v 1.3 2002/10/24 17:00:50 gropp Exp $
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_pfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_PFS_operations = {
    ADIOI_PFS_Open, /* Open */
    ADIOI_PFS_ReadContig, /* ReadContig */
    ADIOI_PFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_PFS_SeekIndividual, /* SeekIndividual */
    ADIOI_PFS_Fcntl, /* Fcntl */
    ADIOI_PFS_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_GEN_WriteStrided, /* WriteStrided */
    ADIOI_PFS_Close, /* Close */
    ADIOI_PFS_IreadContig, /* IreadContig */
    ADIOI_PFS_IwriteContig, /* IwriteContig */
    ADIOI_PFS_ReadDone, /* ReadDone */
    ADIOI_PFS_WriteDone, /* WriteDone */
    ADIOI_PFS_ReadComplete, /* ReadComplete */
    ADIOI_PFS_WriteComplete, /* WriteComplete */
    ADIOI_PFS_IreadStrided, /* IreadStrided */
    ADIOI_PFS_IwriteStrided, /* IwriteStrided */
    ADIOI_PFS_Flush, /* Flush */
    ADIOI_PFS_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
};
