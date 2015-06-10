/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_nfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_NFS_operations = {
    ADIOI_NFS_Open, /* Open */
    ADIOI_FAILSAFE_OpenColl, /* OpenColl */
    ADIOI_NFS_ReadContig, /* ReadContig */
    ADIOI_NFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_NFS_Fcntl, /* Fcntl */
    ADIOI_GEN_SetInfo, /* SetInfo */
    ADIOI_NFS_ReadStrided, /* ReadStrided */
    ADIOI_NFS_WriteStrided, /* WriteStrided */
    ADIOI_GEN_Close, /* Close */
    /* Even with lockd running and NFS mounted 'noac', we have been unable to
     * gaurantee correct behavior over NFS with asyncronous I/O operations */
    ADIOI_FAKE_IreadContig, /* IreadContig */
    ADIOI_FAKE_IwriteContig, /* IwriteContig */
    ADIOI_NFS_ReadDone, /* ReadDone */
    ADIOI_NFS_WriteDone, /* WriteDone */
    ADIOI_NFS_ReadComplete, /* ReadComplete */
    ADIOI_NFS_WriteComplete, /* WriteComplete */
    ADIOI_GEN_IreadStrided, /* IreadStrided */
    ADIOI_GEN_IwriteStrided, /* IwriteStrided */
    ADIOI_GEN_Flush, /* Flush */
    ADIOI_NFS_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
    ADIOI_NFS_Feature, /* Features */
    "NFS:"  /* fsname: just a string */
};
