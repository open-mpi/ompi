/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *   ad_panfs.c
 *
 *   Copyright (C) 2001 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_panfs.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_PANFS_operations = {
    ADIOI_PANFS_Open, /* Open */
    ADIOI_GEN_OpenColl,
    ADIOI_PANFS_ReadContig, /* ReadContig */
    ADIOI_PANFS_WriteContig, /* WriteContig */
    ADIOI_GEN_ReadStridedColl, /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_GEN_Fcntl, /* Fcntl */
    ADIOI_PANFS_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_GEN_WriteStrided, /* WriteStrided */
    ADIOI_GEN_Close, /* Close */
#ifdef ROMIO_HAVE_WORKING_AIO
    ADIOI_GEN_IreadContig, /* IreadContig */
    ADIOI_GEN_IwriteContig, /* IwriteContig */
#else
    ADIOI_FAKE_IreadContig, /* IreadContig */
    ADIOI_FAKE_IwriteContig, /* IwriteContig */
#endif
    ADIOI_GEN_IODone, /* ReadDone */
    ADIOI_GEN_IODone, /* WriteDone */
    ADIOI_GEN_IOComplete, /* ReadComplete */
    ADIOI_GEN_IOComplete, /* WriteComplete */
    ADIOI_GEN_IreadStrided, /* IreadStrided */
    ADIOI_GEN_IwriteStrided, /* IwriteStrided */
    ADIOI_GEN_Flush, /* Flush */
    ADIOI_PANFS_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
    ADIOI_GEN_Feature,
    "PANFS: Panasas PanFS"
};
