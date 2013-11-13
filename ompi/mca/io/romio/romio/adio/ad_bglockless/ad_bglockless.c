/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 2001 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "../ad_bg/ad_bg.h"
#include "ad_bglockless.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_BGLOCKLESS_operations = {
    ADIOI_BG_Open, /* Open */
    ADIOI_GEN_OpenColl, /* Collective open */
    ADIOI_GEN_ReadContig, /* ReadContig */
    ADIOI_GEN_WriteContig, /* WriteContig */
    ADIOI_BG_ReadStridedColl, /* ReadStridedColl */
    ADIOI_BG_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_GEN_Fcntl, /* Fcntl */
    ADIOI_BG_SetInfo, /* SetInfo */
    ADIOI_GEN_ReadStrided, /* ReadStrided */
    ADIOI_NOLOCK_WriteStrided, /* WriteStrided */
    ADIOI_BG_Close, /* Close */
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
    ADIOI_BG_Flush, /* Flush */
    ADIOI_GEN_Resize, /* Resize */
    ADIOI_GEN_Delete, /* Delete */
    ADIOI_BGLOCKLESS_Feature  /* Features */
};
