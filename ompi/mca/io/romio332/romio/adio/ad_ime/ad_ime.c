/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *   Copyright (C) 1997 University of Chicago.
 *   Copyright (C) 2017 DataDirect Networks.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ad_ime.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_IME_operations = {
    ADIOI_IME_Open,     /* Open */
    ADIOI_SCALEABLE_OpenColl, /* OpenColl */ /*XXX*/
        ADIOI_IME_ReadContig,   /* ReadContig */
    ADIOI_IME_WriteContig,      /* WriteContig */
    ADIOI_GEN_ReadStridedColl,  /* ReadStridedColl */
    ADIOI_GEN_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual,   /* SeekIndividual */
    ADIOI_IME_Fcntl,    /* Fcntl */
    ADIOI_GEN_SetInfo,  /* SetInfo */
    ADIOI_GEN_ReadStrided,      /* ReadStrided */
    ADIOI_GEN_WriteStrided,     /* WriteStrided */
    ADIOI_IME_Close,    /* Close */
    ADIOI_FAKE_IreadContig,     /* IreadContig */
    ADIOI_FAKE_IwriteContig,    /* IwriteContig */
    ADIOI_FAKE_IODone,  /* ReadDone */
    ADIOI_FAKE_IODone,  /* WriteDone */
    ADIOI_FAKE_IOComplete,      /* ReadComplete */
    ADIOI_FAKE_IOComplete,      /* WriteComplete */
    ADIOI_FAKE_IreadStrided,    /* IreadStrided */
    ADIOI_FAKE_IwriteStrided,   /* IwriteStrided */
    ADIOI_IME_Flush,    /* Flush */
    ADIOI_IME_Resize,   /* Resize */
    ADIOI_IME_Delete,   /* Delete */
    ADIOI_IME_Feature,
    ADIOI_IME_PREFIX,
};
