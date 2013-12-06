/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bg.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2001 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */
#define BG_OPTIM_STEP1_1 1
#include "ad_bg.h"

/* adioi.h has the ADIOI_Fns_struct define */
#include "adioi.h"

struct ADIOI_Fns_struct ADIO_BG_operations = {
    ADIOI_BG_Open, /* Open */
    ADIOI_GEN_OpenColl, /* Collective open */
    ADIOI_BG_ReadContig, /* ReadContig */
    ADIOI_BG_WriteContig, /* WriteContig */
    ADIOI_BG_ReadStridedColl, /* ReadStridedColl */
    ADIOI_BG_WriteStridedColl, /* WriteStridedColl */
    ADIOI_GEN_SeekIndividual, /* SeekIndividual */
    ADIOI_BG_Fcntl, /* Fcntl */
    ADIOI_BG_SetInfo, /* SetInfo */
    ADIOI_BG_ReadStrided, /* ReadStrided */
    ADIOI_BG_WriteStrided, /* WriteStrided */
    ADIOI_BG_Close, /* Close */
#ifdef ROMIO_HAVE_WORKING_AIO
#warning Consider BG support for NFS before enabling this.
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
    ADIOI_GEN_Feature, /* Features */
};
