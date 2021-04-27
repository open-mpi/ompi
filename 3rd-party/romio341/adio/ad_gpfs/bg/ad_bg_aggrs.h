/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/**
 * \file ad_bg_aggrs.h
 * \brief ???
 */

/*
 *
 * Declares functions specific for the BlueGene platform within the GPFS
 * parallel I/O solution.  Implements aligned file-domain partitioning
 * (7/28/2005);  persistent file doamin work not implemented
 *
 */

#ifndef AD_BG_AGGRS_H_INCLUDED
#define AD_BG_AGGRS_H_INCLUDED

#include "adio.h"
#include <sys/stat.h>

#ifdef HAVE_GPFS_H
#include <gpfs.h>
#endif
#if !defined(GPFS_SUPER_MAGIC)
#define GPFS_SUPER_MAGIC (0x47504653)
#endif

    /* generate a list of I/O aggregators that utilizes BG-PSET orginization. */
int ADIOI_BG_gen_agg_ranklist(ADIO_File fd, int n_aggrs_per_pset);

#endif /* AD_BG_AGGRS_H_INCLUDED */
