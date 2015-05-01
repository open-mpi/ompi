/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_pe_aggrs.h
 * \brief ???
 */

/*
 *
 * Declares functions specific for the PE platform within the GPFS
 * parallel I/O solution.  For now simply processes the MP_IOTASKLIST
 * env var.
 *
 */

#ifndef AD_PE_AGGRS_H_
#define AD_PE_AGGRS_H_

#include "adio.h"
#include <sys/stat.h>

#if !defined(GPFS_SUPER_MAGIC)
  #define GPFS_SUPER_MAGIC (0x47504653)
#endif

    /* generate a list of I/O aggregators following a methodology specific for PE */
    int ADIOI_PE_gen_agg_ranklist(ADIO_File fd);

#endif  /* AD_PE_AGGRS_H_ */
