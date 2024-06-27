/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

/**
 * \file ad_tuning.c
 * \brief Defines common performance tuning env var options
 */

/*---------------------------------------------------------------------
 * ad_tuning.c
 *
 * defines common global variables and functions for performance tuning
 * and functional debugging.
 *---------------------------------------------------------------------*/

#include "ad_tuning.h"

int romio_write_aggmethod;
int romio_read_aggmethod;
int romio_onesided_no_rmw;
int romio_onesided_always_rmw;
int romio_onesided_inform_rmw;
int romio_tunegather;

/* set internal variables for tuning environment variables */
/** \page mpiio_vars MPIIO Configuration
  \section env_sec Environment Variables
 *
 * - ROMIO_WRITE_AGGMETHOD/ROMIO_READ_AGGMETHOD -  Replaces the two-phase
 *   collective IO aggregation
 *   with a one-sided algorithm, significantly reducing communication and
 *   memory overhead.  Fully
 *   supports all datasets and datatypes, the only caveat is that any holes in the data
 *   when writing to a pre-existing file are ignored -- there is no read-modify-write
 *   support to maintain the correctness of regions of pre-existing data so every byte
 *   must be explicitly written to maintain correctness.  Users must beware of middle-ware
 *   libraries like PNETCDF which may count on read-modify-write functionality for certain
 *   features (like fill values).  Possible values:
 *   - 0 - Normal two-phase collective IO is used.
 *   - 1 - A separate one-sided MPI_Put or MPI_Get is used for each contiguous chunk of data
 *         for a compute to write to or read from the collective buffer on the aggregator.
 *   - 2 - An MPI derived datatype is created using all the contiguous chunks and just one
 *         call to MPI_Put or MPI_Get is done with the derived datatype.  On Blue Gene /Q
 *         optimal performance for this is achieved when paired with PAMID_TYPED_ONESIDED=1.
 *   - Default is 0
 *
 * - ROMIO_ONESIDED_NO_RMW - For one-sided write aggregation (ROMIO_WRITE_AGGMETHOD = 1 or 2)
 *   disable the detection of holes in the data when writing to a pre-existing
 *   file requiring a read-modify-write, thereby avoiding the communication
 *   overhead for this detection.
 *   - 0 (hole detection enabled) or 1 (hole detection disabled)
 *   - Default is 0
 *
 * - ROMIO_ONESIDED_ALWAYS_RMW - For one-sided write aggregation (ROMIO_WRITE_AGGMETHOD = 1 or 2)
 *   always pre-read the offset range being written to a pre-existing file thereby filling
 *   any holes that may exist in the data before being written.
 *   - 0 (do not pre-read file offset range) or 1 (pre-read file offset range)
 *   - Default is 0
 *
 * - ROMIO_ONESIDED_INFORM_RMW - For one-sided aggregation
 *   (ROMIO_WRITE_AGGMETHOD = 1 or 2) generate an informational message informing
 *   the user whether holes exist in the data when writing to a pre-existing
 *   file requiring a read-modify-write, thereby educating the user to set
 *   ROMIO_ONESIDED_NO_RMW=1 on a future run to avoid the communication
 *   overhead for this detection.
 *   - 0 (disabled) or 1 (enabled)
 *   - Default is 0
 *
 * - ROMIO_TUNEGATHER - Tune how starting and ending offsets are communicated
 *   for aggregator collective i/o.  Possible values:
 *   - 0 - Use two or three MPI_Allgather's to collect starting and ending offsets.
 *   - 1 - Use MPI_Allreduce(MPI_MAX) to collect starting and ending offsets.
 *   - Default is 1.
 *
 */

void ad_get_env_vars(void)
{
    char *x;

    romio_write_aggmethod = 0;
    x = getenv("ROMIO_WRITE_AGGMETHOD");
    if (x)
        romio_write_aggmethod = atoi(x);

    romio_read_aggmethod = 0;
    x = getenv("ROMIO_READ_AGGMETHOD");
    if (x)
        romio_read_aggmethod = atoi(x);

    romio_onesided_no_rmw = 0;
    x = getenv("ROMIO_ONESIDED_NO_RMW");
    if (x)
        romio_onesided_no_rmw = atoi(x);

    romio_onesided_always_rmw = 0;
    x = getenv("ROMIO_ONESIDED_ALWAYS_RMW");
    if (x)
        romio_onesided_always_rmw = atoi(x);
    if (romio_onesided_always_rmw)
        romio_onesided_no_rmw = 1;

    romio_onesided_inform_rmw = 0;
    x = getenv("ROMIO_ONESIDED_INFORM_RMW");
    if (x)
        romio_onesided_inform_rmw = atoi(x);

    romio_tunegather = 1;
    x = getenv("ROMIO_TUNEGATHER");
    if (x)
        romio_tunegather = atoi(x);
}
