/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_gpfs_tuning.c
 * \brief Defines ad_gpfs performance tuning
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 2008 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

/*---------------------------------------------------------------------
 * ad_gpfs_tuning.c
 *
 * defines global variables and functions for performance tuning and
 * functional debugging.
 *---------------------------------------------------------------------*/

#include "ad_gpfs_tuning.h"
#include "mpi.h"

#if !defined(PVFS2_SUPER_MAGIC)
  #define PVFS2_SUPER_MAGIC (0x20030528)
#endif


int 	gpfsmpio_timing;
int 	gpfsmpio_timing2;
int     gpfsmpio_timing_cw_level;
int 	gpfsmpio_comm;
int 	gpfsmpio_tunegather;
int 	gpfsmpio_tuneblocking;
long    bglocklessmpio_f_type;
int     gpfsmpio_bg_nagg_pset;
int     gpfsmpio_pthreadio;
int     gpfsmpio_p2pcontig;
int     gpfsmpio_write_aggmethod;
int     gpfsmpio_read_aggmethod;
int	gpfsmpio_balancecontig;
int     gpfsmpio_devnullio;
int     gpfsmpio_bridgeringagg;
int     gpfsmpio_onesided_no_rmw;
int     gpfsmpio_onesided_always_rmw;
int     gpfsmpio_onesided_inform_rmw;

double	gpfsmpio_prof_cw    [GPFSMPIO_CIO_LAST+1];
double	gpfsmpio_prof_cr    [GPFSMPIO_CIO_LAST+1];

/* set internal variables for tuning environment variables */
/** \page mpiio_vars MPIIO Configuration
  \section env_sec Environment Variables
 * - GPFSMPIO_COMM - Define how data is exchanged on collective
 *   reads and writes.  Possible values:
 *   - 0 - Use MPI_Alltoallv.
 *   - 1 - Use MPI_Isend/MPI_Irecv.
 *   - Default is 0.
 *
 * - GPFSMPIO_TIMING - collect timing breakdown for MPI I/O collective calls.
 *   Possible values:
 *   - 0 - Do not collect/report timing.
 *   - 1 - Collect/report timing.
 *   - Default is 0.
 *
 * - GPFSMPIO_TUNEGATHER - Tune how starting and ending offsets are communicated
 *   for aggregator collective i/o.  Possible values:
 *   - 0 - Use two MPI_Allgather's to collect starting and ending offsets.
 *   - 1 - Use MPI_Allreduce(MPI_MAX) to collect starting and ending offsets.
 *   - Default is 1.
 *
 * - GPFSMPIO_TUNEBLOCKING - Tune how aggregate file domains are
 *   calculated (block size).  Possible values:
 *   - 0 - Evenly calculate file domains across aggregators.  Also use
 *   MPI_Isend/MPI_Irecv to exchange domain information.
 *   - 1 - Align file domains with the underlying file system's block size.  Also use
 *   MPI_Alltoallv to exchange domain information.
 *   - Default is 1.
 *
 * - BGLOCKLESSMPIO_F_TYPE - Specify a filesystem type that should run
 *   the ad_bglockless driver.   NOTE: Using romio prefixes (such as
 *   "bg:" or "bglockless:") on a file name will override this environment
 *   variable.  Possible values:
 *   - 0xnnnnnnnn - Any valid file system type (or "magic number") from
 *                  statfs() field f_type.
 *   - The default is 0x20030528 (PVFS2_SUPER_MAGIC)
 *
 * - GPFSMPIO_NAGG_PSET - Specify a ratio of "I/O aggregators" to use for each
 *   compute group (compute nodes + i/o nodes).    Possible values:
 *   - any integer
 *   - Default is 8
 *
 * - GPFSMPIO_PTHREADIO - Enables a very simple form of asyncronous io where a
 *   pthread is spawned to do the posix writes while the main thread does the
 *   data aggregation - useful for large files where multiple rounds are
 *   required (more that the cb_buffer_size of data per aggregator).   User
 *   must ensure there is hw resource available for the thread to run.  I
 *   am sure there is a better way to do this involving comm threads - this is
 *   just a start.  NOTE: For some reason the stats collected when this is
 *   enabled misses some of the data so the data sizes are off a bit - this is
 *   a statistical issue only, the data is still accurately written out
 *
 * - GPFSMPIO_P2PCONTIG -  Does simple point-to-point communication between the
 *   aggregator and the procs that feed it.  Performance could be enhanced by a
 *   one-sided put algorithm.  Current implementation allows only 1 round of
 *   data.  Useful/allowed only when:
 * 1.) The datatype is contiguous.
 * 2.) The offsets are increasing in rank-order.
 * 3.) There are no gaps between the offsets.
 * 4.) No single rank has a data size which spans multiple file domains.
 *
 * - GPFSMPIO_WRITE_AGGMETHOD/GPFSMPIO_READ_AGGMETHOD -  Replaces the two-phase
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
 *   - 1 - A separate one-sided MPI_Put or MPI_Get is used for each contigous chunk of data
 *         for a compute to write to or read from the collective buffer on the aggregator.
 *   - 2 - An MPI derived datatype is created using all the contigous chunks and just one
 *         call to MPI_Put or MPI_Get is done with the derived datatype.  On Blue Gene /Q
 *         optimal performance for this is achieved when paired with PAMID_TYPED_ONESIDED=1.
 *   - Default is 0
 *
 * - GPFSMPIO_ONESIDED_NO_RMW - For one-sided aggregation (GPFSMPIO_WRITE_AGGMETHOD = 1 or 2)
 *   disable the detection of holes in the data when writing to a pre-existing
 *   file requiring a read-modify-write, thereby avoiding the communication
 *   overhead for this detection.
 *   - 0 (hole detection enabled) or 1 (hole detection disabled)
 *   - Default is 0
 *
 * - GPFSMPIO_ONESIDED_INFORM_RMW - For one-sided aggregation
 *   (GPFSMPIO_AGGMETHOD = 1 or 2) generate an informational message informing
 *   the user whether holes exist in the data when writing to a pre-existing
 *   file requiring a read-modify-write, thereby educating the user to set
 *   GPFSMPIO_ONESIDED_NO_RMW=1 on a future run to avoid the communication
 *   overhead for this detection.
 *   - 0 (disabled) or 1 (enabled)
 *   - Default is 0
 *
 * - GPFSMPIO_BALANCECONTIG -  Relevant only to BGQ.  File domain blocks are assigned
 *   to aggregators in a breadth-first fashion relative to the ions - additionally,
 *   file domains on the aggregators sharing the same bridgeset and ion have contiguous
 *   offsets.  The breadth-first assignment improves performance in the case of
 *   a relatively small file of size less than the gpfs block size multiplied
 *   by the number of ions. Files: ad_gpfs_aggrs.c ad_bg_aggrs.c.  Possible Values
 *   - 0 - assign file domain blocks in the traditional manner
 *   - 1 - if there are variable sized file domain blocks, spread them out
 *         (balance) across bridge nodes
 *
 * - GPFSMPIO_DEVNULLIO - do everything *except* write to / read from the file
 *   system. When experimenting with different two-phase I/O strategies, it's
 *   helpful to remove the highly variable file system from the experiment.
 *   - 0 (disabled) or 1 (enabled)
 *   - Default is 0
 *
 * - GPFSMPIO_BRIDGERINGAGG - Relevant only to BGQ.  Aggregator placement
 *   optimization whch forms a 5-d ring around the bridge node starting at
 *   GPFSMPIO_BRIDGERINGAGG hops away.  Experimental performance results
 *   suggest best value is 1 and only in conjunction with GPFSMPIO_P2PCONTIG
 *   and GPFSMPIO_BALANCECONTIG.  The number of aggregators selected is still
 *   GPFSMPIO_NAGG_PSET however the bridge node itself is NOT selected.
 *
 */

void ad_gpfs_get_env_vars() {
    char *x, *dummy;

    gpfsmpio_comm   = 0;
	x = getenv( "GPFSMPIO_COMM"         );
	if (x) gpfsmpio_comm         = atoi(x);
    gpfsmpio_timing = 0;
	x = getenv( "GPFSMPIO_TIMING"       );
	if (x) gpfsmpio_timing       = atoi(x);
    gpfsmpio_tunegather = 1;
	x = getenv( "GPFSMPIO_TUNEGATHER"   );
	if (x) gpfsmpio_tunegather   = atoi(x);
    gpfsmpio_tuneblocking = 1;
    x = getenv( "GPFSMPIO_TUNEBLOCKING" );
    if (x) gpfsmpio_tuneblocking = atoi(x);
    bglocklessmpio_f_type = PVFS2_SUPER_MAGIC;
    x = getenv( "BGLOCKLESSMPIO_F_TYPE" );
    if (x) bglocklessmpio_f_type = strtol(x,&dummy,0);
    DBG_FPRINTF(stderr,"BGLOCKLESSMPIO_F_TYPE=%ld/%#lX\n",
            bglocklessmpio_f_type,bglocklessmpio_f_type);
    /* note: this value will be 'sanity checked' in ADIOI_BG_persInfo_init(),
     * when we know a bit more about what "largest possible value" and
     * "smallest possible value" should be */
    gpfsmpio_bg_nagg_pset = ADIOI_BG_NAGG_PSET_DFLT;
    x = getenv("GPFSMPIO_NAGG_PSET");
    if (x) gpfsmpio_bg_nagg_pset = atoi(x);

    gpfsmpio_pthreadio = 0;
    x = getenv( "GPFSMPIO_PTHREADIO" );
    if (x) gpfsmpio_pthreadio = atoi(x);

    gpfsmpio_p2pcontig = 0;
    x = getenv( "GPFSMPIO_P2PCONTIG" );
    if (x) gpfsmpio_p2pcontig = atoi(x);

    gpfsmpio_write_aggmethod = 0;
    x = getenv( "GPFSMPIO_WRITE_AGGMETHOD" );
    if (x) gpfsmpio_write_aggmethod = atoi(x);

    gpfsmpio_read_aggmethod = 0;
    x = getenv( "GPFSMPIO_READ_AGGMETHOD" );
    if (x) gpfsmpio_read_aggmethod = atoi(x);

    gpfsmpio_balancecontig = 0;
    x = getenv( "GPFSMPIO_BALANCECONTIG" );
    if (x) gpfsmpio_balancecontig = atoi(x);

    gpfsmpio_devnullio = 0;
    x = getenv( "GPFSMPIO_DEVNULLIO" );
    if (x) gpfsmpio_devnullio = atoi(x);

    gpfsmpio_bridgeringagg = 0;
    x = getenv( "GPFSMPIO_BRIDGERINGAGG" );
    if (x) gpfsmpio_bridgeringagg = atoi(x);

    gpfsmpio_onesided_no_rmw = 0;
    x = getenv( "GPFSMPIO_ONESIDED_NO_RMW" );
    if (x) gpfsmpio_onesided_no_rmw = atoi(x);

    gpfsmpio_onesided_always_rmw = 0;
    x = getenv( "GPFSMPIO_ONESIDED_ALWAYS_RMW" );
    if (x) gpfsmpio_onesided_always_rmw = atoi(x);
    if (gpfsmpio_onesided_always_rmw)
      gpfsmpio_onesided_no_rmw = 1;

    gpfsmpio_onesided_inform_rmw = 0;
    x = getenv( "GPFSMPIO_ONESIDED_INFORM_RMW" );
    if (x) gpfsmpio_onesided_inform_rmw = atoi(x);
}

/* report timing breakdown for MPI I/O collective call */
void ad_gpfs_timing_crw_report( int rw, ADIO_File fd, int myrank, int nprocs )
{
    int i;

    if (gpfsmpio_timing) {
	/* Timing across the whole communicator is a little bit interesting,
	 * but what is *more* interesting is if we single out the aggregators
	 * themselves.  non-aggregators spend a lot of time in "exchange" not
	 * exchanging data, but blocked because they are waiting for
	 * aggregators to finish writing.  If we focus on just the aggregator
	 * processes we will get a more clear picture about the data exchange
	 * vs. i/o time breakdown */

	/* if deferred open enabled, we could use the aggregator communicator */
	MPI_Comm agg_comm;
	int nr_aggs, agg_rank;
	MPI_Comm_split(fd->comm, (fd->is_agg ? 1 : MPI_UNDEFINED), 0, &agg_comm);
	if(agg_comm != MPI_COMM_NULL) {
	    MPI_Comm_size(agg_comm, &nr_aggs);
	    MPI_Comm_rank(agg_comm, &agg_rank);
	}

	double *gpfsmpio_prof_org = gpfsmpio_prof_cr;
	if (rw) gpfsmpio_prof_org = gpfsmpio_prof_cw;

	double gpfsmpio_prof_avg[ GPFSMPIO_CIO_LAST ];
	double gpfsmpio_prof_max[ GPFSMPIO_CIO_LAST ];

	if( agg_comm != MPI_COMM_NULL) {
	    MPI_Reduce( gpfsmpio_prof_org, gpfsmpio_prof_avg, GPFSMPIO_CIO_LAST, MPI_DOUBLE, MPI_SUM, 0, agg_comm);
	    MPI_Reduce( gpfsmpio_prof_org, gpfsmpio_prof_max, GPFSMPIO_CIO_LAST, MPI_DOUBLE, MPI_MAX, 0, agg_comm);
	}
	if (agg_comm != MPI_COMM_NULL && agg_rank == 0) {

	    for (i=0; i<GPFSMPIO_CIO_LAST; i++) gpfsmpio_prof_avg[i] /= nr_aggs;

	    gpfsmpio_prof_avg[ GPFSMPIO_CIO_B_POSI_RW  ] =
		gpfsmpio_prof_avg[ GPFSMPIO_CIO_DATA_SIZE ] * nr_aggs /
		gpfsmpio_prof_max[ GPFSMPIO_CIO_T_POSI_RW  ];
	    gpfsmpio_prof_avg[ GPFSMPIO_CIO_B_MPIO_RW  ] =
		gpfsmpio_prof_avg[ GPFSMPIO_CIO_DATA_SIZE ] * nr_aggs /
		gpfsmpio_prof_max[ GPFSMPIO_CIO_T_MPIO_RW  ];

	    gpfsmpio_prof_avg[ GPFSMPIO_CIO_B_MPIO_CRW ] =
		gpfsmpio_prof_avg[ GPFSMPIO_CIO_DATA_SIZE ] * nr_aggs /
		gpfsmpio_prof_max[ GPFSMPIO_CIO_T_MPIO_CRW ];

	    fprintf(stderr,"TIMING-%1s,", (rw ? "W" : "R") );
	    fprintf(stderr,"SIZE: %12.4lld , ", (long long int)(gpfsmpio_prof_avg[ GPFSMPIO_CIO_DATA_SIZE ] * nr_aggs));
	    fprintf(stderr,"SEEK-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_SEEK ]     );
	    fprintf(stderr,"SEEK-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_SEEK ]     );
	    fprintf(stderr,"LOCAL-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_LCOMP ]    );
	    fprintf(stderr,"GATHER-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_GATHER ]   );
	    fprintf(stderr,"PATTERN-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_PATANA ]   );
	    fprintf(stderr,"FILEDOMAIN-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_FD_PART ]  );
	    fprintf(stderr,"MYREQ-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_MYREQ ]    );
	    fprintf(stderr,"OTHERREQ-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_OTHREQ ]   );
	    fprintf(stderr,"EXCHANGE-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_DEXCH ]    );
	    fprintf(stderr, "EXCHANGE-RECV_EXCH-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_DEXCH_RECV_EXCH]  );
	    fprintf(stderr, "EXCHANGE-SETUP-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_DEXCH_SETUP]  );
	    fprintf(stderr, "EXCHANGE-NET-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_DEXCH_NET]  );
	    fprintf(stderr, "EXCHANGE-SORT-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_DEXCH_SORT]  );
	    fprintf(stderr, "EXCHANGE-SIEVE-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_DEXCH_SIEVE]  );
	    fprintf(stderr,"POSIX-TIME-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_POSI_RW ]  );
	    fprintf(stderr,"POSIX-TIME-max: %10.3f , ",
		    gpfsmpio_prof_max[ GPFSMPIO_CIO_T_POSI_RW ]  );
	    fprintf(stderr,"MPIIO-CONTIG-TIME-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_MPIO_RW ]  );
	    fprintf(stderr,"MPIIO-STRIDED-TIME-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_T_MPIO_CRW ] );
	    fprintf(stderr,"POSIX-BW-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_B_POSI_RW ]  );
	    fprintf(stderr,"MPI-BW-avg: %10.3f , ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_B_MPIO_RW ]  );
	    fprintf(stderr,"MPI-BW-collective-avg: %10.3f\n ",
		    gpfsmpio_prof_avg[ GPFSMPIO_CIO_B_MPIO_CRW ] );
	}
	if (agg_comm != MPI_COMM_NULL) MPI_Comm_free(&agg_comm);
    }

}
