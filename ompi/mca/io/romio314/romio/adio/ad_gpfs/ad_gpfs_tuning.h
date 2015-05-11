/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_gpfs_tuning.h
 * \brief ???
 */

/*---------------------------------------------------------------------
 * ad_gpfs_tuning.h
 *
 * declares global variables and macros for performance tuning and
 * functional debugging.
 *---------------------------------------------------------------------*/

#ifndef AD_GPFS_TUNING_H_
#define AD_GPFS_TUNING_H_

#include "adio.h"


/*-----------------------------------------
 *  Global variables for the control of
 *  1.  timing
 *  2.  select specific optimizations
 *-----------------------------------------*/

/* timing fields */
enum {
    GPFSMPIO_CIO_DATA_SIZE=0,
    GPFSMPIO_CIO_T_SEEK,
    GPFSMPIO_CIO_T_LCOMP,	/* time for ADIOI_Calc_my_off_len(), local */
    GPFSMPIO_CIO_T_GATHER,	/* time for previous MPI_Allgather, now Allreduce */
    GPFSMPIO_CIO_T_PATANA,	/* time for a quick test if access is contiguous or not, local */
    GPFSMPIO_CIO_T_FD_PART,	/* time for file domain partitioning, local */
    GPFSMPIO_CIO_T_MYREQ,	/* time for ADIOI_Calc_my_req(), local */
    GPFSMPIO_CIO_T_OTHREQ,	/* time for ADIOI_Calc_others_req(), short Alltoall */
    GPFSMPIO_CIO_T_DEXCH,	/* time for I/O data exchange */
    /* the next DEXCH_* timers capture finer-grained portions of T_DEXCH */
    GPFSMPIO_CIO_T_DEXCH_RECV_EXCH,/* time for each process to exchange recieve
				    size info with everyone else */
    GPFSMPIO_CIO_T_DEXCH_SETUP,	/* time for setup portion of I/O data exchange */
    GPFSMPIO_CIO_T_DEXCH_NET,	/* time for network portion of I/O data exchange */
    GPFSMPIO_CIO_T_DEXCH_SORT, 	/* time to sort requesst in I/O data exchange */
    GPFSMPIO_CIO_T_DEXCH_SIEVE, 	/* time for read portion of RMW in two phase */
    GPFSMPIO_CIO_T_POSI_RW,
    GPFSMPIO_CIO_B_POSI_RW,
    GPFSMPIO_CIO_T_MPIO_RW,	/* time for ADIOI_WriteContig() */
    GPFSMPIO_CIO_B_MPIO_RW,
    GPFSMPIO_CIO_T_MPIO_CRW,	/* time for ADIOI_GPFS_WriteStridedColl() */
    GPFSMPIO_CIO_B_MPIO_CRW,
    GPFSMPIO_CIO_LAST
};

/* +1 because GPFSMPIO_CIO_LAST is actually used to say "zero this counter"" */
extern double 	gpfsmpio_prof_cw    [GPFSMPIO_CIO_LAST+1];
extern double 	gpfsmpio_prof_cr    [GPFSMPIO_CIO_LAST+1];


/* corresponds to environment variables to select optimizations and timing level */
extern int 	gpfsmpio_timing;
extern int      gpfsmpio_timing_cw_level;
extern int 	gpfsmpio_comm;
extern int 	gpfsmpio_tunegather;
extern int 	gpfsmpio_tuneblocking;
extern long bglocklessmpio_f_type;
extern int      gpfsmpio_pthreadio;
extern int      gpfsmpio_p2pcontig;
extern int  gpfsmpio_balancecontig;
extern int      gpfsmpio_devnullio;
extern int      gpfsmpio_bridgeringagg;

/* Default is, well, kind of complicated. Blue Gene /L and /P had "psets": one
 * i/o node and all compute nodes wired to it.  On Blue Gene /Q that
 * relationship is a lot more fluid.  There are still I/O nodes, and compute
 * nodes are assigned to an i/o node, but there are two routes to the i/o node,
 * via compute nodes designated as "bridge nodes".  In this code, what we used
 * to call a "pset" is actually "compute nodes associated with and including a
 * bridge node".  So, "nAgg" is roughly "number of aggregators per bridge", but
 * look closely at ADIOI_BG_persInfo_init() for the details */

#define ADIOI_BG_NAGG_PSET_DFLT 16

extern int     gpfsmpio_bg_nagg_pset;


/* set internal variables for tuning environment variables */
void ad_gpfs_get_env_vars(void);

/* report timing breakdown for MPI I/O collective call */
void ad_gpfs_timing_crw_report( int rw, ADIO_File fd, int myrank, int nprocs );

/* note:
 *   T := timing;
 * CIO := collective I/O
 */
#define GPFSMPIO_T_CIO_RESET( RW ) \
	{ \
	  int _i; \
	  for ( _i = 0; _i < GPFSMPIO_CIO_LAST; _i ++ ) \
	    gpfsmpio_prof_c##RW [ _i ] = 0; \
	}

#define GPFSMPIO_T_CIO_REPORT( RW, FD, MYRANK, NPROCS ) \
	ad_gpfs_timing_crw_report ( RW, FD, MYRANK, NPROCS ); \

#define GPFSMPIO_T_CIO_SET_GET(RW, ISSET, ISGET, VAR1, VAR2 ) \
         {\
	 double temp = MPI_Wtime(); \
	 if ( ISSET ) gpfsmpio_prof_c##RW [ VAR1 ] = temp; \
	 if ( ISGET ) gpfsmpio_prof_c##RW [ VAR2 ] = temp - gpfsmpio_prof_c##RW [ VAR2 ] ;\
	 }

#endif  /* AD_GPFS_TUNING_H_ */
