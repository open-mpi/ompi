/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bg_tuning.h
 * \brief ???
 */

/*---------------------------------------------------------------------
 * ad_bg_tuning.h
 *
 * declares global variables and macros for performance tuning and
 * functional debugging.
 *---------------------------------------------------------------------*/

#ifndef AD_BG_TUNING_H_
#define AD_BG_TUNING_H_

#include "adio.h"

#define ADIOI_BG_assert( a ) if (!(a)) { \
                                fprintf( stderr, "AD_BG_assert, file=%s, line=%d\n", __FILE__, __LINE__ ); \
                                MPI_Abort( MPI_COMM_WORLD, 1 ); \
                           }


/*-----------------------------------------
 *  Global variables for the control of
 *  1.  timing
 *  2.  select specific optimizations
 *-----------------------------------------*/

/* timing fields */
enum {
    BGMPIO_CIO_DATA_SIZE=0,
    BGMPIO_CIO_T_SEEK,
    BGMPIO_CIO_T_LCOMP,	/* time for ADIOI_Calc_my_off_len(), local */
    BGMPIO_CIO_T_GATHER,	/* time for previous MPI_Allgather, now Allreduce */
    BGMPIO_CIO_T_PATANA,	/* time for a quick test if access is contiguous or not, local */
    BGMPIO_CIO_T_FD_PART,	/* time for file domain partitioning, local */
    BGMPIO_CIO_T_MYREQ,	/* time for ADIOI_BG_Calc_my_req(), local */
    BGMPIO_CIO_T_OTHREQ,	/* time for ADIOI_Calc_others_req(), short Alltoall */
    BGMPIO_CIO_T_DEXCH,	/* time for I/O data exchange */
    BGMPIO_CIO_T_POSI_RW,
    BGMPIO_CIO_B_POSI_RW,
    BGMPIO_CIO_T_MPIO_RW,	/* time for ADIOI_BG_WriteContig() */
    BGMPIO_CIO_B_MPIO_RW,
    BGMPIO_CIO_T_MPIO_CRW,	/* time for ADIOI_BG_WriteStridedColl() */
    BGMPIO_CIO_B_MPIO_CRW,
    BGMPIO_CIO_LAST
};

extern double 	bgmpio_prof_cw    [BGMPIO_CIO_LAST];
extern double 	bgmpio_prof_cr    [BGMPIO_CIO_LAST];


/* corresponds to environment variables to select optimizations and timing level */
extern int 	bgmpio_timing;
extern int 	bgmpio_timing2;
extern int 	bgmpio_comm;
extern int 	bgmpio_tunegather;
extern int 	bgmpio_tuneblocking;
extern long bglocklessmpio_f_type;


/* set internal variables for tuning environment variables */
void ad_bg_get_env_vars();

/* report timing breakdown for MPI I/O collective call */
void ad_bg_timing_crw_report( int rw, ADIO_File fd, int myrank, int nprocs );

/* note:
 *   T := timing;
 * CIO := collective I/O
 */
#define BGMPIO_T_CIO_RESET( LEVEL, RW ) \
	if (bgmpio_timing_cw_level >= LEVEL) { \
	  int i; \
	  for ( i = 0; i < BGMPIO_T_LAST; i ++ ) \
	    bgmpio_prof_c##RW [ i ] = 0; \
	}

#define BGMPIO_T_CIO_REPORT( LEVEL, RW, FD, MYRANK, NPROCS ) \
	if (bgmpio_timing_cw_level >= LEVEL) { \
	  ad_bg_timing_crw_report ( RW, FD, MYRANK, NPROCS ); \
	}

#define BGMPIO_T_CIO_SET_GET( LEVEL, RW, DOBAR, ISSET, ISGET, VAR1, VAR2 ) \
	if (bgmpio_timing_cw_level >= LEVEL) { \
	  if ( DOBAR ) MPI_Barrier( fd->comm ); \
	  double temp = MPI_Wtime(); \
	  if ( ISSET ) bgmpio_prof_c##RW [ VAR1 ] = temp; \
	  if ( ISGET ) bgmpio_prof_c##RW [ VAR2 ] = temp - bgmpio_prof_c##RW [ VAR2 ] ; \
	}

#endif  /* AD_BG_TUNING_H_ */
