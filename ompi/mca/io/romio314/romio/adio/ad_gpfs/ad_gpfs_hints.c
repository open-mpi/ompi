/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_gpfs_hints.c
 * \brief GPFS hint processing - for now, only used for BlueGene and PE platforms
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *   Copyright (C) 1997 University of Chicago.
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "hint_fns.h"

#include "ad_gpfs.h"

#define   ADIOI_GPFS_CB_BUFFER_SIZE_DFLT      	"16777216"
#define	  ADIOI_GPFS_IND_RD_BUFFER_SIZE_DFLT	"4194304"
#define   ADIOI_GPFS_IND_WR_BUFFER_SIZE_DFLT	"4194304"

#ifdef BGQPLATFORM
#define   ADIOI_BG_NAGG_IN_PSET_HINT_NAME	"bg_nodes_pset"
#endif

/** \page mpiio_vars MPIIO Configuration
 *
 * GPFS MPIIO configuration and performance tuning. Used by ad_gpfs ADIO.
 *
 * Used for BlueGene and PE platforms, which each have their own aggregator selection
 * algorithms that ignore user provided cb_config_list.
 *
 * \section hint_sec Hints
 * - bg_nodes_pset - BlueGene only - specify how many aggregators to use per pset.
 *   This hint will override the cb_nodes hint based on BlueGene psets.
 *   - N - Use N nodes per pset as aggregators.
 *   - Default is based on partition configuration and cb_nodes.
 *
 *   The following default key/value pairs may differ from other platform defaults.
 *
 *     - key = cb_buffer_size     value = 16777216
 *     - key = romio_cb_read      value = enable
 *     - key = romio_cb_write     value = enable
 *     - key = ind_rd_buffer_size value = 4194304
 *     - key = ind_wr_buffer_size value = 4194304
 */

#ifdef BGQPLATFORM
/* Compute the aggregator-related parameters that are required in 2-phase collective IO of ADIO. */
extern int
ADIOI_BG_gen_agg_ranklist(ADIO_File fd, int n_proxy_per_pset);
#elif PEPLATFORM
extern int
ADIOI_PE_gen_agg_ranklist(ADIO_File fd);
#endif

void ADIOI_GPFS_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
/* if fd->info is null, create a new info object.
   Initialize fd->info to default values.
   Initialize fd->hints to default values.
   Examine the info object passed by the user. If it contains values that
   ROMIO understands, override the default. */

    MPI_Info info;
    char *value;
    int flag, intval, nprocs=0, nprocs_is_valid = 0;
    static char myname[] = "ADIOI_GPFS_SETINFO";

    int did_anything = 0;

    if (fd->info == MPI_INFO_NULL) MPI_Info_create(&(fd->info));
    info = fd->info;

    /* Note that fd->hints is allocated at file open time; thus it is
     * not necessary to allocate it, or check for allocation, here.
     */

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));
    ADIOI_Assert ((value != NULL));

    /* initialize info and hints to default values if they haven't been
     * previously initialized
     */
    if (!fd->hints->initialized) {

	ad_gpfs_get_env_vars();
	did_anything = 1;

	/* buffer size for collective I/O */
	ADIOI_Info_set(info, "cb_buffer_size", ADIOI_GPFS_CB_BUFFER_SIZE_DFLT);
	fd->hints->cb_buffer_size = atoi(ADIOI_GPFS_CB_BUFFER_SIZE_DFLT);

	/* default is to let romio automatically decide when to use
	 * collective buffering
	 */
	ADIOI_Info_set(info, "romio_cb_read", "enable");
	fd->hints->cb_read = ADIOI_HINT_ENABLE;
	ADIOI_Info_set(info, "romio_cb_write", "enable");
	fd->hints->cb_write = ADIOI_HINT_ENABLE;

   	if ( fd->hints->cb_config_list != NULL ) ADIOI_Free (fd->hints->cb_config_list);
	fd->hints->cb_config_list = NULL;

	/* number of processes that perform I/O in collective I/O */
	MPI_Comm_size(fd->comm, &nprocs);
	nprocs_is_valid = 1;
	ADIOI_Snprintf(value, MPI_MAX_INFO_VAL+1, "%d", nprocs);
	ADIOI_Info_set(info, "cb_nodes", value);
	fd->hints->cb_nodes = -1;

	/* hint indicating that no indep. I/O will be performed on this file */
	ADIOI_Info_set(info, "romio_no_indep_rw", "false");
	fd->hints->no_indep_rw = 0;

	/* gpfs is not implementing file realms (ADIOI_IOStridedColl),
	   initialize to disabled it. 	   */
	/* hint instructing the use of persistent file realms */
	ADIOI_Info_set(info, "romio_cb_pfr", "disable");
	fd->hints->cb_pfr = ADIOI_HINT_DISABLE;

	/* hint guiding the assignment of persistent file realms */
	ADIOI_Info_set(info, "romio_cb_fr_types", "aar");
	fd->hints->cb_fr_type = ADIOI_FR_AAR;

	/* hint to align file realms with a certain byte value */
	ADIOI_Info_set(info, "romio_cb_fr_alignment", "1");
	fd->hints->cb_fr_alignment = 1;

	/* hint to set a threshold percentage for a datatype's size/extent at
	 * which data sieving should be done in collective I/O */
	ADIOI_Info_set(info, "romio_cb_ds_threshold", "0");
	fd->hints->cb_ds_threshold = 0;

	/* hint to switch between point-to-point or all-to-all for two-phase */
	ADIOI_Info_set(info, "romio_cb_alltoall", "automatic");
	fd->hints->cb_alltoall = ADIOI_HINT_AUTO;

	 /* deferred_open derived from no_indep_rw and cb_{read,write} */
	fd->hints->deferred_open = 0;

	/* buffer size for data sieving in independent reads */
	ADIOI_Info_set(info, "ind_rd_buffer_size", ADIOI_GPFS_IND_RD_BUFFER_SIZE_DFLT);
	fd->hints->ind_rd_buffer_size = atoi(ADIOI_GPFS_IND_RD_BUFFER_SIZE_DFLT);

	/* buffer size for data sieving in independent writes */
	ADIOI_Info_set(info, "ind_wr_buffer_size", ADIOI_GPFS_IND_WR_BUFFER_SIZE_DFLT);
	fd->hints->ind_wr_buffer_size = atoi(ADIOI_GPFS_IND_WR_BUFFER_SIZE_DFLT);


    ADIOI_Info_set(info, "romio_ds_read", "automatic");
    fd->hints->ds_read = ADIOI_HINT_AUTO;
    ADIOI_Info_set(info, "romio_ds_write", "automatic");
    fd->hints->ds_write = ADIOI_HINT_AUTO;

    /* still to do: tune this a bit for a variety of file systems. there's
	 * no good default value so just leave it unset */
    fd->hints->min_fdomain_size = 0;
    fd->hints->striping_unit = 0;

    fd->hints->initialized = 1;
    }

    /* add in user's info if supplied */
    if (users_info != MPI_INFO_NULL) {
	ADIOI_Info_check_and_install_int(fd, users_info, "cb_buffer_size",
		&(fd->hints->cb_buffer_size), myname, error_code);
	/* new hints for enabling/disabling coll. buffering on
	 * reads/writes
	 */
	ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_cb_read",
		&(fd->hints->cb_read), myname, error_code);
	if (fd->hints->cb_read == ADIOI_HINT_DISABLE) {
	    /* romio_cb_read overrides no_indep_rw */
	    ADIOI_Info_set(info, "romio_no_indep_rw", "false");
	    fd->hints->no_indep_rw = ADIOI_HINT_DISABLE;
	}
	ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_cb_write",
		&(fd->hints->cb_write), myname, error_code);
	if (fd->hints->cb_write == ADIOI_HINT_DISABLE) {
	    /* romio_cb_write overrides no_indep_rw */
	    ADIOI_Info_set(info, "romio_no_indep_rw", "false");
	    fd->hints->no_indep_rw = ADIOI_HINT_DISABLE;
	}
	/* Has the user indicated all I/O will be done collectively? */
	ADIOI_Info_check_and_install_true(fd, users_info, "romio_no_indep_rw",
		&(fd->hints->no_indep_rw), myname, error_code);
	if (fd->hints->no_indep_rw == 1) {
	    /* if 'no_indep_rw' set, also hint that we will do
	     * collective buffering: if we aren't doing independent io,
	     * then we have to do collective  */
	    ADIOI_Info_set(info, "romio_cb_write", "enable");
	    ADIOI_Info_set(info, "romio_cb_read", "enable");
	    fd->hints->cb_read = 1;
	    fd->hints->cb_write = 1;
	}

	/* new hints for enabling/disabling data sieving on
	 * reads/writes
	 */
	ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_ds_read",
		&(fd->hints->ds_read), myname, error_code);
	ADIOI_Info_check_and_install_enabled(fd, users_info, "romio_ds_write",
		&(fd->hints->ds_write), myname, error_code);

	ADIOI_Info_check_and_install_int(fd, users_info, "ind_wr_buffer_size",
		&(fd->hints->ind_wr_buffer_size), myname, error_code);
	ADIOI_Info_check_and_install_int(fd, users_info, "ind_rd_buffer_size",
		&(fd->hints->ind_rd_buffer_size), myname, error_code);

	memset( value, 0, MPI_MAX_INFO_VAL+1 );
	ADIOI_Info_get(users_info, "romio_min_fdomain_size", MPI_MAX_INFO_VAL,
			value, &flag);
	if ( flag && ((intval = atoi(value)) > 0) ) {
		ADIOI_Info_set(info, "romio_min_fdomain_size", value);
		fd->hints->min_fdomain_size = intval;
	}
  /* Now we use striping unit in common code so we should
     process hints for it. */
	ADIOI_Info_check_and_install_int(fd, users_info, "striping_unit",
		&(fd->hints->striping_unit), myname, error_code);

#ifdef BGQPLATFORM
	memset( value, 0, MPI_MAX_INFO_VAL+1 );
        ADIOI_Info_get(users_info, ADIOI_BG_NAGG_IN_PSET_HINT_NAME, MPI_MAX_INFO_VAL,
		     value, &flag);
	if (flag && ((intval = atoi(value)) > 0)) {

	    did_anything = 1;
	    ADIOI_Info_set(info, ADIOI_BG_NAGG_IN_PSET_HINT_NAME, value);
	    fd->hints->cb_nodes = intval;
	}
#endif
    }

    /* special CB aggregator assignment */
    if (did_anything) {
#ifdef BGQPLATFORM
	ADIOI_BG_gen_agg_ranklist(fd, fd->hints->cb_nodes);
#elif PEPLATFORM
	ADIOI_PE_gen_agg_ranklist(fd);
#endif
    }

    /* deferred_open won't be set by callers, but if the user doesn't
     * explicitly disable collecitve buffering (two-phase) and does hint that
     * io w/o independent io is going on, we'll set this internal hint as a
     * convenience */
    if ( ( (fd->hints->cb_read != ADIOI_HINT_DISABLE) \
			    && (fd->hints->cb_write != ADIOI_HINT_DISABLE)\
			    && fd->hints->no_indep_rw ) ) {
	    fd->hints->deferred_open = 1;
    } else {
	    /* setting romio_no_indep_rw enable and romio_cb_{read,write}
	     * disable at the same time doesn't make sense. honor
	     * romio_cb_{read,write} and force the no_indep_rw hint to
	     * 'disable' */
	    ADIOI_Info_set(info, "romio_no_indep_rw", "false");
	    fd->hints->no_indep_rw = 0;
	    fd->hints->deferred_open = 0;
    }

    /* BobC commented this out, but since hint processing runs on both bg and
     * bglockless, we need to keep DS writes enabled on gpfs and disabled on
     * PVFS */
    if (ADIO_Feature(fd, ADIO_DATA_SIEVING_WRITES) == 0) {
    /* disable data sieving for fs that do not
       support file locking */
       	ADIOI_Info_get(info, "ind_wr_buffer_size", MPI_MAX_INFO_VAL,
		     value, &flag);
	if (flag) {
	    /* get rid of this value if it is set */
	    ADIOI_Info_delete(info, "ind_wr_buffer_size");
	}
	/* note: leave ind_wr_buffer_size alone; used for other cases
	 * as well. -- Rob Ross, 04/22/2003
	 */
	ADIOI_Info_set(info, "romio_ds_write", "disable");
	fd->hints->ds_write = ADIOI_HINT_DISABLE;
    }

    ADIOI_Free(value);

    *error_code = MPI_SUCCESS;
}
