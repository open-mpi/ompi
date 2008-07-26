/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_bgl_hints.c
 * \brief ???
 */

/* -*- Mode: C; c-basic-offset:4 ; -*- */
/* 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"

#include "ad_bgl.h"
#include "ad_bgl_pset.h"
#include "ad_bgl_aggrs.h"

#define   ADIOI_BGL_CB_BUFFER_SIZE_DFLT      	"16777216"
#define	  ADIOI_BGL_IND_RD_BUFFER_SIZE_DFLT	"4194304"
#define   ADIOI_BGL_IND_WR_BUFFER_SIZE_DFLT	"4194304"
#define   ADIOI_BGL_NAGG_IN_PSET_HINT_NAME	"bgl_nodes_pset"

/* Compute the aggregator-related parameters that are required in 2-phase collective IO of ADIO. */
extern int 
ADIOI_BGL_gen_agg_ranklist(ADIO_File fd, int n_proxy_per_pset);

void ADIOI_BGL_SetInfo(ADIO_File fd, MPI_Info users_info, int *error_code)
{
/* if fd->info is null, create a new info object. 
   Initialize fd->info to default values.
   Initialize fd->hints to default values.
   Examine the info object passed by the user. If it contains values that
   ROMIO understands, override the default. */

    MPI_Info info;
    char *value;
    int flag, intval, tmp_val, nprocs, nprocs_is_valid = 0;
    static char myname[] = "ADIOI_GEN_SETINFO";

    int did_anything = 0;

    if (fd->info == MPI_INFO_NULL) MPI_Info_create(&(fd->info));
    info = fd->info;

    /* Note that fd->hints is allocated at file open time; thus it is
     * not necessary to allocate it, or check for allocation, here.
     */

    value = (char *) ADIOI_Malloc((MPI_MAX_INFO_VAL+1)*sizeof(char));
    AD_BGL_assert ((value != NULL));

    /* initialize info and hints to default values if they haven't been
     * previously initialized
     */
    if (!fd->hints->initialized) {

	did_anything = 1;

	/* buffer size for collective I/O */
	MPI_Info_set(info, "cb_buffer_size", ADIOI_BGL_CB_BUFFER_SIZE_DFLT); 
	fd->hints->cb_buffer_size = atoi(ADIOI_BGL_CB_BUFFER_SIZE_DFLT);

	/* default is to let romio automatically decide when to use
	 * collective buffering
	 */
	MPI_Info_set(info, "romio_cb_read", "enable"); 
	fd->hints->cb_read = ADIOI_HINT_ENABLE;
	MPI_Info_set(info, "romio_cb_write", "enable"); 
	fd->hints->cb_write = ADIOI_HINT_ENABLE;

   	if ( fd->hints->cb_config_list != NULL ) ADIOI_Free (fd->hints->cb_config_list);
	fd->hints->cb_config_list = NULL;

	/* number of processes that perform I/O in collective I/O */
	MPI_Comm_size(fd->comm, &nprocs);
	nprocs_is_valid = 1;
	sprintf(value, "%d", nprocs);
	MPI_Info_set(info, "cb_nodes", value);
	fd->hints->cb_nodes = -1;

	/* hint indicating that no indep. I/O will be performed on this file */
	MPI_Info_set(info, "romio_no_indep_rw", "false");
	fd->hints->no_indep_rw = 0;
	 /* deferred_open derrived from no_indep_rw and cb_{read,write} */
	fd->hints->deferred_open = 0;

	/* buffer size for data sieving in independent reads */
	MPI_Info_set(info, "ind_rd_buffer_size", ADIOI_BGL_IND_RD_BUFFER_SIZE_DFLT);
	fd->hints->ind_rd_buffer_size = atoi(ADIOI_BGL_IND_RD_BUFFER_SIZE_DFLT);

	/* buffer size for data sieving in independent writes */
	MPI_Info_set(info, "ind_wr_buffer_size", ADIOI_BGL_IND_WR_BUFFER_SIZE_DFLT);
	fd->hints->ind_wr_buffer_size = atoi(ADIOI_BGL_IND_WR_BUFFER_SIZE_DFLT);

  if(fd->file_system == ADIO_UFS)
  {
    /* default for ufs/pvfs is to disable data sieving  */
    MPI_Info_set(info, "romio_ds_read", "disable"); 
    fd->hints->ds_read = ADIOI_HINT_DISABLE;
    MPI_Info_set(info, "romio_ds_write", "disable"); 
    fd->hints->ds_write = ADIOI_HINT_DISABLE;
  }
  else
  {
    /* default is to let romio automatically decide when to use data
     * sieving
     */
    MPI_Info_set(info, "romio_ds_read", "automatic"); 
    fd->hints->ds_read = ADIOI_HINT_AUTO;
    MPI_Info_set(info, "romio_ds_write", "automatic"); 
    fd->hints->ds_write = ADIOI_HINT_AUTO;
  }

	fd->hints->initialized = 1;
    }

    /* add in user's info if supplied */
    if (users_info != MPI_INFO_NULL) {
	MPI_Info_get(users_info, "cb_buffer_size", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval=atoi(value)) > 0)) {
	    tmp_val = intval;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != intval) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "cb_buffer_size",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */

	    MPI_Info_set(info, "cb_buffer_size", value);
	    fd->hints->cb_buffer_size = intval;

	}

	/* new hints for enabling/disabling coll. buffering on
	 * reads/writes
	 */
	MPI_Info_get(users_info, "romio_cb_read", MPI_MAX_INFO_VAL, value, &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		MPI_Info_set(info, "romio_cb_read", value);
		fd->hints->cb_read = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		    /* romio_cb_read overrides no_indep_rw */
		MPI_Info_set(info, "romio_cb_read", value);
		MPI_Info_set(info, "romio_no_indep_rw", "false");
		fd->hints->cb_read = ADIOI_HINT_DISABLE;
		fd->hints->no_indep_rw = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		MPI_Info_set(info, "romio_cb_read", value);
		fd->hints->cb_read = ADIOI_HINT_AUTO;
	    }

	    tmp_val = fd->hints->cb_read;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->cb_read) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_read",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}
	MPI_Info_get(users_info, "romio_cb_write", MPI_MAX_INFO_VAL, value, &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		MPI_Info_set(info, "romio_cb_write", value);
		fd->hints->cb_write = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE"))
	    {
		/* romio_cb_write overrides no_indep_rw, too */
		MPI_Info_set(info, "romio_cb_write", value);
		MPI_Info_set(info, "romio_no_indep_rw", "false");
		fd->hints->cb_write = ADIOI_HINT_DISABLE;
		fd->hints->no_indep_rw = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") ||
		     !strcmp(value, "AUTOMATIC"))
	    {
		MPI_Info_set(info, "romio_cb_write", value);
		fd->hints->cb_write = ADIOI_HINT_AUTO;
	    }
	
	    tmp_val = fd->hints->cb_write;

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->cb_write) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_cb_write",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}

	/* new hint for specifying no indep. read/write will be performed */
	MPI_Info_get(users_info, "romio_no_indep_rw", MPI_MAX_INFO_VAL, value, &flag);
	if (flag) {
	    if (!strcmp(value, "true") || !strcmp(value, "TRUE")) {
		    /* if 'no_indep_rw' set, also hint that we will do
		     * collective buffering: if we aren't doing independent io,
		     * then we have to do collective  */
		MPI_Info_set(info, "romio_no_indep_rw", value);
		MPI_Info_set(info, "romio_cb_write", "enable");
		MPI_Info_set(info, "romio_cb_read", "enable");
		fd->hints->no_indep_rw = 1;
		fd->hints->cb_read = 1;
		fd->hints->cb_write = 1;
		tmp_val = 1;
	    }
	    else if (!strcmp(value, "false") || !strcmp(value, "FALSE")) {
		MPI_Info_set(info, "romio_no_indep_rw", value);
		fd->hints->no_indep_rw = 0;
		tmp_val = 0;
	    }
	    else {
		/* default is above */
		tmp_val = 0;
	    }

	    MPI_Bcast(&tmp_val, 1, MPI_INT, 0, fd->comm);
	    /* --BEGIN ERROR HANDLING-- */
	    if (tmp_val != fd->hints->no_indep_rw) {
		MPIO_ERR_CREATE_CODE_INFO_NOT_SAME(myname,
						   "romio_no_indep_rw",
						   error_code);
		return;
	    }
	    /* --END ERROR HANDLING-- */
	}
	/* new hints for enabling/disabling data sieving on
	 * reads/writes
	 */
	MPI_Info_get(users_info, "romio_ds_read", MPI_MAX_INFO_VAL, value, 
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		MPI_Info_set(info, "romio_ds_read", value);
		fd->hints->ds_read = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		MPI_Info_set(info, "romio_ds_read", value);
		fd->hints->ds_read = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		MPI_Info_set(info, "romio_ds_read", value);
		fd->hints->ds_read = ADIOI_HINT_AUTO;
	    }
	    /* otherwise ignore */
	}
	MPI_Info_get(users_info, "romio_ds_write", MPI_MAX_INFO_VAL, value, 
		     &flag);
	if (flag) {
	    if (!strcmp(value, "enable") || !strcmp(value, "ENABLE")) {
		MPI_Info_set(info, "romio_ds_write", value);
		fd->hints->ds_write = ADIOI_HINT_ENABLE;
	    }
	    else if (!strcmp(value, "disable") || !strcmp(value, "DISABLE")) {
		MPI_Info_set(info, "romio_ds_write", value);
		fd->hints->ds_write = ADIOI_HINT_DISABLE;
	    }
	    else if (!strcmp(value, "automatic") || !strcmp(value, "AUTOMATIC"))
	    {
		MPI_Info_set(info, "romio_ds_write", value);
		fd->hints->ds_write = ADIOI_HINT_AUTO;
	    }
	    /* otherwise ignore */
	}

	MPI_Info_get(users_info, "ind_wr_buffer_size", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval = atoi(value)) > 0)) {
	    MPI_Info_set(info, "ind_wr_buffer_size", value);
	    fd->hints->ind_wr_buffer_size = intval;
	}

	MPI_Info_get(users_info, "ind_rd_buffer_size", MPI_MAX_INFO_VAL, 
		     value, &flag);
	if (flag && ((intval = atoi(value)) > 0)) {
	    MPI_Info_set(info, "ind_rd_buffer_size", value);
	    fd->hints->ind_rd_buffer_size = intval;
	}

	memset( value, 0, MPI_MAX_INFO_VAL+1 );
	MPI_Info_get(users_info, ADIOI_BGL_NAGG_IN_PSET_HINT_NAME, MPI_MAX_INFO_VAL,
		     value, &flag);
	if (flag && ((intval = atoi(value)) > 0)) {

	    did_anything = 1;
	    MPI_Info_set(info, ADIOI_BGL_NAGG_IN_PSET_HINT_NAME, value);
	    fd->hints->cb_nodes = intval;
	}
    }

    /* associate CB aggregators to certain CNs in every involved PSET */
    if (did_anything) {
	ADIOI_BGL_gen_agg_ranklist(fd, fd->hints->cb_nodes);
    }

    /* deferred_open won't be set by callers, but if the user doesn't
     * explicitly disable collecitve buffering (two-phase) and does hint that
     * io w/o independent io is going on, we'll set this internal hint as a
     * convenience */
    if ( ( (fd->hints->cb_read != ADIOI_HINT_DISABLE) 
	    && (fd->hints->cb_write != ADIOI_HINT_DISABLE)
	    && fd->hints->no_indep_rw ) ) 
    {
	    fd->hints->deferred_open = 1;
    } else {
	    /* setting romio_no_indep_rw enable and romio_cb_{read,write}
	     * disable at the same time doesn't make sense. honor
	     * romio_cb_{read,write} and force the no_indep_rw hint to
	     * 'disable' */
	    MPI_Info_set(info, "romio_no_indep_rw", "false");
	    fd->hints->no_indep_rw = 0;
	    fd->hints->deferred_open = 0;
    }

    ADIOI_Free(value);

    *error_code = MPI_SUCCESS;
}
