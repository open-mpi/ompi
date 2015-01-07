/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*  
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* Change for BG/L made by Hao Yu, yuh@us.ibm.com
 */

#include "mpi.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void handle_error(int errcode, const char *str)
{
	char msg[MPI_MAX_ERROR_STRING];
	int resultlen;
	MPI_Error_string(errcode, msg, &resultlen);
	fprintf(stderr, "%s: %s\n", str, msg);
	MPI_Abort(MPI_COMM_WORLD, 1);
}
/* this test wants to compare the hints it gets from a file with a set of
 * default hints.  These hints are specific to the MPI-IO implementation, so if
 * you want to test something besides the default you'll have to use a command
 * line argument */

typedef struct hint_defaults {
    int cb_buffer_size;
    int ind_rd_buffer_size;
    int ind_wr_buffer_size;
    const char *romio_cb_read;
    const char *romio_cb_write;
    const char *cb_config_list;
} hint_defaults;

hint_defaults UFS_DEFAULTS = {
    .cb_buffer_size = 16777216,
    .ind_rd_buffer_size = 4194304,
    .ind_wr_buffer_size = 524288,
    .romio_cb_read = "automatic",
    .romio_cb_write = "automatic",
    .cb_config_list = "*:1"
};

hint_defaults BLUEGENE_DEFAULTS = {
    .cb_buffer_size = 16777216, 
    .ind_rd_buffer_size = 4194304, 
    .ind_wr_buffer_size = 4194304, 
    .romio_cb_read = "enable", 
    .romio_cb_write = "enable",
    .cb_config_list = NULL};

/* #undef INFO_DEBUG */

/* Test will print out information about unexpected hint keys or values that
 * differ from the default.  Since this is often interesting but rarely an
 * error, default will be to increment errror cound for true error conditions
 * but not print out these "interesting" non-error cases. */

static int verbose = 0;
static int test_ufs = 0;
static int test_bluegene = 0;

int main(int argc, char **argv)
{
    int i, len, nkeys, flag, mynod, default_striping_factor=0, nprocs, errs = 0;
    MPI_File fh;
    MPI_Info info, info_used;
    char *filename, key[MPI_MAX_INFO_KEY], value[MPI_MAX_INFO_VAL];
    hint_defaults *defaults;
    int ret;

    MPI_Init(&argc,&argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &mynod);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

/* process 0 takes the file name as a command-line argument and 
   broadcasts it to other processes */
    if (!mynod) {
	i = 1;
	while ((i < argc) && strcmp("-fname", *argv)) {
	    if (!strcmp("-v", *argv)) verbose = 1;
	    else if (!strcmp("-u", *argv)) test_ufs = 1;
	    else if (!strcmp("-b", *argv)) test_bluegene = 1;
	    i++;
	    argv++;
	}
	if (i >= argc) {
	    fprintf(stderr, "\n*#  Usage: file_info [-v] -fname filename\n\n");
	    MPI_Abort(MPI_COMM_WORLD, 1);
	}
	argv++;
	len = strlen(*argv);
	filename = (char *) malloc(len+1);
	strcpy(filename, *argv);
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(filename, len+1, MPI_CHAR, 0, MPI_COMM_WORLD);
	MPI_Bcast(&verbose, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(&test_ufs, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(&test_bluegene, 1, MPI_INT, 0, MPI_COMM_WORLD);
    }
    else {
	MPI_Bcast(&len, 1, MPI_INT, 0, MPI_COMM_WORLD);
	filename = (char *) malloc(len+1);
	MPI_Bcast(filename, len+1, MPI_CHAR, 0, MPI_COMM_WORLD);
	MPI_Bcast(&verbose, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(&test_ufs, 1, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Bcast(&test_bluegene, 1, MPI_INT, 0, MPI_COMM_WORLD);
    }
    if (test_ufs) {
	defaults = &UFS_DEFAULTS;
    } else if (test_bluegene) {
	defaults = &BLUEGENE_DEFAULTS;
    } else {
	defaults = NULL;
    }


/* open the file with MPI_INFO_NULL */
    ret = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR, 
                  MPI_INFO_NULL, &fh);
    if (ret != MPI_SUCCESS) handle_error(ret, "MPI_File_open");

/* check the default values set by ROMIO */
    MPI_File_get_info(fh, &info_used);
    MPI_Info_get_nkeys(info_used, &nkeys);

    if (defaults != NULL) {
	for (i=0; i<nkeys; i++) {
	    MPI_Info_get_nthkey(info_used, i, key);
	    MPI_Info_get(info_used, key, MPI_MAX_INFO_VAL-1, value, &flag);
#ifdef INFO_DEBUG
	    if (!mynod) 
		fprintf(stderr, "Process %d, Default:  key = %s, value = %s\n", mynod, 
			key, value);
#endif
	    if (!strcmp("striping_factor", key)) {
		default_striping_factor = atoi(value);
		/* no check */
	    }
	    else if (!strcmp("cb_buffer_size", key)) {
		if (atoi(value) != defaults->cb_buffer_size) {
		    errs++;
		    if (verbose) fprintf(stderr, "cb_buffer_size is %d; should be %d\n",
			    atoi(value), defaults->cb_buffer_size);
		}
	    }
	    else if (!strcmp("romio_cb_read", key)) {
		if (strcmp(defaults->romio_cb_read, value)) {
		    errs++;
		    if (verbose) fprintf(stderr, "romio_cb_read is set to %s; should be %s\n",
			    value, defaults->romio_cb_read);
		}
	    }
	    else if (!strcmp("romio_cb_write", key)) {
		if (strcmp(defaults->romio_cb_write, value)) {
		    errs++;
		    if (verbose) fprintf(stderr, "romio_cb_write is set to %s; should be %s\n",
			    value, defaults->romio_cb_write);
		}
	    }
	    else if (!strcmp("cb_nodes", key)) {
		/* unreliable test -- just ignore value */
	    }
	    else if (!strcmp("romio_no_indep_rw", key)) {
		if (strcmp("false", value)) {
		    errs++;
		    if (verbose) fprintf(stderr, "romio_no_indep_rw is set to %s; should be %s\n",
			    value, "false");
		}
	    }
	    else if (!strcmp("ind_rd_buffer_size", key)) {
		if (atoi(value) != defaults->ind_rd_buffer_size) {
		    errs++;
		    if (verbose) fprintf(stderr, "ind_rd_buffer_size is %d; should be %d\n",
			    atoi(value), defaults->ind_rd_buffer_size);
		}
	    }
	    else if (!strcmp("ind_wr_buffer_size", key)) {
		if (atoi(value) != defaults->ind_wr_buffer_size) {
		    errs++;
		    if (verbose) fprintf(stderr, "ind_wr_buffer_size is %d; should be %d\n",
			    atoi(value), defaults->ind_wr_buffer_size);
		}
	    }
	    else if (!strcmp("romio_ds_read", key)) {
		if (strcmp("automatic", value)) {
		    errs++;
		    if (verbose) fprintf(stderr, "romio_ds_read is set to %s; should be %s\n",
			    value, "automatic");
		}
	    }
	    else if (!strcmp("romio_ds_write", key)) {
		/* Unreliable test -- value is file system dependent.  Ignore. */
	    }
	    else if (!strcmp("cb_config_list", key)) {
#ifndef SKIP_CB_CONFIG_LIST_TEST
		if (strcmp(defaults->cb_config_list, value)) {
		    errs++;
		    if (verbose) fprintf(stderr, "cb_config_list is set to %s; should be %s\n",
			    value, defaults->cb_config_list);
		}
#endif
	    }
	    /* don't care about the defaults for these keys */
	    else if (!strcmp("romio_cb_pfr", key)) {
	    }
	    else if (!strcmp("romio_cb_fr_types", key)) {
	    }
	    else if (!strcmp("romio_cb_fr_alignment", key)) {
	    }
	    else if (!strcmp("romio_cb_ds_threshold", key)) {
	    }
	    else if (!strcmp("romio_cb_alltoall", key)) {
	    }
	    else {
		if (verbose) fprintf(stderr, "unexpected key %s (not counted as an error)\n", key);
	    }
	}
    }
    MPI_Info_free(&info_used);

    MPI_File_close(&fh);

    /* delete the file */
    if (!mynod) MPI_File_delete(filename, MPI_INFO_NULL);
    MPI_Barrier(MPI_COMM_WORLD);

/* set new info values. */

    MPI_Info_create(&info);

/* The following four hints are accepted on all machines. They can
   be specified at file-open time or later (any number of times). */

    /* buffer size for collective I/O */
    MPI_Info_set(info, "cb_buffer_size", "8388608");

    /* number of processes that actually perform I/O in collective I/O */
    sprintf(value, "%d", nprocs/2);
    MPI_Info_set(info, "cb_nodes", value);

    /* buffer size for data sieving in independent reads */
    MPI_Info_set(info, "ind_rd_buffer_size", "2097152");

    /* buffer size for data sieving in independent writes */
    MPI_Info_set(info, "ind_wr_buffer_size", "1048576");


/* The following three hints related to file striping are accepted only 
   on Intel PFS and IBM PIOFS file systems and are ignored elsewhere. 
   They can be specified only at file-creation time; if specified later 
   they will be ignored. */

    /* number of I/O devices across which the file will be striped.
       accepted only if 0 < value < default_striping_factor; 
       ignored otherwise */
    if (default_striping_factor - 1 > 0) {
        sprintf(value, "%d", default_striping_factor-1);
        MPI_Info_set(info, "striping_factor", value);
    }
    else {
        sprintf(value, "%d", default_striping_factor);
        MPI_Info_set(info, "striping_factor", value);
    }

    /* the striping unit in bytes */
    MPI_Info_set(info, "striping_unit", "131072");

#ifndef SKIP_CB_CONFIG_LIST_TEST
    /* set the cb_config_list so we'll get deterministic cb_nodes output */
    MPI_Info_set(info, "cb_config_list", "*:*");
#endif

    /* the I/O device number from which to start striping the file.
       accepted only if 0 <= value < default_striping_factor; 
       ignored otherwise */
    sprintf(value, "%d", default_striping_factor-2);
    MPI_Info_set(info, "start_iodevice", value);


/* The following hint about PFS server buffering is accepted only on 
   Intel PFS. It can be specified anytime. */ 
    MPI_Info_set(info, "pfs_svr_buf", "true");

/* open the file and set new info */
    ret = MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_CREATE | MPI_MODE_RDWR, 
                  info, &fh);
    if (ret != MPI_SUCCESS) handle_error(ret, "MPI_File_open");

/* check the values set */
    ret = MPI_File_get_info(fh, &info_used);
    if (ret != MPI_SUCCESS) handle_error(ret, "MPI_File_get_info");
    MPI_Info_get_nkeys(info_used, &nkeys);

    for (i=0; i<nkeys; i++) {
	MPI_Info_get_nthkey(info_used, i, key);
	MPI_Info_get(info_used, key, MPI_MAX_INFO_VAL-1, value, &flag);
#ifdef INFO_DEBUG	
	if (!mynod) fprintf(stderr, "Process %d, key = %s, value = %s\n", mynod, 
                key, value);
#endif
	if (!strcmp("striping_factor", key)) {
	    if ((default_striping_factor - 1 > 0) && (atoi(value) != default_striping_factor-1)) {
		errs++;
		if (verbose) fprintf(stderr, "striping_factor is %d; should be %d\n",
				     atoi(value), default_striping_factor-1);
	    }
	    else if (atoi(value) != default_striping_factor) {
		errs++;
		if (verbose) fprintf(stderr, "striping_factor is %d; should be %d\n",
				     atoi(value), default_striping_factor);
	    }
	}
	else if (!strcmp("cb_buffer_size", key)) {
	    if (atoi(value) != 8388608) {
		errs++;
		if (verbose) fprintf(stderr, "cb_buffer_size is %d; should be %d\n",
				     atoi(value), 8388608);
	    }
	}
	/* only check the hints we set */
	else if (!strcmp("cb_nodes", key)) {
	    /* unreliable test: just skip */
	}
	else if (!strcmp("romio_no_indep_rw", key)) {
	    if (strcmp("false", value)) {
		errs++;
		if (verbose) fprintf(stderr, "romio_no_indep_rw is set to %s; should be %s\n",
				     value, "false");
	    }
	}
	else if (!strcmp("ind_rd_buffer_size", key)) {
	    if (atoi(value) != 2097152) {
		errs++;
		if (verbose) fprintf(stderr, "ind_rd_buffer_size is %d; should be %d\n",
				     atoi(value), 2097152);
	    }
	}
	else if (!strcmp("ind_wr_buffer_size", key)) {
	    if (atoi(value) != 1048576) {
		errs++;
		if (verbose) fprintf(stderr, "ind_wr_buffer_size is %d; should be %d\n",
				     atoi(value), 1048576);
	    }
	}
	else if (!strcmp("romio_ds_read", key)) {
	    if (strcmp("automatic", value)) {
		errs++;
		if (verbose) fprintf(stderr, "romio_ds_read is set to %s; should be %s\n",
				     value, "automatic");
	    }
	}
	else if (!strcmp("romio_ds_write", key)) {
	    /* Unreliable test -- value is file system dependent.  Ignore. */
	}
	else if (!strcmp("cb_config_list", key)) {
#ifndef SKIP_CB_CONFIG_LIST_TEST
	    if (strcmp("*:*", value)) {
		errs++;
		if (verbose) fprintf(stderr, "cb_config_list is set to %s; should be %s\n",
				     value, "*:*");
	    }
#endif
	}
	else if (!strcmp("romio_cb_pfr", key)) {
   	    if(strcmp("disable", value)) {
		errs++;
		if (verbose) fprintf(stderr, "romio_cb_pfr is set to %s; should be %s\n",
				     value, "automatic");
	    }
	}
	else if (!strcmp("romio_cb_fr_types", key)) {
   	    if(strcmp("aar", value)) {
		errs++;
		if (verbose) fprintf(stderr, "romio_cb_fr_types is set to %s; should be %s\n",
				     value, "aar");
	    }
	}
	else if (!strcmp("romio_cb_fr_alignment", key)) {
   	    if(strcmp("1", value)) {
		errs++;
		if (verbose) fprintf(stderr, "romio_cb_fr_alignment is set to %s; should be %s\n",
				     value, "1");
	    }
	}
	else if (!strcmp("romio_cb_ds_threshold", key)) {
   	    if(strcmp("0", value)) {
		errs++;
		if (verbose) fprintf(stderr, "romio_cb_ds_threshold is set to %s; should be %s\n",
				     value, "0");
	    }
	}
	else if (!strcmp("romio_cb_alltoall", key)) {
   	    if(strcmp("automatic", value)) {
		errs++;
		if (verbose) fprintf(stderr, "romio_cb_alltoall is set to %s; should be %s\n",
				     value, "automatic");
	    }
	}

	else {
	    if (verbose) fprintf(stderr, "unexpected key %s (not counted as an error)\n", key);
	}
    }
	    
    /* Q: SHOULD WE BOTHER LOOKING AT THE OTHER PROCESSES? */
    if (!mynod) {
	if (errs) fprintf(stderr, "Found %d errors.\n", errs);
	else printf(" No Errors\n");
    }
    
    MPI_File_close(&fh);
    free(filename);
    MPI_Info_free(&info_used);
    MPI_Info_free(&info);
    MPI_Finalize();
    return 0;
}
