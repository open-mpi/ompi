/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *
 *  (C) 2013 UChicago/Argonne LLC
 *      See COPYRIGHT in top-level directory.
 */

#ifndef _HINT_FNS_H
#define _HINT_FNS_H

int ADIOI_Info_check_and_install_int(ADIO_File fd, MPI_Info info, const char *key,
	        int *local_cache, char *funcname, int *error_code);

int ADIOI_Info_check_and_install_enabled(ADIO_File fd, MPI_Info info, const char *key,
	        int *local_cache, char *funcname, int *error_code);

int ADIOI_Info_check_and_install_true(ADIO_File fd, MPI_Info info, const char *key,
	        int *local_cache, char *funcname, int *error_code);

int ADIOI_Info_check_and_install_str(ADIO_File fd, MPI_Info info, const char *key,
	        char **local_cache, char *funcname, int *error_code);

#endif
