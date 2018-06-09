/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMI_INTERFACE_H
#define PMI_INTERFACE_H

#include <stdio.h>

void pmi_init(int *rank, int *size);
void pmi_get_local_ranks(int **local_ranks, int *local_cnt);
void pmi_get_shmem_size(char *is_avail, size_t *cum_size);
void pmi_put_key_loc(char *key, int *key_val, int key_size);
void pmi_put_key_rem(char *key, int *key_val, int key_size);
void pmi_put_double(char *key, double val);
void pmi_commit();
void pmi_fence(int collect);
void pmi_fini();
void pmi_get_key_loc(int rank, char *key_name, int **key_val, int *key_size);
void pmi_get_key_rem(int rank, char *key_name, int **key_val, int *key_size);
double pmi_get_double(int rank, char *key);

#endif
