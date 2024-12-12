/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef ADIO_CB_CONFIG_LIST_H_INCLUDED
#define ADIO_CB_CONFIG_LIST_H_INCLUDED

/* I have no idea what the "D" stands for; it's how things are done in adio.h
 */
struct ADIO_cb_name_arrayD {
    int refct;
    int namect;
    char **names;
};

typedef struct ADIO_cb_name_arrayD *ADIO_cb_name_array;

int ADIOI_cb_gather_name_array(MPI_Comm comm, MPI_Comm dupcomm, ADIO_cb_name_array * arrayp);
int ADIOI_cb_copy_name_array(MPI_Comm comm, int keyval, void *extra,
                             void *attr_in, void **attr_out, int *flag);
int ADIOI_cb_delete_name_array(MPI_Comm comm, int keyval, void *attr_val, void *extra);
int ADIOI_cb_config_list_parse(char *config_list, ADIO_cb_name_array array,
                               int ranklist[], int cb_nodes);
int ADIOI_cb_bcast_rank_map(ADIO_File fd);

#endif /* ADIO_CB_CONFIG_LIST_H_INCLUDED */
