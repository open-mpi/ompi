/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef AD_IME_COMMON_H_INCLUDED
#define AD_IME_COMMON_H_INCLUDED
#include "ad_ime.h"

struct ADIOI_IME_fs_s {
    char *ime_filename;
};

typedef struct ADIOI_IME_fs_s ADIOI_IME_fs;

void ADIOI_IME_Init(int rank, int *error_code);
void ADIOI_IME_End(int *error_code);
int ADIOI_IME_End_call(MPI_Comm comm, int keyval, void *attribute_val, void *extra_state);

char *ADIOI_IME_Add_prefix(const char *filename);
#endif /* AD_IME_COMMON_H_INCLUDED */
