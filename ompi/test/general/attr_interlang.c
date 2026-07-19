/*
 * Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * C back-end for the C<->Fortran attribute interlanguage test.  Adapted
 * from the open-mpi/ompi-tests simple/attr test (attr_c.c) for the
 * in-tree ompi/test suite.  Provides keyval-create / write / read / check
 * helpers that the Fortran driver calls, so that attributes set in one
 * language can be read (and value-translated) in the other.
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* This test intentionally exercises the deprecated MPI-1 attribute
 * routines (MPI_Keyval_create / MPI_Attr_put / MPI_Attr_get, ...).
 * ompi_config.h enables the MPI interface-deprecation warnings; turn them
 * off here (before including mpi.h) so the build stays clean. */
#undef OMPI_WANT_MPI_INTERFACE_WARNING
#define OMPI_WANT_MPI_INTERFACE_WARNING 0

#include "mpi.h"

/*
 * Do not change these values without also changing their
 * corresponding values in values-f.h!
 */
enum { FORTRAN_MPI1_VALUE1 = 771 };
enum { FORTRAN_MPI1_VALUE2 = -771 };
enum { FORTRAN_MPI1_VALUE3 = -123 };
enum { FORTRAN_MPI2_VALUE1 = 772 };
enum { FORTRAN_MPI2_VALUE2 = -772 };
enum { FORTRAN_MPI2_VALUE3 = -123 };
enum { C_VALUE1 = 99 };
enum { C_VALUE2 = -99 };
enum { C_VALUE3 = 481 };


typedef enum attr_type {
    INTEGER,
    AINT
} attr_type_t;


/* Back-end functions */
static int comm_delete_fn(MPI_Comm comm, int keyval,
                          void* attribute_val_out,
                          void* extra_state);
static int comm_copy_fn(MPI_Comm comm, int keyval,
                        void* extra_state,
                        void* attribute_val_in,
                        void* attribute_val_out,
                        int* flag);
static int type_delete_fn(MPI_Datatype type, int keyval,
                          void* attribute_val_out,
                          void* extra_state);
static int type_copy_fn(MPI_Datatype type, int keyval,
                        void* extra_state,
                        void* attribute_val_in,
                        void* attribute_val_out,
                        int* flag);
static int win_delete_fn(MPI_Win win, int keyval,
                         void* attribute_val_out,
                         void* extra_state);
static int win_copy_fn(MPI_Win win, int keyval,
                       void* extra_state,
                       void* attribute_val_in,
                       void* attribute_val_out,
                       int* flag);

static void c_create_keyval_backend(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                                    MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                                    MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                                    MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3);
static void c_write_backend(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                            MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                            MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                            MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3,
                            MPI_Fint *win);
static void c_read_backend(MPI_Fint *k1, MPI_Fint *k2,
                           MPI_Fint *kc1, MPI_Fint *kc2,
                           MPI_Fint *kt1, MPI_Fint *kt2,
                           MPI_Fint *kw1, MPI_Fint *kw2,
                           MPI_Fint *f11, MPI_Fint *f12, MPI_Fint *f13,
                           MPI_Fint *fc21, MPI_Fint *fc22, MPI_Fint *fc23,
                           MPI_Fint *ft21, MPI_Fint *ft22, MPI_Fint *ft23,
                           MPI_Fint *fw21, MPI_Fint *fw22, MPI_Fint *fw23,
                           MPI_Fint *win);
static void c_check_mpi1_backend(MPI_Fint *key, MPI_Fint *value);
static void c_check_mpi2_backend(MPI_Fint *key, MPI_Aint *value);

static void check(int key, int expected_value, attr_type_t type);
static void check_comm(int key, int expected_value, attr_type_t type);
static void check_type(int key, int expected_value, attr_type_t type);
static void check_win(MPI_Win win, int key, int expected_value, attr_type_t type);

/* Fortran wrappers */
void c_create_keyval(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                     MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                     MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                     MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3)
{ c_create_keyval_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3); }
void c_create_keyval_(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                      MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                      MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                      MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3)
{ c_create_keyval_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3); }
void c_create_keyval__(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                       MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                       MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                       MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3)
{ c_create_keyval_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3); }
void C_CREATE_KEYVAL(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                     MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                     MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                     MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3)
{ c_create_keyval_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3); }


void c_write(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
             MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
             MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
             MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3,
             MPI_Fint *win)
{ c_write_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3, win); }
void c_write_(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
              MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
              MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
              MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3,
              MPI_Fint *win)
{ c_write_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3, win); }
void c_write__(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
               MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
               MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
               MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3,
               MPI_Fint *win)
{ c_write_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3, win); }
void C_WRITE(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
             MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
             MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
             MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3,
             MPI_Fint *win)
{ c_write_backend(k1, k2, k3, kc1, kc2, kc3, kt1, kt2, kt3, kw1, kw2, kw3, win); }

void c_read(MPI_Fint *k1, MPI_Fint *k2,
            MPI_Fint *kc1, MPI_Fint *kc2,
            MPI_Fint *kt1, MPI_Fint *kt2,
            MPI_Fint *kw1, MPI_Fint *kw2,
            MPI_Fint *f11, MPI_Fint *f12, MPI_Fint *f13,
            MPI_Fint *fc21, MPI_Fint *fc22, MPI_Fint *fc23,
            MPI_Fint *ft21, MPI_Fint *ft22, MPI_Fint *ft23,
            MPI_Fint *fw21, MPI_Fint *fw22, MPI_Fint *fw23,
            MPI_Fint *win)
{ c_read_backend(k1, k2, kc1, kc2, kt1, kt2, kw1, kw2,
                 f11, f12, f13,
                 fc21, fc22, fc23, ft21, ft22, ft23, fw21, fw22, fw23, win); }
void c_read_(MPI_Fint *k1, MPI_Fint *k2,
             MPI_Fint *kc1, MPI_Fint *kc2,
             MPI_Fint *kt1, MPI_Fint *kt2,
             MPI_Fint *kw1, MPI_Fint *kw2,
             MPI_Fint *f11, MPI_Fint *f12, MPI_Fint *f13,
             MPI_Fint *fc21, MPI_Fint *fc22, MPI_Fint *fc23,
             MPI_Fint *ft21, MPI_Fint *ft22, MPI_Fint *ft23,
             MPI_Fint *fw21, MPI_Fint *fw22, MPI_Fint *fw23,
             MPI_Fint *win)
{ c_read_backend(k1, k2, kc1, kc2, kt1, kt2, kw1, kw2,
                 f11, f12, f13,
                 fc21, fc22, fc23, ft21, ft22, ft23, fw21, fw22, fw23, win); }
void c_read__(MPI_Fint *k1, MPI_Fint *k2,
              MPI_Fint *kc1, MPI_Fint *kc2,
              MPI_Fint *kt1, MPI_Fint *kt2,
              MPI_Fint *kw1, MPI_Fint *kw2,
              MPI_Fint *f11, MPI_Fint *f12, MPI_Fint *f13,
              MPI_Fint *fc21, MPI_Fint *fc22, MPI_Fint *fc23,
              MPI_Fint *ft21, MPI_Fint *ft22, MPI_Fint *ft23,
              MPI_Fint *fw21, MPI_Fint *fw22, MPI_Fint *fw23,
              MPI_Fint *win)
{ c_read_backend(k1, k2, kc1, kc2, kt1, kt2, kw1, kw2,
                 f11, f12, f13,
                 fc21, fc22, fc23, ft21, ft22, ft23, fw21, fw22, fw23, win); }
void C_READ(MPI_Fint *k1, MPI_Fint *k2,
            MPI_Fint *kc1, MPI_Fint *kc2,
            MPI_Fint *kt1, MPI_Fint *kt2,
            MPI_Fint *kw1, MPI_Fint *kw2,
            MPI_Fint *f11, MPI_Fint *f12, MPI_Fint *f13,
            MPI_Fint *fc21, MPI_Fint *fc22, MPI_Fint *fc23,
            MPI_Fint *ft21, MPI_Fint *ft22, MPI_Fint *ft23,
            MPI_Fint *fw21, MPI_Fint *fw22, MPI_Fint *fw23,
            MPI_Fint *win)
{ c_read_backend(k1, k2, kc1, kc2, kt1, kt2, kw1, kw2,
                 f11, f12, f13,
                 fc21, fc22, fc23, ft21, ft22, ft23, fw21, fw22, fw23, win); }

void c_check_mpi1(MPI_Fint *key, MPI_Fint *value)
{ c_check_mpi1_backend(key, value); }
void c_check_mpi1_(MPI_Fint *key, MPI_Fint *value)
{ c_check_mpi1_backend(key, value); }
void c_check_mpi1__(MPI_Fint *key, MPI_Fint *value)
{ c_check_mpi1_backend(key, value); }
void C_CHECK_MPI1(MPI_Fint *key, MPI_Fint *value)
{ c_check_mpi1_backend(key, value); }

void c_check_mpi2(MPI_Fint *key, MPI_Aint *value)
{ c_check_mpi2_backend(key, value); }
void c_check_mpi2_(MPI_Fint *key, MPI_Aint *value)
{ c_check_mpi2_backend(key, value); }
void c_check_mpi2__(MPI_Fint *key, MPI_Aint *value)
{ c_check_mpi2_backend(key, value); }
void C_CHECK_MPI2(MPI_Fint *key, MPI_Aint *value)
{ c_check_mpi2_backend(key, value); }

/* keyval storage */
static int c_keyval1;
static int c_keyval2;
static int c_keyval3;
static int c_comm_keyval1;
static int c_comm_keyval2;
static int c_comm_keyval3;
static int c_type_keyval1;
static int c_type_keyval2;
static int c_type_keyval3;
static int c_win_keyval1;
static int c_win_keyval2;
static int c_win_keyval3;

/* attribute storage */
static int c_value1 = C_VALUE1;
static int c_value2 = C_VALUE2;
static int c_value3 = C_VALUE3;
static int f_mpi1_value1 = FORTRAN_MPI1_VALUE1;
static int f_mpi1_value2 = FORTRAN_MPI1_VALUE2;
static int f_mpi1_value3 = FORTRAN_MPI1_VALUE3;
static int f_mpi2_value1 = FORTRAN_MPI2_VALUE1;
static int f_mpi2_value2 = FORTRAN_MPI2_VALUE2;
static int f_mpi2_value3 = FORTRAN_MPI2_VALUE3;

/************************************************************************/

static int comm_delete_fn(MPI_Comm comm, int keyval,
                          void* attribute_val_out,
                          void* extra_state)
{
    printf("In C comm delete function\n");
    return MPI_SUCCESS;
}

static int comm_copy_fn(MPI_Comm comm, int keyval,
                        void* extra_state,
                        void* attribute_val_in,
                        void* attribute_val_out,
                        int* flag)
{
    printf("In C comm copy function\n");
    *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
    return MPI_SUCCESS;
}

static int type_delete_fn(MPI_Datatype type, int keyval,
                          void* attribute_val_out,
                          void* extra_state)
{
    printf("In C datatype delete function\n");
    return MPI_SUCCESS;
}

static int type_copy_fn(MPI_Datatype type, int keyval,
                        void* extra_state,
                        void* attribute_val_in,
                        void* attribute_val_out,
                        int* flag)
{
    printf("In C datatype copy function\n");
    *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
    return MPI_SUCCESS;
}


static int win_delete_fn(MPI_Win win, int keyval,
                         void* attribute_val_out,
                         void* extra_state)
{
    printf("In C win delete function\n");
    return MPI_SUCCESS;
}

static int win_copy_fn(MPI_Win win, int keyval,
                       void* extra_state,
                       void* attribute_val_in,
                       void* attribute_val_out,
                       int* flag)
{
    printf("In C win copy function\n");
    *flag = 1;
   *(void**)attribute_val_out = attribute_val_in;
    return MPI_SUCCESS;
}


/************************************************************************/

static void c_create_keyval_backend(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                                    MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                                    MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                                    MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3)
{
    int key;

    printf("Creating C keyval 1\n");
    MPI_Keyval_create(comm_copy_fn, comm_delete_fn, &key, NULL);
    c_keyval1 = key;
    *k1 = (MPI_Fint) key;
    printf("Created C keyval 1: %d\n", key);

    printf("Creating C keyval 2\n");
    MPI_Keyval_create(comm_copy_fn, comm_delete_fn, &key, NULL);
    c_keyval2 = key;
    *k2 = (MPI_Fint) key;
    printf("Created C keyval 2: %d\n", key);

    printf("Creating C keyval 3\n");
    MPI_Keyval_create(MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN, &key, NULL);
    c_keyval3 = key;
    *k3 = (MPI_Fint) key;
    printf("Created C keyval 3: %d\n", key);

    /***********************/

    printf("Creating C comm keyval 1\n");
    MPI_Comm_create_keyval(comm_copy_fn, comm_delete_fn, &key, NULL);
    c_comm_keyval1 = key;
    *kc1 = (MPI_Fint) key;
    printf("Created C comm keyval 1: %d\n", key);

    printf("Creating C comm keyval 2\n");
    MPI_Comm_create_keyval(comm_copy_fn, comm_delete_fn, &key, NULL);
    c_comm_keyval2 = key;
    *kc2 = (MPI_Fint) key;
    printf("Created C comm keyval 2: %d\n", key);

    printf("Creating C comm keyval 3\n");
    MPI_Comm_create_keyval(MPI_COMM_NULL_COPY_FN, MPI_COMM_NULL_DELETE_FN,
                           &key, NULL);
    c_comm_keyval3 = key;
    *kc3 = (MPI_Fint) key;
    printf("Created C comm keyval 3: %d\n", key);

    /***********************/

    printf("Creating C type keyval 1\n");
    MPI_Type_create_keyval(type_copy_fn, type_delete_fn, &key, NULL);
    c_type_keyval1 = key;
    *kt1 = (MPI_Fint) key;
    printf("Created C type keyval 1: %d\n", key);

    printf("Creating C type keyval 2\n");
    MPI_Type_create_keyval(type_copy_fn, type_delete_fn, &key, NULL);
    c_type_keyval2 = key;
    *kt2 = (MPI_Fint) key;
    printf("Created C type keyval 2: %d\n", key);

    printf("Creating C type keyval 3\n");
    MPI_Type_create_keyval(MPI_TYPE_NULL_COPY_FN, MPI_TYPE_NULL_DELETE_FN,
                           &key, NULL);
    c_type_keyval3 = key;
    *kt3 = (MPI_Fint) key;
    printf("Created C type keyval 3: %d\n", key);

    /***********************/

    printf("Creating C win keyval 1\n");
    MPI_Win_create_keyval(win_copy_fn, win_delete_fn, &key, NULL);
    c_win_keyval1 = key;
    *kw1 = (MPI_Fint) key;
    printf("Created C win keyval 1: %d\n", key);

    printf("Creating C win keyval 2\n");
    MPI_Win_create_keyval(win_copy_fn, win_delete_fn, &key, NULL);
    c_win_keyval2 = key;
    *kw2 = (MPI_Fint) key;
    printf("Created C win keyval 2: %d\n", key);

    printf("Creating C win keyval 3\n");
    MPI_Win_create_keyval(MPI_WIN_NULL_COPY_FN, MPI_WIN_NULL_DELETE_FN,
                          &key, NULL);
    c_win_keyval3 = key;
    *kw3 = (MPI_Fint) key;
    printf("Created C win keyval 3: %d\n", key);
}

/************************************************************************/

static void c_write_backend(MPI_Fint *k1, MPI_Fint *k2, MPI_Fint *k3,
                            MPI_Fint *kc1, MPI_Fint *kc2, MPI_Fint *kc3,
                            MPI_Fint *kt1, MPI_Fint *kt2, MPI_Fint *kt3,
                            MPI_Fint *kw1, MPI_Fint *kw2, MPI_Fint *kw3,
                            MPI_Fint *win)
{
    MPI_Win c_win = MPI_Win_f2c(*win);

    printf("Writing C attr 1: keyval=%d\n", (int) *k1);
    MPI_Attr_put(MPI_COMM_SELF, (int) *k1, (void*) &c_value1);
    printf("Writing C attr 2: keyval=%d\n", (int) *k2);
    MPI_Attr_put(MPI_COMM_SELF, (int) *k2, (void*) &c_value2);
    printf("Writing C attr 3: keyval=%d\n", (int) *k3);
    MPI_Attr_put(MPI_COMM_SELF, (int) *k3, (void*) &c_value3);

    printf("Writing C comm attr 1\n");
    MPI_Comm_set_attr(MPI_COMM_SELF, (int) *kc1, (void*) &c_value1);
    printf("Writing C comm attr 2\n");
    MPI_Comm_set_attr(MPI_COMM_SELF, (int) *kc2, (void*) &c_value2);
    printf("Writing C comm attr 3\n");
    MPI_Comm_set_attr(MPI_COMM_SELF, (int) *kc3, (void*) &c_value3);

    printf("Writing C type attr 1\n");
    MPI_Type_set_attr(MPI_INTEGER, (int) *kt1, (void*) &c_value1);
    printf("Writing C type attr 2\n");
    MPI_Type_set_attr(MPI_INTEGER, (int) *kt2, (void*) &c_value2);
    printf("Writing C type attr 3\n");
    MPI_Type_set_attr(MPI_INTEGER, (int) *kt3, (void*) &c_value3);

    printf("Writing C win attr 1\n");
    MPI_Win_set_attr(c_win, (int) *kw1, (void*) &c_value1);
    printf("Writing C win attr 2\n");
    MPI_Win_set_attr(c_win, (int) *kw2, (void*) &c_value2);
    printf("Writing C win attr 3\n");
    MPI_Win_set_attr(c_win, (int) *kw3, (void*) &c_value3);
}

/************************************************************************/

static void c_read_backend(MPI_Fint *k1, MPI_Fint *k2,
                           MPI_Fint *kc1, MPI_Fint *kc2,
                           MPI_Fint *kt1, MPI_Fint *kt2,
                           MPI_Fint *kw1, MPI_Fint *kw2,
                           MPI_Fint *f11, MPI_Fint *f12, MPI_Fint *f13,
                           MPI_Fint *fc21, MPI_Fint *fc22, MPI_Fint *fc23,
                           MPI_Fint *ft21, MPI_Fint *ft22, MPI_Fint *ft23,
                           MPI_Fint *fw21, MPI_Fint *fw22, MPI_Fint *fw23,
                           MPI_Fint *win)
{
    MPI_Win c_win = MPI_Win_f2c(*win);

    printf("Case 1: C writes, C reads (MPI-1)\n");
    check((int) *k1, c_value1, INTEGER);
    check((int) *k2, c_value2, INTEGER);

    printf("Case 1: C writes, C reads (MPI-2 comm)\n");
    check_comm((int) *kc1, c_value1, INTEGER);
    check_comm((int) *kc2, c_value2, INTEGER);

    printf("Case 1: C writes, C reads (MPI-2 type)\n");
    check_type((int) *kt1, c_value1, INTEGER);
    check_type((int) *kt2, c_value2, INTEGER);

    printf("Case 1: C writes, C reads (MPI-2 win)\n");
    check_win(c_win, (int) *kw1, c_value1, INTEGER);
    check_win(c_win, (int) *kw2, c_value2, INTEGER);

    printf("Case 4: Fortran MPI-1 writes, C reads (MPI-1)\n");
    check((int) *f11, f_mpi1_value1, INTEGER);
    check((int) *f12, f_mpi1_value2, INTEGER);

    printf("Case 4: Fortran MPI-1 writes, C reads (MPI-2)\n");
    check_comm((int) *f11, f_mpi1_value1, INTEGER);
    check_comm((int) *f12, f_mpi1_value2, INTEGER);

    printf("Case 7: Fortran MPI-2 writes, C reads (MPI-1)\n");
    check((int) *fc21, f_mpi2_value1, AINT);
    check((int) *fc22, f_mpi2_value2, AINT);

    printf("Case 7: Fortran MPI-2 writes, C reads (MPI-2 comm)\n");
    check_comm((int) *fc21, f_mpi2_value1, AINT);
    check_comm((int) *fc22, f_mpi2_value2, AINT);

    printf("Case 7: Fortran MPI-2 writes, C reads (MPI-2 type)\n");
    check_type((int) *ft21, f_mpi2_value1, AINT);
    check_type((int) *ft22, f_mpi2_value2, AINT);

    printf("Case 7: Fortran MPI-2 writes, C reads (MPI-2 win)\n");
    check_win(c_win, (int) *fw21, f_mpi2_value1, AINT);
    check_win(c_win, (int) *fw22, f_mpi2_value2, AINT);
}

static void check(int key, int expected_value, attr_type_t type)
{
    int flag;
    void *value;
    int *ivalue;
    MPI_Aint *avalue;

    MPI_Attr_get(MPI_COMM_SELF, key, &value, &flag);
    if (0 == flag) {
        printf("ERROR: Reading attribute got flag==0 when expected flag==1\n");
        exit(-1);
    }
    if (INTEGER == type) {
        ivalue = (int*) value;
        if (expected_value != *ivalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   *ivalue, expected_value);
            exit(-1);
        }
    } else {
        avalue = (MPI_Aint*) value;
        if (expected_value != *avalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   (int) *avalue, expected_value);
            exit(-1);
        }
    }
}

static void check_comm(int key, int expected_value, attr_type_t type)
{
    int flag;
    void *value;
    int *ivalue;
    MPI_Aint *avalue;

    MPI_Comm_get_attr(MPI_COMM_SELF, key, &value, &flag);
    if (0 == flag) {
        printf("ERROR: Reading attribute got flag==0 when expected flag==1\n");
        exit(-1);
    }
    if (INTEGER == type) {
        ivalue = (int*) value;
        if (expected_value != *ivalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   *ivalue, expected_value);
            exit(-1);
        }
    } else {
        avalue = (MPI_Aint*) value;
        if (expected_value != *avalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   (int) *avalue, expected_value);
            exit(-1);
        }
    }
}


static void check_type(int key, int expected_value, attr_type_t type)
{
    int flag;
    void *value;
    int *ivalue;
    MPI_Aint *avalue;

    MPI_Type_get_attr(MPI_INTEGER, key, &value, &flag);
    if (0 == flag) {
        printf("ERROR: Reading attribute got flag==0 when expected flag==1\n");
        exit(-1);
    }
    if (INTEGER == type) {
        ivalue = (int*) value;
        if (expected_value != *ivalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   *ivalue, expected_value);
            exit(-1);
        }
    } else {
        avalue = (MPI_Aint*) value;
        if (expected_value != *avalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   (int) *avalue, expected_value);
            exit(-1);
        }
    }
}


static void check_win(MPI_Win win, int key, int expected_value, attr_type_t type)
{
    int flag;
    void *value;
    int *ivalue;
    MPI_Aint *avalue;

    MPI_Win_get_attr(win, key, &value, &flag);
    if (0 == flag) {
        printf("ERROR: Reading attribute got flag==0 when expected flag==1\n");
        exit(-1);
    }
    if (INTEGER == type) {
        ivalue = (int*) value;
        if (expected_value != *ivalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   *ivalue, expected_value);
            exit(-1);
        }
    } else {
        avalue = (MPI_Aint*) value;
        if (expected_value != *avalue) {
            printf("ERROR: Reading attribute got value==%d when expected %d\n",
                   (int) *avalue, expected_value);
            exit(-1);
        }
    }
}


static void c_check_mpi1_backend(MPI_Fint *key, MPI_Fint *value)
{
    int k = (int) *key;
    int *v;
    union {
        void *c_ptr;
        MPI_Fint f_integer;
    } xlate;

    if (k == c_keyval1 || k == c_comm_keyval1 || k == c_type_keyval1 || k == c_win_keyval1) {
        v = &c_value1;
    } else if (k == c_keyval2 || k == c_comm_keyval2 || k == c_type_keyval2 || k == c_win_keyval2) {
        v = &c_value2;
    } else {
        v = &c_value3;
    }

    xlate.f_integer = *value;
    if (sizeof(void*) == sizeof(MPI_Fint)) {
        if (xlate.c_ptr != v) {
            printf("Checking MPI-2 C attribute value backend (same size), got value=%x; expected %x\n",
                   (int)(uintptr_t) xlate.c_ptr, (int)(uintptr_t) v);
            exit(1);
        }
    } else {
        /* Type-pun through a union: reading a void* through a cast
           MPI_Fint* violates strict-aliasing rules, so optimizing
           compilers are free to miscompile it. */
        union {
            void *ptr;
            MPI_Fint f[sizeof(void*) / sizeof(MPI_Fint)];
        } probe;
        int pos;

        probe.ptr = (void*) 1;
        for (pos = 0; pos < (int) (sizeof(void*) / sizeof(MPI_Fint));
             ++pos) {
            if (probe.f[pos] == 1) {
                break;
            }
        }
        probe.ptr = (void*) v;
        if (probe.f[pos] != *value) {
            printf("Checking MPI-2 C attribute value backend (truncate), got value=%x; expected %x\n",
                   (int)(uintptr_t) xlate.c_ptr, (int) probe.f[pos]);
            exit(1);
        }
    }
}


static void c_check_mpi2_backend(MPI_Fint *key, MPI_Aint *value)
{
    int k = (int) *key;
    int *v;

    if (k == c_keyval1 || k == c_comm_keyval1 || k == c_type_keyval1 ||
        k == c_win_keyval1) {
        v = &c_value1;
    } else if (k == c_keyval2 || k == c_comm_keyval2 || k == c_type_keyval2 ||
        k == c_win_keyval2) {
        v = &c_value2;
    } else {
        v = &c_value3;
    }

    if (((int*) *value) != v) {
        printf("Checking MPI-2 C attribute value backend, got value=%x; expected %x\n",
               (int) *value, (int)(uintptr_t) v);
        exit(1);
    }
}
