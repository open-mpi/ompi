/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/* 
 *
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */


/* header file for MPI-IO implementation. not intended to be
   user-visible */ 

#ifndef MPIOIMPL_INCLUDE
#define MPIOIMPL_INCLUDE

#include "adio.h"
#include "mpio.h"

#ifdef ROMIO_INSIDE_MPICH
#include "glue_romio.h"

#define ROMIO_THREAD_CS_ENTER() MPIR_Ext_cs_enter()
#define ROMIO_THREAD_CS_EXIT() MPIR_Ext_cs_exit()
#define ROMIO_THREAD_CS_YIELD() MPIR_Ext_cs_yield()

/* committed datatype checking support in ROMIO */
#define MPIO_DATATYPE_ISCOMMITTED(dtype_, err_)        \
    do {                                               \
        err_ =  MPIR_Ext_datatype_iscommitted(dtype_); \
    } while (0)

#else /* not ROMIO_INSIDE_MPICH */
/* Any MPI implementation that wishes to follow the thread-safety and
   error reporting features provided by MPICH must implement these 
   four functions.  Defining these as empty should not change the behavior 
   of correct programs */
#define ROMIO_THREAD_CS_ENTER()
#define ROMIO_THREAD_CS_EXIT()
#define ROMIO_THREAD_CS_YIELD()
#define MPIO_DATATYPE_ISCOMMITTED(dtype_, err_) do {} while (0)
#ifdef HAVE_WINDOWS_H
#define MPIU_UNREFERENCED_ARG(a) a
#else
#define MPIU_UNREFERENCED_ARG(a)
#endif
#endif /* ROMIO_INSIDE_MPICH */

/* info is a linked list of these structures */
struct MPIR_Info {
    int cookie;
    char *key, *value;
    struct MPIR_Info *next;
};

#define MPIR_INFO_COOKIE 5835657

MPI_Delete_function ADIOI_End_call;

/* common initialization routine */
void MPIR_MPIOInit(int * error_code);

#ifdef HAVE_MPIIO_CONST
#define ROMIO_CONST const
#else
#define ROMIO_CONST
#endif

#include "mpiu_external32.h"


#include "mpioprof.h"

#ifdef MPI_hpux
#  include "mpioinst.h"
#endif /* MPI_hpux */

#endif

