/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPL_ARGSTR_H_INCLUDED
#define MPL_ARGSTR_H_INCLUDED

#include "mplconfig.h"

/* Make sure that we have the definitions for the malloc routines and size_t */
#include <stdio.h>
#include <stdlib.h>
#include "mpl_dbg.h"

#if defined (MPL_USE_DBG_LOGGING)
extern MPL_dbg_class MPIR_DBG_STRING;
#endif /* MPL_USE_DBG_LOGGING */

#define MPL_STR_SUCCESS    0
#define MPL_STR_FAIL       1
#define MPL_STR_NOMEM      2
#define MPL_STR_TRUNCATED  3

#define MPL_STR_QUOTE_CHAR     '\"'
#define MPL_STR_QUOTE_STR      "\""
#define MPL_STR_DELIM_CHAR     '#'
#define MPL_STR_DELIM_STR      "#"
#define MPL_STR_ESCAPE_CHAR    '\\'
#define MPL_STR_HIDE_CHAR      '*'
#define MPL_STR_SEPAR_CHAR     '$'
#define MPL_STR_SEPAR_STR      "$"

int MPL_str_get_string_arg(const char *str, const char *key, char *val, int maxlen);
int MPL_str_get_binary_arg(const char *str, const char *key, char *buffer,
                           int maxlen, int *out_length);
int MPL_str_get_int_arg(const char *str, const char *key, int *val_ptr);
int MPL_str_add_string_arg(char **str_ptr, int *maxlen_ptr, const char *key, const char *val);
int MPL_str_add_binary_arg(char **str_ptr, int *maxlen_ptr, const char *key,
                           const char *buffer, int length);
int MPL_str_add_int_arg(char **str_ptr, int *maxlen_ptr, const char *key, int val);
int MPL_str_add_string(char **str_ptr, int *maxlen_ptr, const char *val);
int MPL_str_get_string(char **str_ptr, char *val, int maxlen);

#endif /* MPL_ARGSTR_H_INCLUDED */
