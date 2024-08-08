/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_ENV_H_INCLUDED
#define MPL_ENV_H_INCLUDED

#include "mplconfig.h"

/* *INDENT-ON* */
#if defined(__cplusplus)
extern "C" {
#endif
/* *INDENT-OFF* */

#if defined(MPL_HAVE_PUTENV) && defined(MPL_NEEDS_PUTENV_DECL)
extern int putenv(char *string);
#endif

/* Prototypes for the functions to provide uniform access to the environment */
int MPL_env2int(const char *envName, int *val);
int MPL_env2range(const char *envName, int *lowPtr, int *highPtr);
int MPL_env2bool(const char *envName, int *val);
int MPL_env2str(const char *envName, const char **val);
int MPL_env2double(const char *envName, double *val);
int MPL_putenv(char *name_val);

/* *INDENT-ON* */
#if defined(__cplusplus)
}
#endif
/* *INDENT-OFF* */

#endif /* MPL_ENV_H_INCLUDED */
