/*
 * $HEADER$
 *
 * This file is included at the bottom of lam_config.h, and is
 * therefore a) after all the #define's that were output from
 * configure, and b) included in most/all files in LAM/MPI.
 *
 * Since this file is *only* ever included by lam_config.h, and
 * lam_config.h already has #ifndef/#endif protection, there is no
 * need to #ifndef/#endif protection here.
 */

/* If we're in C, bring in the bool type and true/false constants. */

#ifndef __cplusplus
#ifdef HAVE_STDBOOL_H
#include <stdbool.h>
#else
typedef enum { false, true } bool;
#endif
#endif
