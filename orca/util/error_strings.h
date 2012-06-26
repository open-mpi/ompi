/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _ORCA_ERROR_STRINGS_H_
#define _ORCA_ERROR_STRINGS_H_

#include "orca_config.h"

BEGIN_C_DECLS

ORCA_DECLSPEC int orca_err2str(int errnum, const char **errmsg);

END_C_DECLS

#endif /* _ORCA_ERROR_STRINGS_H_ */
