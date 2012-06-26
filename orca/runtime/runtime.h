/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORCA_RUNTIME_H
#define ORCA_RUNTIME_H

#include "orca_config.h"

BEGIN_C_DECLS


/*******************************************
 * Global Variables
 *******************************************/
/*
 * Shared counter for the number of times init/finalize have been called
 */
ORCA_DECLSPEC extern int orca_init_counter;


/*******************************************
 * ...
 *******************************************/

END_C_DECLS

#endif /* ORCA_RUNTIME_H */
