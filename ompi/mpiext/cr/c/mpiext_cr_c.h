/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
#include "opal/runtime/opal_cr.h"

/********************************
 * C/R Interfaces
 ********************************/
/*
 * Request a checkpoint
 */
OMPI_DECLSPEC int OMPI_CR_Checkpoint(char **handle, int *seq, MPI_Info *info);

/*
 * Request a restart
 */
OMPI_DECLSPEC int OMPI_CR_Restart(char *handle, int seq, MPI_Info *info);


/********************************
 * Migration Interface
 ********************************/
/*
 * Request a migration
 */
OMPI_DECLSPEC int OMPI_CR_Migrate(MPI_Comm comm, char *hostname, int rank, MPI_Info *info);


/********************************
 * INC Interfaces
 ********************************/
typedef opal_cr_user_inc_callback_event_t OMPI_CR_INC_callback_event_t;

typedef opal_cr_user_inc_callback_state_t OMPI_CR_INC_callback_state_t;

typedef int (*OMPI_CR_INC_callback_function)(OMPI_CR_INC_callback_event_t event,
                                             OMPI_CR_INC_callback_state_t state);

OMPI_DECLSPEC int OMPI_CR_INC_register_callback(OMPI_CR_INC_callback_event_t event,
                                                OMPI_CR_INC_callback_function function,
                                                OMPI_CR_INC_callback_function *prev_function);


/********************************
 * SELF CRS Application Interfaces
 ********************************/
typedef int (*OMPI_CR_self_checkpoint_fn)(char **restart_cmd);
typedef int (*OMPI_CR_self_restart_fn)(void);
typedef int (*OMPI_CR_self_continue_fn)(void);

OMPI_DECLSPEC int OMPI_CR_self_register_checkpoint_callback(OMPI_CR_self_checkpoint_fn function);
OMPI_DECLSPEC int OMPI_CR_self_register_restart_callback(OMPI_CR_self_restart_fn function);
OMPI_DECLSPEC int OMPI_CR_self_register_continue_callback(OMPI_CR_self_continue_fn function);


/********************************
 * Quiescence Interfaces
 ********************************/
/*
 * Start the Quiescent region.
 * Note: 'comm' required to be MPI_COMM_WORLD
 */
OMPI_DECLSPEC int OMPI_CR_Quiesce_start(MPI_Comm comm, MPI_Info *info);

/*
 * Request a checkpoint during a quiescent region
 * Note: 'comm' required to be MPI_COMM_WORLD
 */
OMPI_DECLSPEC int OMPI_CR_Quiesce_checkpoint(MPI_Comm comm, char **handle, int *seq, MPI_Info *info);

/*
 * End the Quiescent Region
 * Note: 'comm' required to be MPI_COMM_WORLD
 */
OMPI_DECLSPEC int OMPI_CR_Quiesce_end(MPI_Comm comm, MPI_Info *info);
