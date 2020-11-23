/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PNBC_OSC_SCHEDULE_H
#define PNBC_OSC_SCHEDULE_H

#include "opal/class/opal_object.h"
#include "ompi/request/request.h"

BEGIN_C_DECLS

typedef enum {
  PNBC_OSC_ROUND_INVALID,
  PNBC_OSC_ROUND_FLAG_BASED,
  PNBC_OSC_ROUND_REQUEST_BASED,
  PNBC_OSC_ROUND_DEFERED_INIT,
  PNBC_OSC_ROUND_RESTART_POINT,
  PNBC_OSC_ROUND_COMPLETION_POINT,
  PNBC_OSC_ROUND_GAME_OVER,
} PNBC_OSC_Round_type;

/* polymorphic schedule->round struct - specialised by request-based or flag-based */
struct PNBC_OSC_Round {
  opal_object_t super;
  PNBC_OSC_Round_type round_type;
};
typedef struct PNBC_OSC_Round PNBC_OSC_Round;

/* forward reference - needed for function pointer typedefs */
struct PNBC_OSC_Round_flag_based;
typedef struct PNBC_OSC_Round_flag_based PNBC_OSC_Round_flag_based;

/* function that allocates and returns a flag-based round, with undefined flag values */
typedef PNBC_OSC_Round_flag_based* (*PNBC_OSC_Init_flag_round_fn_t)(
  int flags
);

/* function that sets starting values for all flags in a flag-based round */
typedef void (*PNBC_OSC_Start_flag_round_fn_t)(
  PNBC_OSC_Round_flag_based *round
);

/* function that tests flag values against final values,
   returns 0 iff at least one flag is not final value */
typedef int (*PNBC_OSC_Test_flag_round_fn_t)(
  PNBC_OSC_Round_flag_based *round
);

/* function that deallocates a flag-based round, nullifies the round pointer */
typedef void (*PNBC_OSC_Free_flag_round_fn_t)(
  PNBC_OSC_Round_flag_based *round
);

/* struct describing a flag-based round,
   caution: must always be passed by reference because of flexible array member
*/
struct PNBC_OSC_Round_flag_based {
  PNBC_OSC_Round super;
  PNBC_OSC_Init_flag_round_fn_t initRound;
  PNBC_OSC_Start_flag_round_fn_t startRound;
  PNBC_OSC_Test_flag_round_fn_t testRound;
  PNBC_OSC_Free_flag_round_fn_t freeRound;
  int number_of_flags;
  int flags[];
};
typedef struct PNBC_OSC_Round_flag_based PNBC_OSC_Round_flag_based;

/* forward reference - needed for function pointer typedefs */
struct PNBC_OSC_Round_req_based;
typedef struct PNBC_OSC_Round_req_based PNBC_OSC_Round_req_based;

/* function that allocates and returns a request-based round, with undefined request elements */
typedef PNBC_OSC_Round_req_based* (*PNBC_OSC_Init_req_round_fn_t)(
  int reqs
);

/* function that starts a request-based round by calling MPI_Startall for all request elements */
typedef void (*PNBC_OSC_Start_req_round_fn_t)(
  PNBC_OSC_Round_req_based *round
);

/* function that tests a request-based round by calling MPI_Testall for all request elements */
typedef int (*PNBC_OSC_Test_req_round_fn_t)(
  PNBC_OSC_Round_req_based *round
);

/* function that deallocates a request-based round, nullifies the round pointer */
typedef void (*PNBC_OSC_Free_req_round_fn_t)(
  PNBC_OSC_Round_req_based *round
);

/* struct describing a request-based round,
   caution: must always be passed by reference because of flexible array member
*/
struct PNBC_OSC_Round_request_based {
  PNBC_OSC_Round super;
  PNBC_OSC_Init_req_round_fn_t initRound;
  PNBC_OSC_Start_req_round_fn_t startRound;
  PNBC_OSC_Test_req_round_fn_t testRound;
  PNBC_OSC_Free_req_round_fn_t freeRound;
  int number_of_requests;     // total number of requests in this round
  volatile int req_count;     // number of non-complete requests in this round
  ompi_request_t *requests[];
};
typedef struct PNBC_OSC_Round_request_based PNBC_OSC_Round_request_based;

/* struct holding the entire Schedule
   legacy NBC schedule will use heap memory pointed to by *data
   modern PNBC schedules will use the flexible rounds array
*/
struct PNBC_OSC_Schedule {
  opal_object_t super;
  int size;                 //DJH//should be obsolete
  long row_offset;          //DJH//should be obsolete
  int current_round_offset; //DJH//should be obsolete
  char *data;               //DJH//should be obsolete, except for nbc steps
  int number_of_rounds;     // length of array: rounds
  int restart_round;        // index into array: rounds
  PNBC_OSC_Round *rounds[]; // list of rounds (polymorphic)
};
typedef struct PNBC_OSC_Schedule PNBC_OSC_Schedule;
OBJ_CLASS_DECLARATION(PNBC_OSC_Schedule);


END_C_DECLS

#endif /* PNBC_OSC_SCHEDULE_H */
