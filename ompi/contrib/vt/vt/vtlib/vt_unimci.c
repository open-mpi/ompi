/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#include "config.h"

#include "vt_env.h"
#include "vt_error.h"
#include "vt_inttypes.h"
#include "vt_trc.h"
#include "vt_unimci.h"

#include <stdlib.h>

#include "otf.h"

#define MSG_TYPE_INFO    0
#define MSG_TYPE_WARNING 1
#define MSG_TYPE_ERROR   2

uint8_t vt_unimci_is_initialized = 0;

/* array of marker IDs */
static uint32_t marker_id[3];

/* do exit if an error message occurred */
static uint8_t exit_on_error;

void vt_unimci_init()
{
  vt_unimci_is_initialized = 1;

  /* define markers for UniMCI's message types */

  marker_id[MSG_TYPE_INFO] =
    vt_def_marker(UNIMCI_CHECKER_NAME" Info", OTF_MARKER_TYPE_HINT);
  marker_id[MSG_TYPE_WARNING] =
    vt_def_marker(UNIMCI_CHECKER_NAME" Warning", OTF_MARKER_TYPE_WARNING);
  marker_id[MSG_TYPE_ERROR] =
    vt_def_marker(UNIMCI_CHECKER_NAME" Error", OTF_MARKER_TYPE_ERROR);

  exit_on_error = (uint8_t)vt_env_mpicheck_errexit();
}

void vt_unimci_finalize()
{
}

void vt_unimci_check_msg(uint8_t record, uint64_t* time)
{
  /* check for messages */
  while( UNIMCI_has_msg() )
  {
    UNIMCI_MSG* msg;
    int msg_type;

    /* get first message */
    UNIMCI_pop_msg( &msg );
    vt_assert( msg );

    /* record marker, if allowed */
    if( record )
    {
      vt_assert( time );

      /* check for message type */
      switch( msg->msgType )
      {
        case UNIMCI_MSG_TYPE_WARNING:
          msg_type = MSG_TYPE_WARNING;
          break;
        case UNIMCI_MSG_TYPE_ERROR:
          msg_type = MSG_TYPE_ERROR;
          break;
        default: /* UNIMCI_MSG_TYPE_INFO */
          msg_type = MSG_TYPE_INFO;
          break;
      }

      /* write marker */
      vt_marker( time, marker_id[msg_type], msg->strText );
    }

    /* free message */
    UNIMCI_msg_free( &msg );

    /* do exit on error? */
    if( exit_on_error && record && msg_type == MSG_TYPE_ERROR )
    {
      vt_cntl_msg(1, "Application terminated due to "UNIMCI_CHECKER_NAME" "
                     "detected an error, see the trace for details");
      vt_def_comment("__VT_COMMENT__ WARNING: This trace is incomplete, "
                     "because "UNIMCI_CHECKER_NAME" detected an error.");

      /* this should invoke vt_close() to shutdown VampirTrace */
      exit(EXIT_FAILURE);
    }
  }
}
