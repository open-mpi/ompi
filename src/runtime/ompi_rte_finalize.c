/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "runtime/runtime_internal.h"
#include "runtime/ompi_rte_wait.h"
#include "event/event.h"
#include "util/output.h"
#include "util/proc_info.h"
#include "threads/mutex.h"
#include "mca/llm/base/base.h"
#include "mca/pcm/base/base.h"
#include "mca/pcmclient/base/base.h"
#include "mca/oob/oob.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "util/session_dir.h"

/**
 * Leave a OMPI RTE.
 *
 * @retval OMPI_SUCCESS Upon success.
 * @retval OMPI_ERROR Upon failure.
 *
 * This function performs 
 */
int ompi_rte_finalize(void)
{
  ompi_rte_wait_finalize();
  ompi_rte_internal_fini_spawn();

  mca_pcm_base_close();
  mca_llm_base_close();
  mca_pcmclient_base_close();
  mca_ns_base_close();
  mca_gpr_base_close();
  mca_oob_base_close();

  ompi_event_fini();

#ifndef WIN32
    ompi_session_dir_finalize();
#endif

  /* All done */

  return OMPI_SUCCESS;
}
