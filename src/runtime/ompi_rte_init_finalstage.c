/*
 * $HEADER$
 */

/** @file **/

/* #define _GNU_SOURCE */

#include "ompi_config.h"

#include "include/constants.h"
#include "event/event.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/base/base.h"
#include "mca/pcmclient/base/base.h"
#include "mca/llm/base/base.h"
#include "mca/oob/oob.h"
#include "mca/ns/base/base.h"
#include "mca/gpr/base/base.h"
#include "util/proc_info.h"
#include "util/session_dir.h"
#include "util/sys_info.h"

#include "runtime/runtime.h"

int ompi_rte_init_finalstage(bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    int ret;

    /*
     * Call back into OOB to allow do any final initialization
     * (e.g. put contact info in register).
     */
    if (OMPI_SUCCESS != (ret = mca_oob_base_module_init())) {
       ompi_output(0, "ompi_rte_init: failed in mca_oob_base_module_init()\n");
       return ret;
    }

     /* 
     * All done 
     */
    return OMPI_SUCCESS;
}
