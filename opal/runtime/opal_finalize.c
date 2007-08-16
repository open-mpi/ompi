/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "opal_config.h"

#include "opal/class/opal_object.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"
#include "opal/util/malloc.h"
#include "opal/util/if.h"
#include "opal/util/keyval_parse.h"
#include "opal/memoryhooks/memory.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"
#include "opal/mca/installdirs/base/base.h"
#include "opal/mca/memcpy/base/base.h"
#include "opal/mca/memory/base/base.h"
#include "opal/mca/backtrace/base/base.h"
#include "opal/mca/timer/base/base.h"
#include "opal/mca/paffinity/base/base.h"

int
opal_finalize_util(void)
{
    /* Clear out all the registered MCA params */
    mca_base_param_finalize();

    /* close interfaces code.  This is lazy opened, but protected from
       close when not opened internally */
    opal_iffinalize();

    /* keyval lex-based parser */
    opal_util_keyval_parse_finalize();

    opal_installdirs_base_close();

    /* finalize the memory allocator */
    opal_malloc_finalize();

    /* finalize the trace system */
    opal_trace_finalize();

    /* finalize the output system.  This has to come *after* the
       malloc code, as the malloc code needs to call into this, but
       the malloc code turning off doesn't affect opal_output that
       much */
    opal_output_finalize();

    /* finalize the class/object system */
    opal_class_finalize();

    return OPAL_SUCCESS;
}

extern int opal_initialized;

int
opal_finalize(void)
{
    if( --opal_initialized != 0 ) {
        if( opal_initialized < 0 ) {
            return OPAL_ERROR;
        }
        return OPAL_SUCCESS;
    }
    /* close high resolution timers */
    opal_timer_base_close();

    opal_backtrace_base_close();

    /* close the memory manager components.  Registered hooks can
       still be fired any time between now and the call to
       opal_mem_free_finalize(), and callbacks from the memory manager
       hooks to the bowels of the mem_free code can still occur any
       time between now and end of application (even post main()!) */
    opal_memory_base_close();

    /* finalize the memory manager / tracker */
    opal_mem_hooks_finalize();

    /* close the processor affinity base */
    opal_paffinity_base_close();

    /* close the memcpy base */
    opal_memcpy_base_close();

    /* finalize the mca */
    mca_base_close();

    /* finalize util code */
    opal_finalize_util();

    return OPAL_SUCCESS;
}
