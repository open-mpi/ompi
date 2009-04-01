/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/* The goal of this component is to wholly replace the underlying
   allocator with our internal ptmalloc2 allocator.  

   See the file README-open-mpi.txt for details of how it works. */

#include "opal_config.h"

/* Include <link.h> for _DYNAMIC */
#include <link.h>

#include "opal/constants.h"
#include "opal/mca/memory/memory.h"
#include "opal/memoryhooks/memory.h"

static int ptmalloc2_open(void);

const opal_memory_base_component_2_0_0_t mca_memory_ptmalloc2_component = {
    /* First, the mca_component_t struct containing meta information
       about the component itself */
    {
        OPAL_MEMORY_BASE_VERSION_2_0_0,

        /* Component name and version */
        "ptmalloc2",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        ptmalloc2_open,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
};

/* Public symbols */
bool opal_memory_ptmalloc2_free_invoked = false;
bool opal_memory_ptmalloc2_malloc_invoked = false;
bool opal_memory_ptmalloc2_realloc_invoked = false;
bool opal_memory_ptmalloc2_memalign_invoked = false;

static int ptmalloc2_open(void)
{
    /* We always provide munmap support as part of libopen-pal.la. */
    int val = OPAL_MEMORY_MUNMAP_SUPPORT;

    /* We can't catch munmap if we're a static library */
    if (NULL == &_DYNAMIC) {
        val = 0;
    }

    /* We will also provide malloc/free support if we've been
       activated.  We don't rely on the __malloc_initialize_hook()
       previously being called because it's possible that our hook was
       called, but then someone else reset the hooks to point to
       something else (i.e., before MPI_INIT).  So explicitly test
       here if our hooks are still in place.  If they are, then enable
       FREE|CUNK_SUPPORT.  If not, then don't enable that support --
       just leave it at MUNMAP_SUPPORT.

       (Look in hooks.c for the __malloc_initialize_hook setup) */

    /* Do a simple set of tests to see if our hooks are still the ones
       installed.  Explicitly reset the flags indicating that our
       functions were invoked */
    opal_memory_ptmalloc2_malloc_invoked = false;
    opal_memory_ptmalloc2_realloc_invoked = false;
    opal_memory_ptmalloc2_memalign_invoked = false;
    opal_memory_ptmalloc2_free_invoked = false;

    void *p = malloc(1024 * 1024 * 4);
    if (NULL == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    realloc(p, 1024 * 1024 * 4 + 32);
    free(p);
    memalign(1, 1024 * 1024);
    free(p);

    if (opal_memory_ptmalloc2_malloc_invoked &&
        opal_memory_ptmalloc2_realloc_invoked &&
        opal_memory_ptmalloc2_memalign_invoked &&
        opal_memory_ptmalloc2_free_invoked) {
        /* Happiness; our functions were invoked */
        val |= OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_CHUNK_SUPPORT;
    }

    /* Set the support level */
    opal_mem_hooks_set_support(val);

    return OPAL_SUCCESS;
}
