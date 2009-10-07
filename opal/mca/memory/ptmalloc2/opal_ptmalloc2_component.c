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

#include <malloc.h>
#include <sys/mman.h>

#include "opal/constants.h"
#include "opal/mca/memory/memory.h"
#include "opal/memoryhooks/memory.h"

/* Need to call a function in hooks.c to ensure that all those symbols
   get pulled in at link time (e.g., when building libmpi.a, so that
   those symbols end up in the final executable -- especially if we
   use --disable-dlopen and therefore -Wl,--export-dynamic isn't used
   when we build OMPI). */
extern void *opal_memory_ptmalloc2_hook_pull(void);

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
bool opal_memory_ptmalloc2_munmap_invoked = false;

static int ptmalloc2_open(void)
{
    void *p;
    int val = 0;

    /* Call a dummy function in hooks.c.  ***Do not remove this
       call!*** See comment at the beginning of this file explaining
       why it is here. */
    p = opal_memory_ptmalloc2_hook_pull();

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
    opal_memory_ptmalloc2_munmap_invoked = false;

    p = malloc(1024 * 1024 * 4);
    if (NULL == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    p = realloc(p, 1024 * 1024 * 4 + 32);
    if (NULL == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    free(p);
    p = memalign(4, 1024 * 1024);
    if (NULL == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    free(p);

#if HAVE_POSIX_MEMALIGN
    /* Double check for posix_memalign, too */
    if (opal_memory_ptmalloc2_memalign_invoked) {
        opal_memory_ptmalloc2_memalign_invoked = false;
        if (0 != posix_memalign(&p, sizeof(void*), 1024 * 1024)) {
            return OPAL_ERR_IN_ERRNO;
        }
        free(p);
    }
#endif

    if (opal_memory_ptmalloc2_malloc_invoked &&
        opal_memory_ptmalloc2_realloc_invoked &&
        opal_memory_ptmalloc2_memalign_invoked &&
        opal_memory_ptmalloc2_free_invoked) {
        /* Happiness; our functions were invoked */
        val |= OPAL_MEMORY_FREE_SUPPORT | OPAL_MEMORY_CHUNK_SUPPORT;
    }

    p = mmap(NULL, 4096, PROT_READ, (MAP_ANONYMOUS | MAP_PRIVATE), -1, 0);
    if (MAP_FAILED == p) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    munmap(p, 4096);

    if (opal_memory_ptmalloc2_munmap_invoked) {
      val |= OPAL_MEMORY_MUNMAP_SUPPORT;
    }

    /* Set the support level */
    opal_mem_hooks_set_support(val);

    return OPAL_SUCCESS;
}
