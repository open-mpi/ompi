/*
 * Copyright 2002-2003. The Regents of the University of California. This material
 * was produced under U.S. Government contract W-7405-ENG-36 for Los Alamos
 * National Laboratory, which is operated by the University of California for
 * the U.S. Department of Energy. The Government is granted for itself and
 * others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
 * license in this material to reproduce, prepare derivative works, and
 * perform publicly and display publicly. Beginning five (5) years after
 * October 10,2002 subject to additional five-year worldwide renewals, the
 * Government is granted for itself and others acting on its behalf a paid-up,
 * nonexclusive, irrevocable worldwide license in this material to reproduce,
 * prepare derivative works, distribute copies to the public, perform publicly
 * and display publicly, and to permit others to do so. NEITHER THE UNITED
 * STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR THE UNIVERSITY OF
 * CALIFORNIA, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR
 * IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY,
 * COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR
 * PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD NOT INFRINGE PRIVATELY
 * OWNED RIGHTS.
                                                                                                     
 * Additionally, this program is free software; you can distribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the License,
 * or any later version.  Accordingly, this program is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
                                                                                                     
#ifndef _LAM_MALLOC_H_
#define _LAM_MALLOC_H_

#include <stdlib.h>
#include "lam_config.h"
#include "lam/util/lam_log.h"

/*
 * Set LAM_MALLOC_DEBUG_LEVEL to
 * 0 for no checking
 * 1 for basic error checking
 * 2 for more error checking
 */

#ifndef LAM_MALLOC_DEBUG_LEVEL
#define LAM_MALLOC_DEBUG_LEVEL 2
#endif

/*
 * malloc and free with error checking
 */

static inline void *_lam_malloc(ssize_t size, int debug_level, char *file, int line)
{
    void *addr = NULL;
    if (debug_level > 1) {
        if (size <= 0) {
            _lam_set_file_line(file, line);
            _lam_warn("Warning: ulm_malloc: Request for %ld bytes\n",
                      (long) size);
        }
    }
    addr = malloc((size_t) size);
    if (debug_level > 0) {
        if (addr == NULL) {
            _lam_set_file_line(file, line);
            _lam_err("Error: ulm_malloc: Request for %ld bytes failed\n",
                     (long) size);
        }
    }

    return addr;
}

static inline void _lam_free(void *addr, int debug_level, char *file, int line)
{
    if (debug_level > 1 && addr == NULL) {
        _lam_set_file_line(file, line);
        _lam_warn("Warning: ulm_free: Invalid pointer %p\n", addr);
        return;
    }
    free(addr);
}

#if LAM_MALLOC_DEBUG_LEVEL > 0

#define lam_malloc(SIZE) _lam_malloc(SIZE, LAM_MALLOC_DEBUG_LEVEL, __FILE__, __LINE__)
#define lam_free(ADDR) \
    do { _lam_free((ADDR), LAM_MALLOC_DEBUG_LEVEL, __FILE__, __LINE__); ADDR = NULL; } while (0)

#else

#define lam_malloc(SIZE) malloc(SIZE)
#define lam_free(ADDR) free(ADDR)

#endif
#endif
