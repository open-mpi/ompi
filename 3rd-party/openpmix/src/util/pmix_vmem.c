/*
 * Copyright (c) 2021-2022 Triad National Security, LLC. All rights reserved.
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"
#include "src/util/pmix_vmem.h"

#include "src/include/pmix_globals.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_string_copy.h"

#ifdef HAVE_ERRNO_H
#include "errno.h"
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <sys/mman.h>

#define PMIX_VMEM_ALIGN2MB  (2  * 1024 * 1024UL)
#define PMIX_VMEM_ALIGN64MB (64 * 1024 * 1024UL)

typedef enum {
    VMEM_MAP_FILE = 0,
    VMEM_MAP_ANONYMOUS = 1,
    VMEM_MAP_HEAP = 2,
    VMEM_MAP_STACK = 3,
    /** vsyscall/vdso/vvar shouldn't occur since we stop after stack. */
    VMEM_MAP_OTHER = 4
} pmix_vmem_map_kind_t;

static int
parse_map_line(
    const char *line,
    unsigned long *beginp,
    unsigned long *endp,
    pmix_vmem_map_kind_t *kindp
) {
    const char *tmp = line, *next;
    unsigned long value;

    /* "beginaddr-endaddr " */
    value = strtoull(tmp, (char **) &next, 16);
    if (next == tmp) {
        return PMIX_ERROR;
    }

    *beginp = (unsigned long) value;

    if (*next != '-') {
        return PMIX_ERROR;
    }

    tmp = next + 1;

    value = strtoull(tmp, (char **) &next, 16);
    if (next == tmp) {
        return PMIX_ERROR;
    }
    *endp = (unsigned long) value;
    tmp = next;

    if (*next != ' ') {
        return PMIX_ERROR;
    }
    tmp = next + 1;

    /* look for ending absolute path */
    next = strchr(tmp, '/');
    if (next) {
        *kindp = VMEM_MAP_FILE;
    } else {
        /* look for ending special tag [foo] */
        next = strchr(tmp, '[');
        if (next) {
            if (!strncmp(next, "[heap]", 6)) {
                *kindp = VMEM_MAP_HEAP;
            } else if (!strncmp(next, "[stack]", 7)) {
                *kindp = VMEM_MAP_STACK;
            } else {
                char *end;
                if ((end = strchr(next, '\n')) != NULL) {
                    *end = '\0';
                }
                *kindp = VMEM_MAP_OTHER;
            }
        } else {
            *kindp = VMEM_MAP_ANONYMOUS;
        }
    }

    return PMIX_SUCCESS;
}

static pmix_status_t
use_hole(
    unsigned long holebegin,
    unsigned long holesize,
    unsigned long *addrp,
    unsigned long size
) {
    unsigned long aligned;
    unsigned long middle = holebegin + holesize / 2;

    if (holesize < size) {
        return PMIX_ERROR;
    }

    /* try to align the middle of the hole on 64MB for POWER's 64k-page PMD */
    aligned = (middle + PMIX_VMEM_ALIGN64MB) & ~(PMIX_VMEM_ALIGN64MB - 1);
    if (aligned + size <= holebegin + holesize) {
        *addrp = aligned;
        return PMIX_SUCCESS;
    }

    /* try to align the middle of the hole on 2MB for x86 PMD */
    aligned = (middle + PMIX_VMEM_ALIGN2MB) & ~(PMIX_VMEM_ALIGN2MB - 1);
    if (aligned + size <= holebegin + holesize) {
        *addrp = aligned;
        return PMIX_SUCCESS;
    }

    /* just use the end of the hole */
    *addrp = holebegin + holesize - size;
    return PMIX_SUCCESS;
}

pmix_status_t
pmix_vmem_find_hole(
    pmix_vmem_hole_kind_t hkind,
    size_t *addrp,
    size_t size
) {
    unsigned long biggestbegin = 0;
    unsigned long biggestsize = 0;
    unsigned long prevend = 0;
    pmix_vmem_map_kind_t prevmkind = VMEM_MAP_OTHER;
    int in_libs = 0;
    FILE *file;
    char line[96];

    file = fopen("/proc/self/maps", "r");
    if (!file) {
        return PMIX_ERROR;
    }

    while (fgets(line, sizeof(line), file) != NULL) {
        unsigned long begin = 0, end = 0;
        pmix_vmem_map_kind_t mkind = VMEM_MAP_OTHER;

        if (!parse_map_line(line, &begin, &end, &mkind)) {
            switch (hkind) {
                case VMEM_HOLE_BEGIN:
                    fclose(file);
                    return use_hole(0, begin, addrp, size);

                case VMEM_HOLE_AFTER_HEAP:
                    if (prevmkind == VMEM_MAP_HEAP && mkind != VMEM_MAP_HEAP) {
                        /* only use HEAP when there's no other HEAP after it
                         * (there can be several of them consecutively).
                         */
                        fclose(file);
                        return use_hole(prevend, begin - prevend, addrp, size);
                    }
                    break;

                case VMEM_HOLE_BEFORE_STACK:
                    if (mkind == VMEM_MAP_STACK) {
                        fclose(file);
                        return use_hole(prevend, begin - prevend, addrp, size);
                    }
                    break;

                case VMEM_HOLE_IN_LIBS:
                    /* see if we are between heap and stack */
                    if (prevmkind == VMEM_MAP_HEAP) {
                        in_libs = 1;
                    }
                    if (mkind == VMEM_MAP_STACK) {
                        in_libs = 0;
                    }
                    if (!in_libs) {
                        /* we're not in libs, ignore this entry */
                        break;
                    }
                    /* we're in libs, consider this entry for searching the biggest hole below */
                    /* fallthrough */

                case VMEM_HOLE_BIGGEST:
                    if (begin - prevend > biggestsize) {
                        biggestbegin = prevend;
                        biggestsize = begin - prevend;
                    }
                    break;

                default:
                    assert(0);
            }
        }

        while (!strchr(line, '\n')) {
            if (!fgets(line, sizeof(line), file)) {
                goto done;
            }
        }

        if (mkind == VMEM_MAP_STACK) {
            /* Don't go beyond the stack. Other VMAs are special (vsyscall, vvar, vdso, etc),
             * There's no spare room there. And vsyscall is even above the userspace limit.
             */
            break;
        }

        prevend = end;
        prevmkind = mkind;
    }

done:
    fclose(file);
    if (hkind == VMEM_HOLE_IN_LIBS || hkind == VMEM_HOLE_BIGGEST) {
        return use_hole(biggestbegin, biggestsize, addrp, size);
    }

    return PMIX_ERROR;
}
