/*
 * Copyright (c) 2017      Intel, Inc. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"

#include "opal/util/vmtracker.h"

typedef enum {
    OPAL_VM_MAP_FILE = 0,
    OPAL_VM_MAP_ANONYMOUS = 1,
    OPAL_VM_MAP_HEAP = 2,
    OPAL_VM_MAP_STACK = 3,
    OPAL_VM_MAP_OTHER = 4 /* vsyscall/vdso/vvar shouldn't occur since we stop after stack */
} opal_vm_map_kind_t;

typedef struct {
    opal_list_item_t super;
    size_t start;
    size_t size;
} opal_vmtracker_t;
OBJ_CLASS_INSTANCE(opal_vmtracker_t,
                   opal_list_item_t,
                   NULL, NULL);

static bool vmtracker_debug = false;
static bool initialized = false;
static opal_list_t assignments;

static int parse_map_line(const char *line,
                          unsigned long *beginp,
                          unsigned long *endp,
                          opal_vm_map_kind_t *kindp);
static int use_hole(unsigned long holebegin,
                    unsigned long holesize,
                    unsigned long *addrp,
                    unsigned long size);

void opal_vmtracker_init(void)
{
    if (NULL != getenv("OPAL_VMTRACKER_DEBUG")) {
        vmtracker_debug = true;
    }
    OBJ_CONSTRUCT(&assignments, opal_list_t);
    initialized = true;
}

void opal_vmtracker_finalize(void)
{
    OPAL_LIST_DESTRUCT(&assignments);
    initialized = false;
}

int opal_vmtracker_assign_address(opal_vm_hole_kind_t hkind,
                                  size_t *addrp, size_t size)
{
    unsigned long biggestbegin = 0;
    unsigned long biggestsize = 0;
    unsigned long prevend = 0;
    opal_vm_map_kind_t prevmkind = OPAL_VM_MAP_OTHER;
    int in_libs = 0;
    FILE *file;
    char line[96];

    if (!initialized) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    file = fopen("/proc/self/maps", "r");
    if (!file) {
        return OPAL_ERROR;
    }

    while (fgets(line, sizeof(line), file) != NULL) {
        unsigned long begin=0, end=0;
        opal_vm_map_kind_t mkind=OPAL_VM_MAP_OTHER;

        if (!parse_map_line(line, &begin, &end, &mkind)) {
            if (vmtracker_debug) {
                opal_output(0, "found %s from 0x%lx to 0x%lx\n",
                            mkind == OPAL_VM_MAP_HEAP ? "HEAP" :
                            mkind == OPAL_VM_MAP_STACK ? "STACK" :
                            mkind == OPAL_VM_MAP_OTHER ? "OTHER" :
                            mkind == OPAL_VM_MAP_FILE ? "FILE" :
                            mkind == OPAL_VM_MAP_ANONYMOUS ? "ANON" : "unknown",
                            begin, end);
            }

            switch (hkind) {
                case OPAL_VM_HOLE_BEGIN:
                    fclose(file);
                    return use_hole(0, begin, addrp, size);

                case OPAL_VM_HOLE_AFTER_HEAP:
                    if (prevmkind == OPAL_VM_MAP_HEAP && mkind != OPAL_VM_MAP_HEAP) {
                        /* only use HEAP when there's no other HEAP after it
                         * (there can be several of them consecutively).
                         */
                        fclose(file);
                        return use_hole(prevend, begin-prevend, addrp, size);
                    }
                    break;

                case OPAL_VM_HOLE_BEFORE_STACK:
                    if (mkind == OPAL_VM_MAP_STACK) {
                        fclose(file);
                        return use_hole(prevend, begin-prevend, addrp, size);
                    }
                    break;

                case OPAL_VM_HOLE_IN_LIBS:
                    /* see if we are between heap and stack */
                    if (prevmkind == OPAL_VM_MAP_HEAP) {
                        in_libs = 1;
                    }
                    if (mkind == OPAL_VM_MAP_STACK) {
                        in_libs = 0;
                    }
                    if (!in_libs) {
                        /* we're not in libs, ignore this entry */
                        break;
                    }
                    /* we're in libs, consider this entry for searching the biggest hole below */
                    /* fallthrough */

                case OPAL_VM_HOLE_BIGGEST:
                    if (begin-prevend > biggestsize) {
                        if (vmtracker_debug) {
                            opal_output(0, "new biggest 0x%lx - 0x%lx = %lu (%lu MB)\n",
                                        prevend, begin, begin-prevend, (begin-prevend)>>20);
                        }
                        biggestbegin = prevend;
                        biggestsize = begin-prevend;
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

        if (mkind == OPAL_VM_MAP_STACK) {
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
    if (hkind == OPAL_VM_HOLE_IN_LIBS || hkind == OPAL_VM_HOLE_BIGGEST) {
        return use_hole(biggestbegin, biggestsize, addrp, size);
    }

    return OPAL_ERROR;
}

#define ALIGN2MB (2*1024*1024UL)

static int use_hole(unsigned long holebegin,
                    unsigned long holesize,
                    unsigned long *addrp,
                    unsigned long size)
{
    unsigned long aligned;
    unsigned long middle = holebegin+holesize/2;
    opal_vmtracker_t *trk;

    if (vmtracker_debug) {
        opal_output(0, "looking in hole [0x%lx-0x%lx] size %lu (%lu MB) for %lu (%lu MB)\n",
                    holebegin, holebegin+holesize, holesize, holesize>>20, size, size>>20);
    }

    if (holesize < size) {
        return OPAL_ERROR;
    }

    /* try to align the middle of the hole on 64MB for POWER's 64k-page PMD */
    #define ALIGN64MB (64*1024*1024UL)
    aligned = (middle + ALIGN64MB) & ~(ALIGN64MB-1);

  retrypower:
    /* do we already have someone assigned here? */
    OPAL_LIST_FOREACH(trk, &assignments, opal_vmtracker_t) {
        /* see if there is any overlap */
        if (trk->start <= aligned) {
            if ((trk->start + trk->size) > aligned) {
                /* need to find another spot */
                aligned += ALIGN64MB;
                goto retrypower;
            }
        } else if (trk->start < (aligned + size)) {
                /* need to find another spot */
                aligned += ALIGN64MB;
                goto retrypower;
        }
    }
    if (aligned + size <= holebegin + holesize) {
        if (vmtracker_debug) {
            opal_output(0, "aligned [0x%lx-0x%lx] (middle 0x%lx) to 0x%lx for 64MB\n",
                        holebegin, holebegin+holesize, middle, aligned);
            opal_output(0, " there are %lu MB free before and %lu MB free after\n",
                        (aligned-holebegin)>>20, (holebegin+holesize-aligned-size)>>20);
        }

        *addrp = aligned;
        trk = OBJ_NEW(opal_vmtracker_t);
        trk->start = aligned;
        trk->size = size;
        opal_list_append(&assignments, &trk->super);
        return OPAL_SUCCESS;
    }

    /* try to align the middle of the hole on 2MB for x86 PMD */
    aligned = (middle + ALIGN2MB) & ~(ALIGN2MB-1);
  retryx86:
    /* do we already have someone assigned here? */
    OPAL_LIST_FOREACH(trk, &assignments, opal_vmtracker_t) {
        /* see if there is any overlap */
        if (trk->start <= aligned) {
            if ((trk->start + trk->size) > aligned) {
                /* need to find another spot */
                aligned += ALIGN64MB;
                goto retryx86;
            }
        } else if (trk->start < (aligned + size)) {
                /* need to find another spot */
                aligned += ALIGN64MB;
                goto retryx86;
        }
    }
    if (aligned + size <= holebegin + holesize) {
        if (vmtracker_debug) {
            opal_output(0, "aligned [0x%lx-0x%lx] (middle 0x%lx) to 0x%lx for 2MB\n",
                        holebegin, holebegin+holesize, middle, aligned);
            opal_output(0, " there are %lu MB free before and %lu MB free after\n",
                        (aligned-holebegin)>>20, (holebegin+holesize-aligned-size)>>20);
        }
        *addrp = aligned;
        trk = OBJ_NEW(opal_vmtracker_t);
        trk->start = aligned;
        trk->size = size;
        opal_list_append(&assignments, &trk->super);
        return OPAL_SUCCESS;
    }

    /* just use the end of the hole */
    aligned = holebegin + holesize - size;
        /* do we already have someone assigned here? */
    OPAL_LIST_FOREACH(trk, &assignments, opal_vmtracker_t) {
        /* see if there is any overlap */
        if (trk->start <= aligned) {
            if ((trk->start + trk->size) > aligned) {
                return OPAL_ERR_OUT_OF_RESOURCE;
            }
        } else if (trk->start < (aligned + size)) {
                return OPAL_ERR_OUT_OF_RESOURCE;
        }
    }
    *addrp = aligned;
    trk = OBJ_NEW(opal_vmtracker_t);
    trk->start = aligned;
    trk->size = size;
    opal_list_append(&assignments, &trk->super);

    if (vmtracker_debug) {
        opal_output(0, "using the end of hole starting at 0x%lx\n", *addrp);
        opal_output(0, " there are %lu MB free before\n", (*addrp-holebegin)>>20);
    }
    return OPAL_SUCCESS;
}

static int parse_map_line(const char *line,
                          unsigned long *beginp,
                          unsigned long *endp,
                          opal_vm_map_kind_t *kindp)
{
    const char *tmp = line, *next;
    unsigned long value;

    /* "beginaddr-endaddr " */
    value = strtoull(tmp, (char **) &next, 16);
    if (next == tmp) {
        return OPAL_ERROR;
    }

    *beginp = (unsigned long) value;

    if (*next != '-') {
        return OPAL_ERROR;
    }

     tmp = next + 1;

    value = strtoull(tmp, (char **) &next, 16);
    if (next == tmp) {
        return OPAL_ERROR;
    }
    *endp = (unsigned long) value;
    tmp = next;

    if (*next != ' ') {
        return OPAL_ERROR;
    }
    tmp = next + 1;

    /* look for ending absolute path */
    next = strchr(tmp, '/');
    if (next) {
        *kindp = OPAL_VM_MAP_FILE;
    } else {
        /* look for ending special tag [foo] */
        next = strchr(tmp, '[');
        if (next) {
            if (!strncmp(next, "[heap]", 6)) {
                *kindp = OPAL_VM_MAP_HEAP;
            } else if (!strncmp(next, "[stack]", 7)) {
                *kindp = OPAL_VM_MAP_STACK;
            } else {
                char *end;
                if ((end = strchr(next, '\n')) != NULL) {
                    *end = '\0';
                }
                if (vmtracker_debug) {
                    opal_output(0, "Found special VMA \"%s\" before stack", next);
                }
                *kindp = OPAL_VM_MAP_OTHER;
            }
        } else {
            *kindp = OPAL_VM_MAP_ANONYMOUS;
        }
    }

    return OPAL_SUCCESS;
}
