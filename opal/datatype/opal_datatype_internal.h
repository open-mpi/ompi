/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2021      IBM Corporation. All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED

#include "opal_config.h"

#include <stdarg.h>
#include <string.h>
#include <float.h>

#if defined(VERBOSE)
#    include "opal/util/output.h"

extern int opal_datatype_dfd;

#    define DDT_DUMP_STACK(PSTACK, STACK_POS, PDESC, NAME) \
        opal_datatype_dump_stack((PSTACK), (STACK_POS), (PDESC), (NAME))

#    define DUMP(...) opal_output(opal_datatype_dfd, __VA_ARGS__)

#else

#    define DDT_DUMP_STACK(PSTACK, STACK_POS, PDESC, NAME)
#    define DUMP(...)

#endif /* VERBOSE */

/*
 * There 3 types of predefined data types.
 * - the basic one composed by just one basic datatype which are
 *   definitively contiguous
 * - the derived ones where the same basic type is used multiple times.
 *   They should be most of the time contiguous.
 * - and finally the derived one where multiple basic types are used.
 *   Depending on the architecture they can be contiguous or not.
 *
 * At the OPAL-level we do not care from which language the datatype came from
 * (C, C++ or FORTRAN), we only focus on their internal representation in
 * the host memory. There is one notable exception, the long predefined type
 * which need to be handled at the lowest level due to it's variable size but
 * fixed XDR representation.
 *
 * NOTE: This predefined datatype order should be matched by any upper-level
 * users of the OPAL datatype.
 */
#define OPAL_DATATYPE_LOOP                0
#define OPAL_DATATYPE_END_LOOP            1
#define OPAL_DATATYPE_LB                  2
#define OPAL_DATATYPE_UB                  3
#define OPAL_DATATYPE_FIRST_TYPE          4 /* Number of first real type */
#define OPAL_DATATYPE_INT1                4
#define OPAL_DATATYPE_INT2                5
#define OPAL_DATATYPE_INT4                6
#define OPAL_DATATYPE_INT8                7
#define OPAL_DATATYPE_INT16               8
#define OPAL_DATATYPE_UINT1               9
#define OPAL_DATATYPE_UINT2               10
#define OPAL_DATATYPE_UINT4               11
#define OPAL_DATATYPE_UINT8               12
#define OPAL_DATATYPE_UINT16              13
#define OPAL_DATATYPE_FLOAT2              14
#define OPAL_DATATYPE_FLOAT4              15
#define OPAL_DATATYPE_FLOAT8              16
#define OPAL_DATATYPE_FLOAT12             17
#define OPAL_DATATYPE_FLOAT16             18
#define OPAL_DATATYPE_SHORT_FLOAT_COMPLEX 19
#define OPAL_DATATYPE_FLOAT_COMPLEX       20
#define OPAL_DATATYPE_DOUBLE_COMPLEX      21
#define OPAL_DATATYPE_LONG_DOUBLE_COMPLEX 22
#define OPAL_DATATYPE_BOOL                23
#define OPAL_DATATYPE_WCHAR               24
#define OPAL_DATATYPE_LONG                25
#define OPAL_DATATYPE_UNSIGNED_LONG       26
#define OPAL_DATATYPE_FLOAT128_COMPLEX    27
#define OPAL_DATATYPE_UNAVAILABLE         28

#ifndef OPAL_DATATYPE_MAX_PREDEFINED
#    define OPAL_DATATYPE_MAX_PREDEFINED (OPAL_DATATYPE_UNAVAILABLE + 1)
#elif OPAL_DATATYPE_MAX_PREDEFINED <= OPAL_DATATYPE_UNAVAILABLE
/*
 * If the number of basic datatype should change update
 * OPAL_DATATYPE_MAX_PREDEFINED in opal_datatype.h
 */
#    error OPAL_DATATYPE_MAX_PREDEFINED should be updated to the next value after the OPAL_DATATYPE_UNAVAILABLE define
#endif

#define DT_INCREASE_STACK 8

BEGIN_C_DECLS

struct ddt_elem_id_description {
    uint16_t flags; /**< flags for the record */
    uint16_t type;  /**< the basic data type id */
};
typedef struct ddt_elem_id_description ddt_elem_id_description;

/**
 * The data element description. It is similar to a vector type, a contiguous
 * blocklen number of basic elements, with a displacement for the first element
 * and then an extent for all the extra count.
 */
struct ddt_elem_desc {
    ddt_elem_id_description common; /**< basic data description and flags */
    uint32_t count;                 /**< number of blocks */
    size_t blocklen;                /**< number of elements on each block */
    ptrdiff_t extent;               /**< extent of each block (in bytes) */
    ptrdiff_t disp;                 /**< displacement of the first block */
};
typedef struct ddt_elem_desc ddt_elem_desc_t;

/**
 * The loop description, with it's two markers: one for the beginning and one for
 * the end. The initial marker contains the number of repetitions, the number of
 * elements in the loop, and the extent of each loop. The end marker contains in
 * addition to the number of elements (so that we can easily pair together the
 * two markers), the size of the data contained inside and the displacement of
 * the first element.
 */
struct ddt_loop_desc {
    ddt_elem_id_description common; /**< basic data description and flags */
    uint32_t items;                 /**< number of items in the loop */
    uint32_t loops;                 /**< number of elements */
    size_t unused;                  /**< not used right now */
    ptrdiff_t extent;               /**< extent of the whole loop */
};
typedef struct ddt_loop_desc ddt_loop_desc_t;

struct ddt_endloop_desc {
    ddt_elem_id_description common; /**< basic data description and flags */
    uint32_t items;                 /**< number of elements */
    uint32_t unused;                /**< not used right now */
    size_t size;                    /**< real size of the data in the loop */
    ptrdiff_t first_elem_disp;      /**< the displacement of the first block in the loop */
};
typedef struct ddt_endloop_desc ddt_endloop_desc_t;

union dt_elem_desc {
    ddt_elem_desc_t elem;
    ddt_loop_desc_t loop;
    ddt_endloop_desc_t end_loop;
};

#define CREATE_LOOP_START(_place, _count, _items, _extent, _flags)         \
    do {                                                                   \
        (_place)->loop.common.type = OPAL_DATATYPE_LOOP;                   \
        (_place)->loop.common.flags = (_flags) & ~OPAL_DATATYPE_FLAG_DATA; \
        (_place)->loop.loops = (_count);                                   \
        (_place)->loop.items = (_items);                                   \
        (_place)->loop.extent = (_extent);                                 \
        (_place)->loop.unused = -1;                                        \
    } while (0)

#define CREATE_LOOP_END(_place, _items, _first_item_disp, _size, _flags)       \
    do {                                                                       \
        (_place)->end_loop.common.type = OPAL_DATATYPE_END_LOOP;               \
        (_place)->end_loop.common.flags = (_flags) & ~OPAL_DATATYPE_FLAG_DATA; \
        (_place)->end_loop.items = (_items);                                   \
        (_place)->end_loop.first_elem_disp = (_first_item_disp);               \
        (_place)->end_loop.size = (_size); /* the size inside the loop */      \
        (_place)->end_loop.unused = -1;                                        \
    } while (0)

/**
 * Create an element entry in the description. If the element is contiguous
 * collapse everything into the blocklen.
 */
#define CREATE_ELEM(_place, _type, _flags, _blocklen, _count, _disp, _extent)                 \
    do {                                                                                      \
        (_place)->elem.common.flags = (_flags) | OPAL_DATATYPE_FLAG_DATA;                     \
        (_place)->elem.common.type = (_type);                                                 \
        (_place)->elem.blocklen = (_blocklen);                                                \
        (_place)->elem.count = (_count);                                                      \
        (_place)->elem.extent = (_extent);                                                    \
        (_place)->elem.disp = (_disp);                                                        \
        if (_extent == (ptrdiff_t) (_blocklen * opal_datatype_basicDatatypes[_type]->size)) { \
            /* collapse it into a single large blocklen */                                    \
            (_place)->elem.blocklen *= _count;                                                \
            (_place)->elem.extent *= _count;                                                  \
            (_place)->elem.count = 1;                                                         \
        }                                                                                     \
    } while (0)
/*
 * This array holds the descriptions desc.desc[2] of the predefined basic datatypes.
 */
OPAL_DECLSPEC extern union dt_elem_desc
    opal_datatype_predefined_elem_desc[2 * OPAL_DATATYPE_MAX_PREDEFINED];
struct opal_datatype_t;

/* Other fields starting after bdt_used (index of OPAL_DATATYPE_LOOP should be ONE) */
/*
 * NOTE: The order of initialization *MUST* match the order of the OPAL_DATATYPE_-numbers.
 * Unfortunately, I don't get the preprocessor to replace
 *     OPAL_DATATYPE_INIT_BTYPES_ARRAY_ ## OPAL_DATATYPE ## NAME
 * into
 *     OPAL_DATATYPE_INIT_BTYPES_ARRAY_[0-21], then order and naming would _not_ matter....
 */

#define OPAL_DATATYPE_INIT_PTYPES_ARRAY_UNAVAILABLE NULL
#define OPAL_DATATYPE_INIT_PTYPES_ARRAY(NAME)                              \
    (size_t[OPAL_DATATYPE_MAX_PREDEFINED])                                 \
    {                                                                      \
        [OPAL_DATATYPE_##NAME] = 1, [OPAL_DATATYPE_MAX_PREDEFINED - 1] = 0 \
    }


#define BASIC_DDT_FROM_ELEM(ELEM) (opal_datatype_basicDatatypes[(ELEM).elem.common.type])

#define SAVE_STACK(PSTACK, INDEX, TYPE, COUNT, DISP) \
    do {                                             \
        (PSTACK)->index = (INDEX);                   \
        (PSTACK)->type = (TYPE);                     \
        (PSTACK)->count = (COUNT);                   \
        (PSTACK)->disp = (DISP);                     \
    } while (0)

#define PUSH_STACK(PSTACK, STACK_POS, INDEX, TYPE, COUNT, DISP)   \
    do {                                                          \
        dt_stack_t *pTempStack = (PSTACK) + 1;                    \
        SAVE_STACK(pTempStack, (INDEX), (TYPE), (COUNT), (DISP)); \
        (STACK_POS)++;                                            \
        (PSTACK) = pTempStack;                                    \
    } while (0)

#if OPAL_ENABLE_DEBUG
#    define OPAL_DATATYPE_SAFEGUARD_POINTER(ACTPTR, LENGTH, INITPTR, PDATA, COUNT)                \
        {                                                                                         \
            unsigned char *__lower_bound = (INITPTR), *__upper_bound;                             \
            assert( (COUNT) != 0 );                                                               \
            __lower_bound += (PDATA)->true_lb;                                                    \
            __upper_bound = (INITPTR) + (PDATA)->true_ub +                                        \
                            ((PDATA)->ub - (PDATA)->lb) * ((COUNT) -1);                           \
            if (((ACTPTR) < __lower_bound) || ((ACTPTR) >= __upper_bound)) {                      \
                opal_datatype_safeguard_pointer_debug_breakpoint((ACTPTR), (LENGTH), (INITPTR),   \
                                                                 (PDATA), (COUNT));               \
                opal_output(0,                                                                    \
                            "%s:%d\n\tPointer %p size %lu is outside [%p,%p] for\n\tbase ptr %p " \
                            "count %lu and data \n",                                              \
                            __FILE__, __LINE__, (void *) (ACTPTR), (unsigned long) (LENGTH),      \
                            (void *) __lower_bound, (void *) __upper_bound, (void *) (INITPTR),   \
                            (unsigned long) (COUNT));                                             \
                opal_datatype_dump((PDATA));                                                      \
            }                                                                                     \
        }

#else
#    define OPAL_DATATYPE_SAFEGUARD_POINTER(ACTPTR, LENGTH, INITPTR, PDATA, COUNT)
#endif /* OPAL_ENABLE_DEBUG */

static inline int GET_FIRST_NON_LOOP(const union dt_elem_desc *_pElem)
{
    int element_index = 0;

    /* We dont have to check for the end as we always put an END_LOOP
     * at the end of all datatype descriptions.
     */
    while (_pElem->elem.common.type == OPAL_DATATYPE_LOOP) {
        ++_pElem;
        element_index++;
    }
    return element_index;
}

#define UPDATE_INTERNAL_COUNTERS(DESCRIPTION, POSITION, ELEMENT, COUNTER) \
    do {                                                                  \
        (ELEMENT) = &((DESCRIPTION)[(POSITION)]);                         \
        if (OPAL_DATATYPE_LOOP == (ELEMENT)->elem.common.type)            \
            (COUNTER) = (ELEMENT)->loop.loops;                            \
        else                                                              \
            (COUNTER) = (ELEMENT)->elem.count * (ELEMENT)->elem.blocklen; \
    } while (0)

OPAL_DECLSPEC int opal_datatype_contain_basic_datatypes(const struct opal_datatype_t *pData,
                                                        char *ptr, size_t length);
OPAL_DECLSPEC int opal_datatype_dump_data_flags(unsigned short usflags, char *ptr, size_t length);
OPAL_DECLSPEC int opal_datatype_dump_data_desc(union dt_elem_desc *pDesc, int nbElems, char *ptr,
                                               size_t length);

extern bool opal_ddt_position_debug;
extern bool opal_ddt_copy_debug;
extern bool opal_ddt_unpack_debug;
extern bool opal_ddt_pack_debug;
extern bool opal_ddt_raw_debug;

END_C_DECLS
#endif /* OPAL_DATATYPE_INTERNAL_H_HAS_BEEN_INCLUDED */
