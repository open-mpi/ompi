/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED

#define MEMCPY( DST, SRC, BLENGTH ) \
    memcpy( (DST), (SRC), (BLENGTH) )

/*
 * This macro is called whenever we are packing/unpacking a DDT that
 * that is built with basic datatypes.
 * Specifying a fixed size for the memcpy() makes the intel compiler
 * inline it as an assignment operation.
 * This code is a bit hacky, but doing this we can divide the latency
 * by up to 2 during DDT exechanges.
 */
#define BASIC_DTT_MEMCPY( DST, SRC, BLENGTH )                          \
    do {                                                               \
        if (4 == (BLENGTH)) {          /* We are copying an int */     \
            memcpy((DST), (SRC), 4);                                   \
        } else if (8 == (BLENGTH)) {   /* We are copying a double */   \
            memcpy((DST), (SRC), 8);                                   \
        } else {                                                       \
            memcpy((DST), (SRC), (BLENGTH));                           \
        }                                                              \
    } while (0)

#endif  /* OPAL_DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED */
