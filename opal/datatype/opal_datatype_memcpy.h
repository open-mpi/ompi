/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (C) 2019      Arm Ltd.  ALL RIGHTS RESERVED.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED

#ifdef __ARM_FEATURE_SVE
#include <arm_sve.h>
#endif /* __ARM_FEATURE_SVE */

#include <stdio.h>
/*
 * This macro is called whenever we are packing/unpacking a DDT that
 * that is built with basic datatypes.
 * Specifying a fixed size for the memcpy() makes the intel compiler
 * inline it as an assignment operation.
 * This code is a bit hacky, but doing this we can divide the latency
 * by up to 2 during DDT exechanges.
 */
#define MEMCPY( DST, SRC, BLENGTH )                                 \
    do {                                                            \
        if( (BLENGTH) < 16 ) {                                      \
            uintptr_t align = ((uintptr_t)(DST)) ^ ((uintptr_t)(SRC));                        \
            if( (4 == (BLENGTH)) && (0 == (align & 0x3)) ) {  /* We are copying an int */     \
                *(int*)(DST) = *(int*)(SRC);                        \
            } else if( (8 == (BLENGTH)) && (0 == (align & 0x7)) ) {   /* We are copying a double */   \
                *(double*)(DST) = *(double*)(SRC);                  \
            } else if( (16 == (BLENGTH)) && (0 == (align & 0xF)) ) {   /* We are copying a long double */   \
                *(long double*)(DST) = *(long double*)(SRC);        \
            } else {                                                \
                memcpy((DST), (SRC), (BLENGTH));                    \
            }                                                       \
        } else {                                                    \
            /* min value when length > 4* VL */                     \
            if( (BLENGTH) >= svcntb() * 4 ) {                       \
                svbool_t Pg = svptrue_b8();                         \
                uint64_t i;                                         \
                for(i=0; i<BLENGTH; i+=svcntb() * 4)                \
                {   /*printf("H%d,%d ", BLENGTH, i); */             \
                    svuint8x4_t  vsrc = svld4(Pg, (uint8_t*)(SRC + i) );           \
                    svst4(Pg, (uint8_t*)(DST + i), vsrc);               \
                }                                                       \
                Pg = svwhilelt_b8_u64(i, BLENGTH);                 \
                svuint8x4_t  vsrc = svld4(Pg, (uint8_t*)(SRC+i));  \
                svst4(Pg, (uint8_t*)(DST+i), vsrc);                \
            }                                                           \
            else { memcpy((DST), (SRC), (BLENGTH)); }                   \
        }                                                               \
    } while (0)
#endif  /* OPAL_DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED */
