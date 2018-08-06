/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2018      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Sandia National Laboratories.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PML_OB1_CUSTOM_MATCH_H
#define PML_OB1_CUSTOM_MATCH_H

#include "ompi_config.h"
#include "ompi/mca/pml/ob1/pml_ob1.h"

#define CUSTOM_MATCH_DEBUG         1
#define CUSTOM_MATCH_DEBUG_VERBOSE 1

/**
 * Custom match types
 */
#define MCA_PML_OB1_CUSTOM_MATCHING_NONE        0
#define MCA_PML_OB1_CUSTOM_MATCHING_LINKEDLIST  1
#define MCA_PML_OB1_CUSTOM_MATCHING_ARRAYS      2
#define MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_BYTE  3
#define MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_SHORT 4
#define MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_WORD  5
#define MCA_PML_OB1_CUSTOM_MATCHING_VECTOR      6

#if MCA_PML_OB1_CUSTOM_MATCHING != MCA_PML_OB1_CUSTOM_MATCHING_NONE

#define MCA_PML_OB1_CUSTOM_MATCH 1

#if MCA_PML_OB1_CUSTOM_MATCHING == MCA_PML_OB1_CUSTOM_MATCHING_LINKEDLIST
#include "pml_ob1_custom_match_linkedlist.h"
#elif MCA_PML_OB1_CUSTOM_MATCHING == MCA_PML_OB1_CUSTOM_MATCHING_ARRAYS
#include "pml_ob1_custom_match_arrays.h"
#elif MCA_PML_OB1_CUSTOM_MATCHING == MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_BYTE
#include "pml_ob1_custom_match_fuzzy512-byte.h"
#elif MCA_PML_OB1_CUSTOM_MATCHING == MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_SHORT
#include "pml_ob1_custom_match_fuzzy512-short.h"
#elif MCA_PML_OB1_CUSTOM_MATCHING == MCA_PML_OB1_CUSTOM_MATCHING_FUZZY_WORD
#include "pml_ob1_custom_match_fuzzy512-word.h"
#elif MCA_PML_OB1_CUSTOM_MATCHING == MCA_PML_OB1_CUSTOM_MATCHING_VECTOR
#include "pml_ob1_custom_match_vectors.h"
#endif

#else

#define MCA_PML_OB1_CUSTOM_MATCH 0

#endif

#endif
