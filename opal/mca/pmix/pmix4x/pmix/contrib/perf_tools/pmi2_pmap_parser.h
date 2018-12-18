/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

/* This code was taken from Open MPI project, file
   opal/mca/pmix/s2/pmi2_pmap_parser.h
*/


#ifndef PMI2_PMAP_PARSER_H
#define PMI2_PMAP_PARSER_H

int *mca_common_pmi2_parse_pmap(char *pmap, int my_rank,
                                  int *node, int *nlrs);
#endif
