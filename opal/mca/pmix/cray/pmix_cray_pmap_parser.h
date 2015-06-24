/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#ifndef PMIX_CRAY_PMAP_PARSER_H
#define PMIX_CRAY_PMAP_PARSER_H

int *pmix_cray_parse_pmap(char *pmap, int my_rank,
                          int *node, int *nlrs);
#endif
