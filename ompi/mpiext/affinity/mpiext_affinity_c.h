/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#define OMPI_AFFINITY_STRING_MAX 1024

OMPI_DECLSPEC int OMPI_Affinity_str(char ompi_bound[OMPI_AFFINITY_STRING_MAX],
                                    char current_binding[OMPI_AFFINITY_STRING_MAX],
                                    char exists[OMPI_AFFINITY_STRING_MAX]);

