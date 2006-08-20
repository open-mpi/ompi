/*
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * $Id: ompi_universe_setup_file I/O functions $
 * 
 */

#ifndef ORTE_UNIV_SETUP_FILE_IO_H
#define ORTE_UNIV_SETUP_FILE_IO_H

#include "orte_config.h"
#include "orte/util/univ_info.h"

ORTE_DECLSPEC int orte_write_universe_setup_file(char *filename, orte_universe_t *info);

ORTE_DECLSPEC int orte_read_universe_setup_file(char *filename, orte_universe_t *info);

#endif
