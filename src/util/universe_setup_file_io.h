/*
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

OMPI_DECLSPEC int orte_write_universe_setup_file(char *filename);

OMPI_DECLSPEC int orte_read_universe_setup_file(char *filename);

#endif
