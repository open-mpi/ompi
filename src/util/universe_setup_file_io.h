/*
 * 
 * $HEADER$
 *
 * $Id: ompi_universe_setup_file I/O functions $
 * 
 */

#ifndef OMPI_UNIV_SETUP_FILE_IO_H
#define OMPI_UNIV_SETUP_FILE_IO_H

#include "ompi_config.h"

#include "tools/openmpi/openmpi.h"

int ompi_write_universe_setup_file(char *filename, ompi_universe_t *universe);

int ompi_read_universe_setup_file(char *filename, ompi_universe_t *universe);

#endif
