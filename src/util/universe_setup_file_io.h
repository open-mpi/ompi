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

OMPI_DECLSPEC int ompi_write_universe_setup_file(char *filename);

OMPI_DECLSPEC int ompi_read_universe_setup_file(char *filename);

#endif
