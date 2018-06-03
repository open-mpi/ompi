/*
 * Copyright © 2016 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#ifndef _NETLOCSCOTCH_H_
#define _NETLOCSCOTCH_H_

#ifndef _GNU_SOURCE
#define _GNU_SOURCE // for asprintf
#endif

#include <hwloc/autogen/config.h>
#include <netloc.h>

/* Includes for Scotch */
#include <stdio.h>
#include <scotch.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * A structure to represent process mapping
 */
typedef struct {
    int rank; /**< Rank of the process */
    char *nodename; /**< Name of the node */
    int core; /**< Physical slot number of the core */
} netlocscotch_core_t;

/**
 * \brief Build the Scotch architecture representing the all machine
 *
 * \param arch Pointer to the Scotch arch that will be built.
 *
 * \returns 0 on success
 * \returns NETLOC_ERROR on error
 */
int netlocscotch_build_global_arch(SCOTCH_Arch *arch);

/**
 * \brief Build the Scotch architecture representing the available resources
 *
 * This function reads the file about available resources, found by reading the
 * environment variable NETLOC_CURRENTSLOTS. The file must be generated before
 * calling the program running this functions with: mpirun -np <nprocs>
 * netloc_mpi_find_hosts <outputfile>
 * The complete architecture is needed since the sub architecture use data from it.
 *
 * \param arch Pointer to the Scotch arch that will be built.
 * \param subarch Pointer to the Scotch sub arch that will be built.
 *
 * \returns 0 on success
 * \returns NETLOC_ERROR on error
 */
int netlocscotch_build_current_arch(SCOTCH_Arch *arch, SCOTCH_Arch *subarch);

/**
 * \brief Give a good mapping with Scotch from a file containing a
 * communication matrix
 *
 * This function reads the file about available resources, found by reading the
 * environment variable NETLOC_CURRENTSLOTS. The file must be generated before
 * calling the program running this functions with: mpirun -np <nprocs>
 * netloc_mpi_find_hosts <outputfile>
 *
 * An application graph is built from the communication matrix and is mapped to
 * the architecture graph built from the resource file.
 *
 * \param[in] filename Filename of the matrix file, where the matrix is stored line
 * by line with spaces between values.
 *
 * \param[out] pnum_processes Pointer to the integer where th number of processes
 * will be written.
 *
 * \param[out] pcores Array of pnum_processes elements.
 *
 * \returns 0 on succes 
 * \returns NETLOC_ERROR on error
 */
int netlocscotch_get_mapping_from_comm_file(char *filename, int *pnum_processes,
        netlocscotch_core_t **pcores);

/**
 * \brief Give a good mapping with Scotch from a communication matrix
 *
 * This function reads the file about available resources, found by reading the
 * environment variable NETLOC_CURRENTSLOTS. The file must be generated before
 * calling the program running this functions with: mpirun -np <nprocs>
 * netloc_mpi_find_hosts <outputfile>
 *
 * An application graph is built from the communication matrix and is mapped to
 * the architecture graph built from the resource file.
 *
 * \param[in] comm pointer to the lines of the matrix of communications.
 *
 * \param[in] num_vertices number of processes, that corresponds to the size of
 * the matrix.
 *
 * \param[out] pcores Array of num_vertices elements.
 *
 * \returns 0 on success
 * \returns NETLOC_ERROR on error
 */
int netlocscotch_get_mapping_from_comm_matrix(double **comm, int num_vertices,
        netlocscotch_core_t **pcores);

#ifdef __cplusplus
} /* extern "C" */
#endif

/** @} */

#endif // _NETLOC_H_
