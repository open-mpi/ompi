/* ---------------------------------------------------------------- */
/* (C)Copyright IBM Corp.  2007, 2008                               */
/* ---------------------------------------------------------------- */
/**
 * \file ad_gpfs_aggrs.h
 * \brief ???
 */

/*
 * File: ad_gpfs_aggrs.h
 *
 * Declares functions optimized specifically for GPFS parallel I/O solution.
 *
 */

#ifndef AD_GPFS_AGGRS_H_
#define AD_GPFS_AGGRS_H_

#include "adio.h"
#include <sys/stat.h>

#ifdef HAVE_GPFS_H
#include <gpfs.h>
#endif


    /* overriding ADIOI_Calc_file_domains() to apply 'aligned file domain partitioning'. */
    void ADIOI_GPFS_Calc_file_domains(ADIO_File fd,
	                                  ADIO_Offset *st_offsets,
				          ADIO_Offset *end_offsets,
				          int          nprocs,
				          int          nprocs_for_coll,
				          ADIO_Offset *min_st_offset_ptr,
				          ADIO_Offset **fd_start_ptr,
				          ADIO_Offset **fd_end_ptr,
				          ADIO_Offset *fd_size_ptr,
                  void        *fs_ptr);

    /* overriding ADIOI_Calc_aggregator() for the default implementation is specific for
       static file domain partitioning */
    int ADIOI_GPFS_Calc_aggregator(ADIO_File fd,
				  ADIO_Offset off,
				  ADIO_Offset min_off,
				  ADIO_Offset *len,
				  ADIO_Offset fd_size,
				  ADIO_Offset *fd_start,
				  ADIO_Offset *fd_end);

    /* overriding ADIOI_Calc_my_req for the default implementation is specific for
       static file domain partitioning */
    void ADIOI_GPFS_Calc_my_req ( ADIO_File fd, ADIO_Offset *offset_list, ADIO_Offset *len_list,
				 int contig_access_count, ADIO_Offset
				 min_st_offset, ADIO_Offset *fd_start,
				 ADIO_Offset *fd_end, ADIO_Offset fd_size,
				 int nprocs,
				 int *count_my_req_procs_ptr,
				 int **count_my_req_per_proc_ptr,
				 ADIOI_Access **my_req_ptr,
				 int **buf_idx_ptr);

    /*
     * ADIOI_Calc_others_req
     *
     * param[in]  count_my_req_procs        Number of processes whose file domain my
     *                                        request touches.
     * param[in]  count_my_req_per_proc     count_my_req_per_proc[i] gives the no. of
     *                                        contig. requests of this process in
     *                                        process i's file domain.
     * param[in]  my_req                    A structure defining my request
     * param[in]  nprocs                    Number of nodes in the block
     * param[in]  myrank                    Rank of this node
     * param[out] count_others_req_proc_ptr Number of processes whose requests lie in
     *                                        my process's file domain (including my
     *                                        process itself)
     * param[out] others_req_ptr            Array of other process' requests that lie
     *                                        in my process's file domain
     */
     void ADIOI_GPFS_Calc_others_req(ADIO_File fd, int count_my_req_procs,
				    int *count_my_req_per_proc,
				    ADIOI_Access *my_req,
				    int nprocs, int myrank,
				    int *count_others_req_procs_ptr,
				    ADIOI_Access **others_req_ptr);


#endif  /* AD_GPFS_AGGRS_H_ */
