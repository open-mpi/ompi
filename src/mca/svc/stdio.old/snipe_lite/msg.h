/*
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
 */

/*
	HARNESS G_HCORE

	Innovative Computer Laboratory,
	University of Tennessee,
	Knoxville, TN, USA.

	harness@cs.utk.edu

 --------------------------------------------------------------------------

 Authors:	
			Graham E Fagg <fagg@cs.utk.edu>

 --------------------------------------------------------------------------

                              NOTICE

 Permission to use, copy, modify, and distribute this software and
 its documentation for any purpose and without fee is hereby granted
 provided that the above copyright notice appear in all copies and
 that both the copyright notice and this permission notice appear in
 supporting documentation.

 Neither the University of Tennessee nor the Authors make any
 representations about the suitability of this software for any
 purpose.  This software is provided ``as is'' without express or
 implied warranty.

 HARNESS, HARNESS G_HCORE and  FT_MPI was funded in part by the 
 U.S. Department of Energy.

*/

#ifndef _GHCORE_MSG_H
#define _GHCORE_MSG_H  1


#define EMPTYMSGBUF -1812
#define MSG_BAD_HEADER	-1111

#define	PKMESSAGEVER 0xABCD


int send_pkmesg (int send_s,int  myid,int  ntag,int * tagp,int  buf_id,int  free_buf);
int recv_pkmesg (int recv_s,int * from,int * ntag,int * tagp,int * buf_id);

#endif /* _GHCORE_MSG_H */
