/*
 * Copyright (c) 2006 The Trustees of Indiana University and Indiana
 *                    University Research and Technology
 *                    Corporation.  All rights reserved.
 * Copyright (c) 2006 The Technical University of Chemnitz. All 
 *                    rights reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 */
#ifndef __NBC_H__
#define __NBC_H__

#ifdef __cplusplus
extern "C" {
#endif


/*******************************************************
 ****** external NBC functions are defined here *******
 *******************************************************/

/* TODO: some hacks */
int NBC_Operation(void *buf3, void *buf1, void *buf2, MPI_Op op, MPI_Datatype type, int count);

void NBC_Reset_times(void);
void NBC_Print_times(double div);


#ifdef __cplusplus
}
#endif

#endif
