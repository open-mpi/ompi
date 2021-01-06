/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPIU_GREQ_H_INCLUDED
#define MPIU_GREQ_H_INCLUDED

int MPIU_Greq_query_fn(void *extra_state, MPI_Status * status);
int MPIU_Greq_free_fn(void *extra_state);
int MPIU_Greq_cancel_fn(void *extra_state, int complete);

#endif /* MPIU_GREQ_H_INCLUDED */
