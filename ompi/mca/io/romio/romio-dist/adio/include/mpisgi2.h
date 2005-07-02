/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (C) 1997, Silicon Graphics, Inc.
 * All Rights Reserved
 */

#if (!defined(HAVE_MPI_COMBINERS) && !defined(MPI_COMBINER_NAMED))

#define MPI_COMBINER_NAMED	(-1)
#define MPI_COMBINER_CONTIGUOUS	0
#define MPI_COMBINER_VECTOR	1
#define MPI_COMBINER_HVECTOR	2
#define MPI_COMBINER_INDEXED	3
#define MPI_COMBINER_HINDEXED	4
#define MPI_COMBINER_STRUCT	5

#endif
