/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPIU_EXTERNAL32_H_INCLUDED
#define MPIU_EXTERNAL32_H_INCLUDED

int MPIU_write_external32_conversion_fn(const void *userbuf, MPI_Datatype datatype,
                                        int count, void *filebuf);
int MPIU_read_external32_conversion_fn(void *userbuf, MPI_Datatype datatype,
                                       int count, void *filebuf);
int MPIU_datatype_full_size(MPI_Datatype datatype, MPI_Aint * size);

/* given a buffer, count, and datatype, return an apropriately sized and
 *  * external32-formatted buffer, suitable for handing off to a subsequent write
 *   * routine */
int MPIU_external32_buffer_setup(const void *buf, int count, MPI_Datatype type, void **newbuf);

#endif /* MPIU_EXTERNAL32_H_INCLUDED */
