/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 */
#ifndef MCA_PTL_ADDRESS_H
#define MCA_PTL_ADDRESS_H


/*
 * macro to convert virtual address, to address relative to a base
 * offset
 */
#define  RELATIVE_ADDRESS(A,B)  (void *) ( (char *)(A) -  \
        (size_t)(B) )


#endif /* !ADDRESS */

