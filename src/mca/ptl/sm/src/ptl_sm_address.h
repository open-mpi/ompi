/*
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

