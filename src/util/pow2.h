/*
 * $HEADER$
 */

#ifndef OMPI_POW2_H
#define OMPI_POW2_H
	
/**
 *  This routine takes in an interger, and returns the nearest
 *  power of 2 integer, same or greater than the input.
 *
 *  @param input_integer input value
 *
 *  @returnvalue power of 2 integer same or greater than the input
 *               parameter.
 */
int ompi_round_up_to_nearest_pow2(int input_integer);

#endif /* OMPI_POW2_H */
