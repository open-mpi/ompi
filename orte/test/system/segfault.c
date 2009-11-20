/* -*- C -*-
 *
 * $HEADER$
 *
 * A program that just segfaults
 */

#include <stdio.h>

int main(int argc, char* argv[])
{

    double pi;
    char *dum=NULL;

    pi = (double)*dum;
    
}
