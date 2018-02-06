/*
 * Copyright © 2009 CNRS
 * Copyright © 2009 inria.  All rights reserved.
 * Copyright © 2009 Université Bordeaux
 * Copyright © 2009 Cisco Systems, Inc.  All rights reserved.
 * See COPYING in top-level directory.
 */

#include <hwloc.h>
#include <stdio.h>

extern int do_test(void);


int main(int argc, char *argv[])
{
    /* Make the test be in a separate library that will fail to link
       properly if hwloc forces compilation with visibility
       enabled. */
    return do_test();
}
