/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif


#include "util/sys_info.h"
#include "support.h"
#include "../src/util/bufpack.h"
#include "../src/include/constants.h"

/* used for debugging */
/* int dump_buf (ompi_buffer_t buf); */

ompi_buffer_t bufA;
ompi_buffer_t bufB;
ompi_buffer_t bufC;


static bool test1(void);        /* verify different buffer inits */
static bool test2(void);        /* verify we can pack ok */
static bool test3(void);          /* verify we can pack expanding buf */
static bool test4(void);        /* verify pack a packed buffer */
static bool test5(void);        /* verify unpack */
static bool test6(void);        /* verify free */
static bool test7(void);        /* verify preallocated buffer init, pack and unpack */
static bool test8(void);        /* verify string pack and unpack */

int main (int argc, char* argv[])
{

    test_init("ompi_pack");

    if (test1()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test1 failed");
    }

    if (test2()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test2 failed");
    }

    if (test3()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test3 failed");
    }

    if (test4()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test4 failed");
    }

    if (test5()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test5 failed");
    }

    if (test6()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test6 failed");
    }

    if (test7()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test7 failed");
    }

    if (test8()) {
        test_success();
    }
    else {
      test_failure("ompi_pack test8 failed");
    }


/*     if (testN()) { */
/*         test_success(); */
/*     } */
/*     else { */
/*       test_failure("ompi_pack testN failed"); */
/*     } */


    test_finalize();
    return (0);
}

static bool test1(void)        /* verify different buffer inits */
{
    int rc;

    rc = ompi_buffer_init (&bufA, 0);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_init failed"); return(false);}

    rc = ompi_buffer_init (&bufB, 16);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_init failed"); return(false);}

    rc = ompi_buffer_init (&bufC, 1024);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_init failed"); return(false);}

    return (true);

    
}

/* BufA should hold 1024 INT32s */
static bool test2(void)        /* verify we can pack ok */
{
    int rc;
    int i;

    for (i=0;i<1024;i++) {
        rc = ompi_pack (bufA, &i, 1, OMPI_INT32);
        if (OMPI_ERROR==rc) { test_comment ("ompi_pack failed"); return(false);}
    }

    return (true);
}

    
/* BufB was init with 16 bytes not 1024 sizeof(INT32)s */
/* so it should expand and keep packing */
static bool test3(void)          /* verify we can pack expanding buf */
{
    int rc;
    int i;
    int j;

    for (i=0;i<1024;i++) {
        j = i * 2;  /* so we can verify */
        rc = ompi_pack (bufB, &j, 1, OMPI_INT32);
        if (OMPI_ERROR==rc) { test_comment ("ompi_pack failed"); return(false);}
    }

    return (true);
}

static bool test4(void)        /* verify pack a packed buffer */
{
    int rc;

    rc = ompi_pack (bufC, bufA, 1, OMPI_PACKED);
    if (OMPI_ERROR==rc) { test_comment ("ompi_pack failed"); return(false);}
    rc = ompi_pack (bufC, bufB, 1, OMPI_PACKED);
    if (OMPI_ERROR==rc) { test_comment ("ompi_pack failed"); return(false);}

    return (true);
}

static bool test5(void)        /* verify unpack */
{
    int rc;
    int i, j;
    int out;

    for (i=0;i<1024;i++) {
        j = i; /* for bufA */
        rc = ompi_unpack (bufA, &out, 1, OMPI_INT32);
        if (OMPI_ERROR==rc) { test_comment ("ompi_unpack failed"); return(false);}

        if (out!=j) { test_comment ("bufA packed != unpacked data"); return(false);}
    }

    for (i=0;i<1024;i++) {
        j = i*2; /* for bufB */
        rc = ompi_unpack (bufB, &out, 1, OMPI_INT32);
        if (OMPI_ERROR==rc) { test_comment ("ompi_unpack failed"); return(false);}

        if (out!=j) { test_comment ("bufB packed != unpacked data"); return(false);}
    }

    for (i=0;i<2048;i++) {
    
        if (i<1024) { j = i; /* bufAs data 1st half */ }
        else { j = (i-1024)*2; /* bufBs data 2nd half */ }

        rc = ompi_unpack (bufC, &out, 1, OMPI_INT32);
        if (OMPI_ERROR==rc) { test_comment ("ompi_unpack failed"); return(false);}

        if (out!=j) { 
            test_comment ("bufC packed != unpacked data"); 
            printf("iteration %d expected %d have %d\n", i, j, out);
            return(false);
        }
    }




    return (true);
}

static bool test6(void)        /* verify free */
{
    int rc;

    rc = ompi_buffer_free (bufA);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_free failed"); return(false);}
    rc = ompi_buffer_free (bufB);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_free failed"); return(false);}
    rc = ompi_buffer_free (bufC);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_free failed"); return(false);}

    return (true);
}

static bool test7(void)        /* verify preallocated buffer init, pack and unpack */
{

    int rc;
    int i, j, out;
    char *mybuf;
    int *p;


    /* we cannot use heap/static memory for a buffer as it cannot be realloced as needed for this test */
    mybuf = (char*) malloc (sizeof(int)*5);
    p = (int*) mybuf;
    for(i=0;i<5;i++) {
        *p++ = htonl (i);    /* the data must be in network byte order for this test to be valid */
                                /* as the test is emulating prepacked data recvd from the network (OOB) */
    }


    rc = ompi_buffer_init_preallocated (&bufA, mybuf, sizeof(int)*5);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_init_preallocated failed"); return(false);}

    i = 5;
    rc = ompi_pack (bufA, &i, 1, OMPI_INT32);
    if (OMPI_ERROR==rc) { test_comment ("ompi_pack failed"); return(false);}

    /* ok, now check its contents */
    for (i=0;i<6;i++) {
        j = i; /* for bufA */
        rc = ompi_unpack (bufA, &out, 1, OMPI_INT32);
        if (OMPI_ERROR==rc) { test_comment ("ompi_unpack failed"); return(false);}

        if (out!=j) { 
            test_comment ("bufA packed != unpacked data"); 
            printf("iteration %d expected %d have %d\n", i, j, out);
            return(false);
        }
    }

    
    /* I do not free mybuf as ompi_buffer_free() will */    

    rc = ompi_buffer_free (bufA);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_free failed"); return(false);}

    return (true);
}


static bool test8(void)        /* verify string pack and unpack */
{

    int rc;
	char *str1;
	char *str2;

    rc = ompi_buffer_init (&bufA, 0);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_init failed"); return(false);}

    rc = ompi_buffer_init (&bufB, 10);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_init failed"); return(false);}

    rc = ompi_buffer_init (&bufC, 16);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_init failed"); return(false);}

    rc = ompi_pack_string (bufA, "HELLO ");
    if (OMPI_ERROR==rc) { test_comment ("ompi_pack_string failed"); return(false);}

    rc = ompi_pack_string (bufB, "WORLD!");
    if (OMPI_ERROR==rc) { test_comment ("ompi_pack_string failed"); return(false);}

    rc = ompi_pack (bufC, bufA, 1, OMPI_PACKED);
    if (OMPI_ERROR==rc) { test_comment ("ompi_pack failed"); return(false);}
    rc = ompi_pack (bufC, bufB, 1, OMPI_PACKED);
    if (OMPI_ERROR==rc) { test_comment ("ompi_pack failed"); return(false);}

	/* we now have a buffer with two strings in it */
    rc = ompi_unpack_string (bufC, &str1);
    if (OMPI_ERROR==rc) { test_comment ("ompi_unpack_string failed"); return(false);}

	rc = strcmp ("HELLO ", str1);
	if (rc) { test_comment ("strcmp returns no zero value."); return (false); }

    rc = ompi_unpack_string (bufC, &str2);
    if (OMPI_ERROR==rc) { test_comment ("ompi_unpack_string failed"); return(false);}

	rc = strcmp ("WORLD!", str2);
	if (rc) { test_comment ("strcmp returns no zero value."); return (false); }


    rc = ompi_buffer_free (bufA);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_free failed"); return(false);}

    rc = ompi_buffer_free (bufB);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_free failed"); return(false);}

    rc = ompi_buffer_free (bufC);
    if (OMPI_ERROR==rc) { test_comment ("ompi_buffer_free failed"); return(false);}

	if (str1) { free (str1); }
	if (str2) { free (str2); }

    return (true);
}

/* int dump_buf (ompi_buffer_t buf) */
/* { */
/* int rc, i, out; */
/*  */
/* rc = 0; */
/* i = 0; */
/* while (1) { */
/*     rc = ompi_unpack (buf, &out, 1, OMPI_INT32); */
/*     if (rc==0) printf("%d[%d] ", i, out); */
/*     else { */
/*         printf("\n"); */
/*         break; */
/*     } */
/*     i++; */
/* } */
/*  */
/* return (i); */
/* } */

