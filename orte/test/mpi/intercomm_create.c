#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>

#include <mpi.h>

#define NB_SPAWN 4

static void do_parent(char *argv[], int rank, int count);
static void do_target(char* argv[], MPI_Comm parent);

static char *cmd_argv1 = "b";
static char *cmd_argv2 = "c";
static char *whoami = "a";
static int tag = 201;

void ompitest_warning( char* filename, int lineno, const char* fmt, ... )
{
    char* buf = NULL;
    va_list va_list;

    va_start(va_list, fmt);
    vasprintf( &buf, fmt, va_list );
    va_end(va_list);
    printf( "*warning* %s:%d %s\n", filename, lineno, buf );
    free(buf);
}

void ompitest_error( char* filename, int lineno, const char* fmt, ... )
{
    char* buf = NULL;
    va_list va_list;

    va_start(va_list, fmt);
    vasprintf( &buf, fmt, va_list );
    va_end(va_list);
    printf( "*error* %s:%d %s\n", filename, lineno, buf );
    free(buf);
}

int 
main(int argc, char *argv[])
{
    int rank, size;
    MPI_Comm parent;
 
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* Check to see if we *were* spawned -- because this is a test, we
       can only assume the existence of this one executable.  Hence, we
       both mpirun it and spawn it. */

    parent = MPI_COMM_NULL;
    MPI_Comm_get_parent(&parent);
    if (parent != MPI_COMM_NULL) {
        whoami = argv[1];
        do_target(argv, parent);
    } else {
        do_parent(argv, rank, size);
    }

    /* All done */

    MPI_Finalize();
    return 0;
}

static int
spawn_and_merge( char* argv[], char* arg, int count,
                 MPI_Comm* inter, MPI_Comm* intra )
{
    int *errcode, err, i;
    char *spawn_argv[2];

    errcode = malloc(sizeof(int) * count);
    if (errcode == NULL)
        ompitest_error(__FILE__, __LINE__, "Doh!  Rank %d was not able to allocate enough memory.  MPI test aborted!\n", 0);
    memset(errcode, -1, count);
    /*MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);*/

    spawn_argv[0] = arg;
    spawn_argv[1] = NULL;
    err = MPI_Comm_spawn(argv[0], spawn_argv, count, MPI_INFO_NULL, 0,
                         MPI_COMM_WORLD, inter, errcode); 
    for (i = 0; i < count; i++)
        if (errcode[i] != MPI_SUCCESS)
            ompitest_error(__FILE__, __LINE__, 
                           "ERROR: MPI_Comm_spawn returned errcode[%d] = %d\n", 
                           i, errcode[i]);
    if (err != MPI_SUCCESS)
        ompitest_error(__FILE__, __LINE__, 
                       "ERROR: MPI_Comm_spawn returned errcode = %d\n", err);
    err = MPI_Intercomm_merge( *inter, 0, intra );
    free(errcode);
    return err;
}

static void
do_parent(char *argv[], int rank, int count)
{
    MPI_Comm ab_inter, ab_intra, ac_inter, ac_intra, ab_c_inter, abc_intra;
    int err;

    err = spawn_and_merge( argv, cmd_argv1, count, &ab_inter, &ab_intra );
    err = spawn_and_merge( argv, cmd_argv2, count, &ac_inter, &ac_intra );
    
    printf( "%s: MPI_Intercomm_create( ab_intra, 0, ac_intra, %d, %d, &inter) (%d)\n",
            whoami, count, tag, err );
    err = MPI_Intercomm_create( ab_intra, 0, ac_intra, count, tag, &ab_c_inter );
    printf( "%s: intercomm_create (%d)\n", whoami, err );

    printf( "%s: barrier on inter-comm - before\n", whoami );
    err = MPI_Barrier(ab_c_inter);
    printf( "%s: barrier on inter-comm - after\n", whoami );

    err = MPI_Intercomm_merge(ab_c_inter, 0, &abc_intra);
    printf( "%s: intercomm_merge(%d) (%d) [rank %d]\n", whoami, 0, err, rank );
    err = MPI_Barrier(abc_intra);
    printf( "%s: barrier (%d)\n", whoami, err );

    MPI_Comm_disconnect(&ab_inter);
    MPI_Comm_disconnect(&ac_inter);
}


static void
do_target(char* argv[], MPI_Comm parent)
{
    int rank, first = 0, err;
    MPI_Comm intra, inter, merge1;

    if( 0 == strcmp(argv[1], cmd_argv1) ) first = 1;

    /*MPI_Comm_set_errhandler(parent, MPI_ERRORS_RETURN);*/

    err = MPI_Intercomm_merge( parent, 1, &intra );
    MPI_Comm_rank(intra, &rank);

    if( first ) {
        printf( "%s: MPI_Intercomm_create( intra, 0, intra, MPI_COMM_NULL, %d, &inter) [rank %d]\n", whoami, tag, rank );
        err = MPI_Intercomm_create( intra, 0, MPI_COMM_NULL, 0, tag, &inter);
        printf( "%s: intercomm_create (%d)\n", whoami, err );
    } else {
        printf( "%s: MPI_Intercomm_create( MPI_COMM_WORLD, 0, intra, 0, %d, &inter) [rank %d]\n", whoami, tag, rank );
        err = MPI_Intercomm_create( MPI_COMM_WORLD, 0, intra, 0, tag, &inter);
        printf( "%s: intercomm_create (%d)\n", whoami, err );
    }
    printf( "%s: barrier on inter-comm - before\n", whoami );
    err = MPI_Barrier(inter);
    printf( "%s: barrier on inter-comm - after\n", whoami );

    err = MPI_Intercomm_merge( inter, 0, &merge1 );
    MPI_Comm_rank(merge1, &rank);
    printf( "%s: intercomm_merge(%d) (%d) [rank %d]\n", whoami, first, err, rank );
    err = MPI_Barrier(merge1);
    printf( "%s: barrier (%d)\n", whoami, err );

    MPI_Comm_free(&merge1);
    MPI_Comm_free(&inter);
    MPI_Comm_free(&intra);

    MPI_Comm_disconnect(&parent);
}

