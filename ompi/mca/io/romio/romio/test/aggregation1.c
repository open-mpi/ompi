/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*  
 *  (C) 2007 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* Test case from John Bent (ROMIO req #835)
 * Aggregation code was not handling certain access patterns when collective
 * buffering forced */
#include <unistd.h>
#include <stdlib.h>
#include <mpi.h>
#include <stdio.h>
#include <string.h>

#define NUM_OBJS 4
#define OBJ_SIZE 1048576 

extern char *optarg;
extern int optind, opterr, optopt;


char *prog = NULL;
int  debug = 0;

static void
Usage( int line ) {
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if ( rank == 0 ) {
        fprintf( stderr, 
                "Usage (line %d): %s [-d] [-h] -f filename\n"
                "\t-d for debugging\n"
                "\t-h to turn on the hints to force collective aggregation\n",
                line, prog );
    }
    exit( 0 );
}

static void
fatal_error( int mpi_ret, MPI_Status *mpi_stat, char *msg ) {
    fprintf( stderr, "Fatal error %s: %d\n", msg, mpi_ret );
    MPI_Abort( MPI_COMM_WORLD, -1 );
}

static void
print_hints( int rank, MPI_File *mfh ) {
    MPI_Info info;
    int nkeys;
    int i, dummy_int;
    char key[1024];
    char value[1024];

    MPI_Barrier( MPI_COMM_WORLD );
    if ( rank == 0 ) {
        MPI_File_get_info( *mfh, &info );
        MPI_Info_get_nkeys( info, &nkeys );

        printf( "HINTS:\n" );
        for( i = 0; i < nkeys; i++ ) {
            MPI_Info_get_nthkey( info, i, key );
            printf( "%35s -> ", key );
            MPI_Info_get( info, key, 1024, value, &dummy_int ); 
            printf( "%s\n", value );
        }
	MPI_Info_free(&info);
    }
    MPI_Barrier( MPI_COMM_WORLD );
}

static void
fill_buffer( char *buffer, int bufsize, int rank, MPI_Offset offset ) {
    memset( (void*)buffer, 0, bufsize );
    snprintf( buffer, bufsize, "Hello from %d at %lld\n", rank, offset );
}

static MPI_Offset
get_offset( int rank, int num_objs, int obj_size, int which_obj ) {
    MPI_Offset offset;
    offset = (MPI_Offset)rank * num_objs * obj_size + which_obj * obj_size;
    return offset;
}

static void
write_file( char *target, int rank, MPI_Info *info ) {
    MPI_File wfh;
    MPI_Status mpi_stat;
    int mpi_ret;
    int i;
    char buffer[OBJ_SIZE];

    if ( debug ) printf( "%d writing file %s\n", rank, target );
    
    if( (mpi_ret = MPI_File_open(MPI_COMM_WORLD, target, 
                    MPI_MODE_WRONLY | MPI_MODE_CREATE, *info, &wfh ) )
                != MPI_SUCCESS ) 
    {
        fatal_error( mpi_ret, NULL, "open for write" );
    }

    for( i = 0; i < NUM_OBJS; i++ ) {
        MPI_Offset offset = get_offset( rank, NUM_OBJS, OBJ_SIZE, i );
        fill_buffer( buffer, OBJ_SIZE, rank, offset );
        if ( debug ) printf( "%s", buffer );
        if ( (mpi_ret = MPI_File_write_at_all( wfh, offset, buffer, OBJ_SIZE,
                        MPI_CHAR, &mpi_stat ) ) != MPI_SUCCESS ) 
        {
            fatal_error( mpi_ret, &mpi_stat, "write" );
        }
    }

    if ( debug ) print_hints( rank, &wfh );

    if( (mpi_ret = MPI_File_close( &wfh ) ) != MPI_SUCCESS ) {
        fatal_error( mpi_ret, NULL, "close for write" );
    }
    if ( debug ) printf( "%d wrote file %s\n", rank, target );
}

static int
reduce_corruptions( int corrupt_blocks ) {
    int mpi_ret;
    int sum;
    if ( ( mpi_ret = MPI_Reduce( &corrupt_blocks, &sum, 1, 
                    MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD ) ) != MPI_SUCCESS )
    {
        fatal_error( mpi_ret, NULL, "MPI_Reduce" );
    }
    return sum;
}

static void
read_file( char *target, int rank, MPI_Info *info, int *corrupt_blocks ) {
    MPI_File rfh;
    MPI_Status mpi_stat;
    int mpi_ret;
    int i;
    char buffer[OBJ_SIZE];
    char *verify_buf = NULL;
    verify_buf = (char *)malloc(OBJ_SIZE);

    if ( debug ) printf( "%d reading file %s\n", rank, target );
    
    if( (mpi_ret = MPI_File_open(MPI_COMM_WORLD, target, 
                    MPI_MODE_RDONLY, *info, &rfh ) ) != MPI_SUCCESS ) 
    {
        fatal_error( mpi_ret, NULL, "open for read" );
    }

    for( i = 0; i < NUM_OBJS; i++ ) {
        MPI_Offset offset = get_offset( rank, NUM_OBJS, OBJ_SIZE, i );
        fill_buffer( verify_buf, OBJ_SIZE, rank, offset );
        if ( debug ) printf( "Expecting %s", buffer );
        if ( (mpi_ret = MPI_File_read_at_all( rfh, offset, buffer, OBJ_SIZE,
                        MPI_CHAR, &mpi_stat ) ) != MPI_SUCCESS ) 
        {
            fatal_error( mpi_ret, &mpi_stat, "read" );
        }
        if ( memcmp( verify_buf, buffer, OBJ_SIZE ) != 0 ) {
            (*corrupt_blocks)++;
            printf( "Corruption at %lld\n", offset );
            if ( debug ) {
                printf( "\tExpecting %s\n"
                         "\tRecieved  %s\n",
                         verify_buf, buffer );
            }
        }
    }

    if( (mpi_ret = MPI_File_close( &rfh ) ) != MPI_SUCCESS ) {
        fatal_error( mpi_ret, NULL, "close for read" );
    }
    free(verify_buf);

}

static void
set_hints( MPI_Info *info ) {
    MPI_Info_set( *info, "romio_cb_write", "enable" ); 
    MPI_Info_set( *info, "romio_no_indep_rw", "1" ); 
    MPI_Info_set( *info, "cb_nodes", "1" ); 
    MPI_Info_set( *info, "cb_buffer_size", "4194304" );
}

/*
void
set_hints( MPI_Info *info, char *hints ) {
    char *delimiter = " ";
    char *hints_cp  = strdup( hints );
    char *key = strtok( hints_cp, delimiter );
    char *val;
    while( key ) {
        val = strtok( NULL, delimiter );
        if ( debug ) printf( "HINT: %s = %s\n", key, val );
        if ( ! val ) {
            Usage( __LINE__ ); 
        }
        MPI_Info_set( *info, key, val );
        key = strtok( NULL, delimiter );
    }
    free( hints_cp );
}
*/

int 
main( int argc, char *argv[] ) {
	int nproc = 1, rank = 0;
    char *target = NULL;
    int c;
    MPI_Info info;
    int mpi_ret;
    int corrupt_blocks = 0;

    MPI_Init( &argc, &argv );
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if( (mpi_ret = MPI_Info_create(&info)) != MPI_SUCCESS) {
        if(rank == 0) fatal_error( mpi_ret, NULL, "MPI_info_create.\n");
    }

    prog = strdup( argv[0] );

    while( ( c = getopt( argc, argv, "df:h" ) ) != EOF ) {
        switch( c ) {
            case 'd':
                debug = 1;
                break;
            case 'f':
                target = strdup( optarg );
                break;
            case 'h':
                set_hints( &info );
                break;
            default:
                Usage( __LINE__ );
        }
    }
    if ( ! target ) {
        Usage( __LINE__ );
    }

    write_file( target, rank, &info );
    read_file(  target, rank, &info, &corrupt_blocks );

    corrupt_blocks = reduce_corruptions( corrupt_blocks );
    if ( rank == 0 ) {
	if (corrupt_blocks == 0) {
	    fprintf(stdout, " No Errors\n");
	} else {
            fprintf(stdout, "%d/%d blocks corrupt\n",
                corrupt_blocks, nproc * NUM_OBJS );
	}
    }
    MPI_Info_free(&info);

    MPI_Finalize();
    free(prog);
    exit( 0 );
}
