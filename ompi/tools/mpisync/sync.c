/*
 * Copyright (C) 2014 Artem Polyakov <artpol84@gmail.com>
 */

#include <stdio.h>
#include <mpi.h>
#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include "hpctimer.h"
#include "mpigclock.h"

typedef enum { Gen, Chk } prog_mode_t;

char *filename = NULL;
prog_mode_t mode = Gen;
double orig_rtt = 0.0, orig_offs = 0.0;

void print_help(char *progname);
int parse_opts(int rank, int argc, char **argv);

void print_help(char *progname)
{
    printf("%s: ./%s -o <output file>\n", progname, progname);
}

int parse_opts(int rank, int argc, char **argv)
{
    while (1) {
        int option_index = 0;
        static struct option long_options[] = { 
            {"output", required_argument, 0, 'o' },
            {"help",   required_argument, 0, 'h' },
            { 0,       0,                 0, 0   } };

        int c = getopt_long(argc, argv, "o:h",
            long_options, &option_index);
        if (c == -1)
            break;
        switch (c) {
        case 'h':
            if( rank == 0 )
                print_help(argv[0]);
            return 1;
        case 'o':
            filename = strdup(optarg);
            if( filename == NULL ){
                perror("Cannot allocate memory");
                return -1;
            }
            break;
        default:
            return -1;
        }
    }
    return 0;
}

int main(int argc, char **argv)
{
    MPI_Init(&argc, &argv);
    MPI_Comm comm = MPI_COMM_WORLD;
    int rank, commsize;
    double offs, rtt;
    char hname[1024];

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &commsize);

    int ret = parse_opts(rank, argc, argv);
    
    if( ret < 0 ){
        // Error exit
        MPI_Finalize();
        exit(1);
    }else if( ret > 0 ){
        // Normal exit after help printout
        MPI_Finalize();
        exit(0);
    }

    if( filename == NULL ){
        if( rank == 0 ){
            fprintf(stderr, "The name of output file wasn't specified. Abort\n");
        }
        MPI_Finalize();
        exit(1);
    }


    if( gethostname(hname, 1024) ){
        perror("Cannot get hostname");
        MPI_Finalize();
        exit(1);
    }

    // Clear output file if it exists
    if( rank == 0 ){
        FILE  *fp = fopen(filename, "w");
        if( fp == NULL ){
            fprintf(stderr,"Cannot open output file %s for writing. Abort: %s\n",
                     filename, strerror(errno));
            MPI_Finalize();
            exit(1);
        }
        fclose(fp);
    }

    int rc = hpctimer_initialize("gettimeofday");

    if( rc == HPCTIMER_FAILURE ){
        fprintf(stderr, "Fail to initialize hpc timer. Abort\n");
        MPI_Finalize();
        exit(1);
    }

    offs = mpigclock_sync_linear(comm, 0, &rtt);

    FILE *fp = fopen(filename,"a");
    if( fp == NULL ){
        fprintf(stderr, "Cannot open %s for appending. Abort\n", filename);
        MPI_Finalize();
        exit(1);
    }
    fprintf(fp, "%s %lf %lf\n", hname, rtt, offs);
    fclose(fp);

    MPI_Finalize();
    return 0;
}
