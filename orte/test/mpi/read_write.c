/*
 * A simple MPI test that reads lines from standard input and writes them
 * to both standard output and a file
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <mpi.h>

int main(int argc, char *argv[])
{
    int self;
    int size;
    int value;
    char line[1024];
    FILE *file;
    unsigned int bytes = 0;
    int reader = 0;
    char *junk;
    
    if (2 == argc) {
        /* a reader was specified */
        reader = strtol(argv[1], NULL, 10);
        fprintf(stderr, "reading from %d\n", reader);
    }
    
    MPI_Init(NULL, NULL);
    MPI_Comm_rank(MPI_COMM_WORLD, &self);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    printf("Hello from process %d of %d\n", self, size);
    MPI_Barrier(MPI_COMM_WORLD);
    if (-1 == reader || reader == self) {
        asprintf(&junk, "./junk%d", self);
        unlink(junk);
        file = fopen(junk, "w+");
        if (NULL == file) {
            fprintf(stderr, "Couldn't open %s!", junk);
            free(junk);
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        while (NULL != fgets(line, sizeof(line), stdin)) {
            fprintf(stderr, "%s", line);
            fprintf(file, "%s", line);
            bytes += strlen(line) + 1;
        }
        fclose(file);
        fprintf(stderr, "\nWrote %d bytes to %s\n", bytes, junk);
        free(junk);
    }
    MPI_Finalize();

    return 0;
}
