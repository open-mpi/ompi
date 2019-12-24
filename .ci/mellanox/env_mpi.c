#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

extern char **environ;

int main(int argc, char **argv)
{
    int i=0;
    char *astr;
    MPI_Init(&argc,&argv);
    astr=environ[i];
    while(astr) {
        printf("%s\n",astr);
        astr=environ[++i];
    }
   MPI_Finalize();
}
