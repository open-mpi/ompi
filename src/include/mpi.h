/*
 * $HEADER$
 */

#ifndef LAM_MPI_H
#define LAM_MPI_H

#include "lam_config.h"

#define MPI_SUCCESS 0
#define MPI_MAX_OBJECT_NAME 64

#define LAM_MPI 1

typedef struct lam_communicator *MPI_Comm;
typedef struct lam_group *MPI_Group;
typedef struct lam_datatype *MPI_Datatype;

extern MPI_Comm MPI_COMM_NULL;
extern MPI_Comm MPI_COMM_WORLD;
extern MPI_Comm MPI_COMM_SELF;

extern MPI_Datatype MPI_TYPE_NULL;

int MPI_Comm_set_name(MPI_Comm comm, char *name);
int MPI_Init(int *argc, char ***argv);
int MPI_Finalize(void);

#if LAM_WANT_MPI_PROFILING

int PMPI_Comm_set_name(MPI_Comm comm, char *name);
int PMPI_Init(int *argc, char ***argv);
int PMPI_Finalize(void);

#endif

#endif /* LAM_MPI_H */
