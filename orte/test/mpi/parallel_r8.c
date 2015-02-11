
/* parallel MPI read from a single file */

#include "mpi.h"
#include <stdio.h>
#include <string.h>

#define D      3             /* dimensions */

#define X   256             /* global x grid size */
#define Y   256             /* global y grid size */
#define Z   256             /* global z grid size */

#define nx   128             /* local x grid size */
#define ny   128             /* local y grid size */
#define nz   128             /* local z grid size */

#define ng (nx*ny*nz)        /* local grid (cube) size */

#define npx    2             /* number of PE's in x direction */
#define npy    2             /* number of PE's in y direction */
#define npz    2             /* number of PE's in z direction */

#define np (npx*npy*npz)  /* total PE count */

#define LOOP 1

#define MAX_RR_NAME 7

int
main(int argc, char* argv[])
{
  int  i, rank, npes, bug=0;
  int buf[ng];
  MPI_File     thefile;
  MPI_Status   status;
  MPI_Datatype filetype;
  MPI_Comm     new_comm;
  MPI_Offset   offset=0;
  MPI_Info     info=MPI_INFO_NULL;
  int gsize[D],distrib[D],dargs[D],psize[D];
  int dims[D],periods[D],reorder;
  double t1,t2,mbs;
  double to1,to2,tc1,tc2;
  double et,eto,etc;
  double max_mbs,min_mbs,avg_mbs;
  double max_et,min_et,avg_et;
  double max_eto,min_eto,avg_eto;
  double max_etc,min_etc,avg_etc;
  char process_name[MPI_MAX_PROCESSOR_NAME + 1];
  char rr_blank[] = {"       "};
  char rr_empty[] = {"???????"};
  int  count;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &npes);
  if ( rank == 0 )
    {
     if ( argc < 2 )
       {
        printf(" ERROR: no filename given\n");
        bug++;
       }
     if ( npes == np )
       {
        printf(" file name: %s\n",argv[1]);
        printf(" total number of PE's: %3d\n",np);
        printf(" number of PE's in x direction: %3d\n",npx);
        printf(" number of PE's in y direction: %3d\n",npy);
        printf(" number of PE's in z direction: %3d\n",npz);
        printf(" global grid size: %dx%dx%d 4 byte integers (total %lu)\n",X,Y,Z,(unsigned long)X*Y*Z);
        printf("  local grid size: %dx%dx%d 4 byte integers (total %d)\n",nx,ny,nz,ng);
       }
     else
       {
        printf(" ERROR: total number of PE's must be %d\n",np);
        printf("        actual number of PE's was %d\n",npes);
        bug++;
       }
     if ( bug )
       {
        MPI_Abort(MPI_COMM_WORLD,-1);
       }
    }
 if ( MPI_Get_processor_name(process_name, &count) != MPI_SUCCESS)
   {
    sprintf(process_name, "%s", rr_empty);
   }
 else
   {
    if (count < MAX_RR_NAME) strncat(&process_name[count],rr_blank,MAX_RR_NAME-count);
    process_name[MAX_RR_NAME] = '\0';
   }

  MPI_Info_create(&info);

/* allow multiple writers to write to the file concurrently */

/*MPI_Info_set(info,"panfs_concurrent_write","1");*/

/* use data aggregation */

/*MPI_Info_set(info,"romio_cb_write","enable"); */
/*MPI_Info_set(info,"romio_cb_write","disable");*/
/*MPI_Info_set(info,"romio_cb_read","enable"); */
/*MPI_Info_set(info,"romio_cb_read","disable");*/

/* use one aggregator/writer per node */

/*MPI_Info_set(info,"cb_config_list","*:1");*/

/* aggregators/writers per allocation: use this or the above (both work) */

/*i = ((npes-1)/8) + 1;
  sprintf(awpa,"%d",i);
  MPI_Info_set (info,"cb_nodes",awpa);*/

  for ( i=0; i<D; i++ )
    {
     periods[i] = 1;  /* true */
    }

  reorder = 1;        /* true */

  dims[0] = npx;
  dims[1] = npy;
  dims[2] = npz;

  MPI_Cart_create(MPI_COMM_WORLD, D, dims, periods, reorder, &new_comm);

  for ( i=0; i<D; i++ )
    {
     distrib[i] = MPI_DISTRIBUTE_BLOCK;
     dargs[i]   = MPI_DISTRIBUTE_DFLT_DARG;
/*   psize[i]   = 0; */
    }

  gsize[0] = X;
  gsize[1] = Y;
  gsize[2] = Z;

  psize[0] = npx;
  psize[1] = npy;
  psize[2] = npz;

/*
  MPI_Dims_create(npes, D, psize);  

  printf("psize %d %d %d\n",psize[0],psize[1],psize[2]);
*/

  MPI_Type_create_darray(npes, rank, D, gsize, distrib, dargs, psize, MPI_ORDER_FORTRAN, MPI_INT, &filetype);

  MPI_Type_commit(&filetype);
  
  to1 = MPI_Wtime();
  MPI_File_open(new_comm, argv[1], MPI_MODE_RDONLY, info, &thefile);
  to2 = MPI_Wtime();

  MPI_File_set_view(thefile, offset, MPI_INT, filetype, "native", MPI_INFO_NULL);

  t1 = MPI_Wtime();
  for ( i=0; i<LOOP; i++ )
    {
     MPI_File_read_all(thefile, buf, ng, MPI_INT, &status);
    }
  t2 = MPI_Wtime();

/*MPI_File_sync(thefile); */

  tc1 = MPI_Wtime();
  MPI_File_close(&thefile);
  tc2 = MPI_Wtime();

  et  = (t2  - t1)/LOOP;
  eto = (to2 - to1)/LOOP;
  etc = (tc2 - tc1)/LOOP;

  mbs = (((double)(LOOP*X*Y*Z)*sizeof(int)))/(1000000.0*(t2-t1));

/*printf(" %s[%3d]    ET  %5.2f  %6.2f  %6.2f     %5.1f mbs       Data %9d %9d \n", process_name, rank, t1, t2, t2-t1, mbs, buf[0], buf[ng-1]);*/

  MPI_Barrier(MPI_COMM_WORLD);

  MPI_Reduce(&mbs, &avg_mbs, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Reduce(&mbs, &min_mbs, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&mbs, &max_mbs, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

  MPI_Reduce(&et, &avg_et, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Reduce(&et, &min_et, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&et, &max_et, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

  MPI_Reduce(&eto, &avg_eto, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Reduce(&eto, &min_eto, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&eto, &max_eto, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

  MPI_Reduce(&etc, &avg_etc, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Reduce(&etc, &min_etc, 1, MPI_DOUBLE, MPI_MIN, 0, MPI_COMM_WORLD);
  MPI_Reduce(&etc, &max_etc, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

  fflush(stdout);

  if ( rank == 0 )
    {
     mbs = avg_mbs/npes;
     printf("\n     average read rate: %9.1f mbs\n", mbs);
     printf("     minimum read rate: %9.1f mbs\n", min_mbs);
     printf("     maximum read rate: %9.1f mbs\n\n", max_mbs);
     avg_eto = avg_eto/npes;
     avg_et  = avg_et/npes;
     avg_etc = avg_etc/npes;
     printf("     open time:  %9.3f min %9.3f avg %9.3f max\n",min_eto,avg_eto,max_eto);
     printf("     read time:  %9.3f min %9.3f avg %9.3f max\n",min_et,avg_et,max_et);
     printf("     close time: %9.3f min %9.3f avg %9.3f max\n\n",min_etc,avg_etc,max_etc);
     fflush(stdout);
    }

  MPI_Finalize();

  return 0;
}
