/*
 * $HEADER$
 */

/** @file **/

#include "ompi_config.h"

#include "include/constants.h"
#include "runtime/runtime.h"
#include "util/output.h"
#include "threads/mutex.h"
#include "event/event.h"


/**
 * First function that must be called in a OMPI process.
 *
 * @param argc Passed in from main()
 * @param argv Passed in from main()
 *
 * @retval OMPI_SUCCESS Upon success.
 * @retval OMPI_ERROR Upon failure.
 *
 * This is the first function that must be called for a OMPI process.
 * It sets up the following:
 *
 * - calls ompi_output_init(): sets up default verbose/0 stream
 *
 * - calls ompi_set_using_threads(): sets value to false (someone else,
 *   such as MPI_INIT, may reset this value later)
 *
 * More will likely be filled in here in the future.  Note that OMPI
 * RTE MCA module setup is distinctly \em not covered in this function
 * -- not all OMPI processes need OMPI RTE MCA modules (e.g., wrapper
 * compilers).  OMPI RTE MCA modules are initialized in ompi_rte_init().
 * 
 * Just a note for developer: 

 * So there are 3 ways in which an application can be started
 * 1) rte_boot, followed by mpirun
 * 2) mpirun (alone)
 * 3) singleton (./a.out)
 * 
 * Case 1) If the rte has already been booted, then mpirun will accept
 * an optional command line parameter --universe=<rte universe name>
 * which says which universe this application wants to be a part
 * of. mpirun will then package this universe name and send it to the
 * processes it will be starting off (fork/exec) on local or remote
 * node.The packaging mechanism can be either command line parameter
 * to the a.out it forks or make it part of environment
 * (implementation dependent).  
 *
 * Case 2) When mpirun is done alone and no universe is present, then
 * the mpirun starts off the universe (using rte_boot), then
 * fork/execs the processes, passin g along the <universe_name>. 
 *
 * Case 3) For a singleton, if there is alrady an existing rte
 * universe which it wants to join, it can specify that using the
 * --universe command line. So it will do 
 *
 * $ ./a.out --universe=<universe_name>
 * 
 * In this case, MPI_Init will have to be called as MPI_Init(&argc, &argv)

 * If it does not want to join any existing rte, then it just starts
 * off as ./a.out with no command line option. In that case, MPI_Init
 * does not necesaarily needs to passed argc and argv. Infact if argc
 * and argv are not passed or just have one entry (the command name),
 * then MPI_Init would assume that new rte universe needs to be
 * started up.
 *
 *
 * MPI_Init() will look at its argc, argv. If it find the universe
 * name there, fine. Else it looks at the environment variables for
 * universe_name. If it finds there, fine again. Under such
 * conditions, it joins the existing rte universe. If no universe
 * name is found, it calls rte_boot to start off a new rte universe.
 *
 * For singleton, MPI_Init() do:
 *
 * if (I am a singleton) and (there is no universe)
 *    do rte_boot
 *
 * But if I am not a singleton, then I have been started by mpirun and
 * already provided a universe_name to join. So I wont ever start a
 * universe under such conditons. mpirun will pass me the
 * universe_name (either mpirun would have started the universe, or it
 * would have been already started by rte_boot)
 */
int ompi_init(int argc, char *argv[])
{
  /* Open up the output streams */

  if (!ompi_output_init())
    return OMPI_ERROR;

  /* For the moment, the OMPI library is not multi-threaded.  MPI_INIT
     may reset this value later, but for now, we say that we are not
     using threads. */

  ompi_set_using_threads(false);

  /* For malloc debugging */

  ompi_malloc_init();

  /* Other things that we'll probably need:

     - session directory setup
     - ...?
  */

  /* All done */

  return OMPI_SUCCESS;
}
