
/*   
 * $HEADER$
 */

#include "support.h"

#include "class/ompi_circular_buffer_fifo.h"


int main(int argc, char **argv) {

    /* local variables */
    ompi_cb_fifo_t *fifo;
    int size_of_fifo;

    /* get queue size */
    size_of_fifo=atoi(argv[1]);

    /* init result tracking */
    test_init("ompi_circular_buffer_fifo");

    /* finalize result tracking */
    return test_finalize();
}
