#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "class/ompi_free_list.h"
#include "event/event.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "gm.h"

/* maintain list of registered buffers for send and receive */

struct reg_buf {
    void       *start;          /* pointer to registered memory */
    int         length;
};
