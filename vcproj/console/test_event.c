/* 
 * $HEADER$
 */

#include "ompi_config.h"
#include "event.h"

int main(int argc, char **argv) {

	WSADATA winsockdata;
	WSAStartup (MAKE_WORD(2,2), &winsockdata);

	ompi_init();
	ompi_event_init();
	WSACleanup();


	return 0;
}
