/*
 * Copyright (c) 2010      High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/win32/ompi_socket.h"
#include "opal/util/output.h"

int
create_socketpair(int family, int type, int protocol, ompi_socket_t fd[2])
{
	/* This code is originally from Tor.  Used with permission. */

	/* This socketpair does not work when localhost is down. So
	 * it's really not the same thing at all. But it's close enough
	 * for now, and really, when localhost is down sometimes, we
	 * have other problems too.
	 */
#ifdef WIN32
#define ERR(e) WSA##e
#else
#define ERR(e) e
#endif
	ompi_socket_t listener = -1;
	ompi_socket_t connector = -1;
	ompi_socket_t acceptor = -1;
	struct sockaddr_in listen_addr;
	struct sockaddr_in connect_addr;
	ompi_socklen_t size;
	int saved_errno = -1;

	if (protocol
		|| (family != AF_INET
#ifdef AF_UNIX
		    && family != AF_UNIX
#endif
		)) {
        opal_output(0, "Protocol not support: %d", (ERR(EAFNOSUPPORT)));
		return -1;
	}
	if (!fd) {
        opal_output(0, "Invalid socked: %d", (ERR(EINVAL)));
		return -1;
	}

	listener = socket(AF_INET, type, 0);
	if (listener < 0)
		return -1;
	memset(&listen_addr, 0, sizeof(listen_addr));
	listen_addr.sin_family = AF_INET;
	listen_addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	listen_addr.sin_port = 0;	/* kernel chooses port.	 */
	if (bind(listener, (struct sockaddr *) &listen_addr, sizeof (listen_addr))
		== -1)
		goto tidy_up_and_fail;
	if (listen(listener, 1) == -1)
		goto tidy_up_and_fail;

	connector = socket(AF_INET, type, 0);
	if (connector < 0)
		goto tidy_up_and_fail;
	/* We want to find out the port number to connect to.  */
	size = sizeof(connect_addr);
	if (getsockname(listener, (struct sockaddr *) &connect_addr, &size) == -1)
		goto tidy_up_and_fail;
	if (size != sizeof (connect_addr))
		goto abort_tidy_up_and_fail;
	if (connect(connector, (struct sockaddr *) &connect_addr,
				sizeof(connect_addr)) == -1)
		goto tidy_up_and_fail;

	size = sizeof(listen_addr);
	acceptor = accept(listener, (struct sockaddr *) &listen_addr, &size);
	if (acceptor < 0)
		goto tidy_up_and_fail;
	if (size != sizeof(listen_addr))
		goto abort_tidy_up_and_fail;
	closesocket(listener);
	/* Now check we are talking to ourself by matching port and host on the
	   two sockets.	 */
	if (getsockname(connector, (struct sockaddr *) &connect_addr, &size) == -1)
		goto tidy_up_and_fail;
	if (size != sizeof (connect_addr)
		|| listen_addr.sin_family != connect_addr.sin_family
		|| listen_addr.sin_addr.s_addr != connect_addr.sin_addr.s_addr
		|| listen_addr.sin_port != connect_addr.sin_port)
		goto abort_tidy_up_and_fail;
	fd[0] = connector;
	fd[1] = acceptor;

	return 0;

 abort_tidy_up_and_fail:
	saved_errno = ERR(ECONNABORTED);
 tidy_up_and_fail:
	if (saved_errno < 0)
		saved_errno = WSAGetLastError();
	if (listener != -1)
		closesocket(listener);
	if (connector != -1)
		closesocket(connector);
	if (acceptor != -1)
		closesocket(acceptor);

	WSASetLastError(saved_errno);
	return -1;
#undef ERR
}