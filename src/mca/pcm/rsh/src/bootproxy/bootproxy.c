/* 
 * $HEADER
 */

#include "ompi_config.h"
#include "include/constants.h"
#include "util/cmd_line.h"
#include "mca/base/base.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"
#include "mca/pcm/base/pcm_base_pack_unpack.h"

/* Timeout for socket listens */

#define FE_TIMEOUT 600


/* Global debug file for debugging purposes */

FILE *debug;

#if 0
#define fprintf(a, b, ...)
#define fopen(a, b)
#endif


/* Global static variables */

static int *vpid_success;
static int cmd_argc;
static char **cmd_argv;
static ompi_value_array_t *hostmap;

/**
 * Bootproxy is a relay proxy for rsh PCM purposes. It will start on
 * each remot e node where processes needs to be launched before any
 * process can be launched.
 *
 * Bootproxy gets on its command line the parent host, port,
 * start_vpid and the argv for the process to be launched (where the
 * same binary needs to be launched on all nodes).
 *
 * Bootproxy will fork/exec the processes which it needs to start on
 * the local node and for remote nodes, it will call rsh PCM
 * launch_processes to do the job
 *
 */
int main(int argc, char *argv[])
{
    int ret;
    int sockfd, peer_port;
    char *peer_address;
    ompi_cmd_line_t *cmd_handle;
    struct sockaddr_in parent_addr;
    int start_vpid;
    fd_set readset;
    struct timeval timeout;
    int nfds, vpid_count;
    int got_env, got_hostmap;
    int got_size_env, got_size_host;
    int size_env, size_host, no_env;
    char ***launch_argv;
    int *launch_argc;
    char *inmap, *env, **outenv;
    mca_pcm_base_module_t *module;
    mca_pcm_t *actions;
    int num_launched;
    bool user_threads, hidden_threads;

    /* Open a file to put debugging information */    
#if 1
    debug = fopen("/tmp/ompi-launch/debug.txt", "a");
    if (NULL == debug) {
	fprintf(stderr, "BOOT: Could not open debug file \n");
	return -1;
    }
    {
	int c = argc;
	int i = 0;
	while (c) {
	    fprintf(debug,"ARG: %s \n", argv[i]);
	    fflush(debug);
	    ++i;
	    --c;
	}
    }

#endif

    /* Create command line handle */

    cmd_handle = NULL;
    cmd_handle = ompi_cmd_line_create();
    if (NULL == cmd_handle) {
	return OMPI_ERROR;
    }

    /* Fill up valid options */

    ret = ompi_cmd_line_make_opt(cmd_handle, 'H',
			        "host", 1, "caller IP address");

    if (OMPI_SUCCESS != ret) {
	fprintf(debug, "ERROR: BOOT: cmd line could not take option\n");
	return ret;
    }

    ret = ompi_cmd_line_make_opt(cmd_handle, 'P',
				"port", 1, "caller port");

    if (OMPI_SUCCESS != ret) {
	fprintf(debug, "ERROR: BOOT: cmd line could not take option\n");
	return ret;
    }

    ret = ompi_cmd_line_make_opt(cmd_handle, 'V',
			        "start_vpid", 1, "starting vpid");

    if (OMPI_SUCCESS != ret) {
	fprintf(debug, "ERROR: BOOT: cmd line could not take option\n");
	return ret;
    }

    /* Parse the cmdline */

    fprintf(debug, "bootproxy: just before cmd_line_parse \n");
    fflush(debug);

    ompi_cmd_line_parse(cmd_handle, 1, argc, argv);

    /* Create a socket for communication with the parent */

    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	return OMPI_ERROR;
    }
    
    peer_port = atoi(ompi_cmd_line_get_param(cmd_handle, "P", 0, 0));
    peer_address = ompi_cmd_line_get_param(cmd_handle, "H", 0, 0);
    start_vpid = atoi(ompi_cmd_line_get_param(cmd_handle, "V", 0, 0));

    /* See if we got any argvs on command line */

    ret = ompi_cmd_line_get_tail(cmd_handle, &cmd_argc, &cmd_argv);
    if (OMPI_ERROR == ret) {
	fprintf(stderr, "Invalid command handle \n");
	return ret;
    }

    launch_argc = NULL;

    if (NULL != cmd_argv) {
	launch_argv = malloc (2 * sizeof(char **));
	launch_argv[0] = cmd_argv;
	launch_argv[1] = NULL;
    } else {
	launch_argv = NULL;
    }

    fprintf(debug, "BOOTPROXY: got host %s port %d vpid %d\n",
	    peer_address, peer_port, start_vpid);
    fflush(debug);

    /* connect to the parent */

    parent_addr.sin_family = AF_INET;
    parent_addr.sin_port = htons(peer_port);
    parent_addr.sin_addr.s_addr = inet_addr(peer_address);
								   
    memset(&(parent_addr.sin_zero), '\0', 8);
    
    fprintf(debug, "BOOTPROXY: now conencting to parent \n");
    fflush(debug);

    if (connect(sockfd, (struct sockaddr *)&parent_addr, 
		sizeof(struct sockaddr)) < 0) {
	perror("BOOT: Connect failed");
	return OMPI_ERROR;
    }
		  
    /* Read the host map array and env from the parent - Do this with
       a timeout, because if the parent crased before it could send
       the data, this recv will block for ever */

    timeout.tv_sec = FE_TIMEOUT;
    timeout.tv_usec = 0;
    
    FD_ZERO(&readset);

    /* Add socket for recv with a timeout */

    got_size_host = 0;
    got_size_env = 0;
    got_hostmap = 0;
    got_env = 0;
    no_env = 0;

    while (1) {

	FD_SET(sockfd, &readset);
	nfds = sockfd + 1;
	
	ret = select(nfds, &readset, NULL, NULL, &timeout);
    
	if (FD_ISSET(sockfd, &readset) != 0) {

	    /* We have something on the sockfd - go read it - it can
	       either be a valid message to be read or if the parent has
	       been killed, a lost connection packet */

	    if (!got_size_host) {
		ret = ompi_recv_message((char *)&size_host, sizeof(int), 
					sockfd);
		if (OMPI_ERROR == ret) {
		    return ret;
		}

		if (0 == ret) { /* Connection closed by peer */
		    return OMPI_ERROR;
		}

		size_host = ntohl(size_host);

		fprintf(debug, "BOOT: got size host %d \n", size_host);
		fflush(debug);

		inmap = (char *) malloc (size_host * sizeof(char));

		got_size_host = 1;
		continue;
	    }

	    if (!got_size_env) {
		ret = ompi_recv_message((char *)&size_env, sizeof(int),
					sockfd);

		if (OMPI_ERROR == ret) {
		    return ret;
		}
		
		size_env = ntohl(size_env);
		
		fprintf(debug, "BOOT: got size env %d  \n", size_env);
		fflush(debug);

		if (0 == size_env) {
		    no_env = 1;
		} else {
		    env = (char *) malloc (size_env * sizeof(char));
		}
		got_size_env = 1;

		continue;
	    }
	    
	    if (!got_hostmap) {
		if (OMPI_ERROR == ompi_recv_message(inmap, size_host,
						    sockfd)) {
		    fprintf(debug, "BOOTPROXY: Recv failed for hostmap"
			    "from parent \n");;
		    return OMPI_ERROR;
		}
		got_hostmap = 1;

		fprintf(debug, "BOOT: got inmap %s \n", inmap);
		fflush(debug);

		if (no_env) {
		    break;
		} else {
		    continue;
		}
	    }

	    if (!got_env) {
		fprintf(debug, "BOOT: getting env \n");
		fflush(debug);

		if (OMPI_ERROR == ompi_recv_message(env, size_env, sockfd)) {
		    fprintf(debug, "BOOTPROXY: Recv failed for hostmap"
			    "from parent \n");;
		    return OMPI_ERROR;
		}
		fprintf(debug, "Got env :%s \n", env);
		fflush(debug);
		got_env = 1;
		break;
	    }
	    
	} else {
	    
	    /* We timed out, so parent is not alive, simply exit */

	    return OMPI_ERROR;
	}
    }

    /* Unpack and put the env in the environment of this process, so that the
       children can make use of it */

    unpack_and_set_environment(env, &outenv);

    /* Convert hostmap to an array format */

    fprintf(debug, "The string from parent is : %s \n", inmap);
    fflush(debug);

    hostmap = unpack_info_from_parent(inmap, &vpid_count);

    /* Create the success array for me and my subtree */

    vpid_success = (int *) malloc (vpid_count * sizeof(int));

    ompi_display(hostmap, debug);

    fprintf(debug, "LAUNCH: now launching on nodes \n");
    fflush(debug);
	
    /* Open mca base */

    if (OMPI_SUCCESS != (ret = mca_base_open())) {
	return ret;
    }

    if (OMPI_SUCCESS != (ret = mca_pcm_base_open())) {

	fprintf(debug, "mca open failed \n");
	fflush(debug);
	return ret;
    }

    if (OMPI_SUCCESS != (ret = mca_pcm_base_select(0, "rsh", &module,
						   &actions,
						   &user_threads, 
						   &hidden_threads))) {
	fprintf(debug, "mca select  failed \n");
	fflush(debug);
	return ret;
    }

    if (OMPI_ERROR == 
	(num_launched = actions->launch_processes(hostmap, 0, 
						 launch_argv,
						 launch_argc, 
						 outenv,
						 start_vpid, 
						 actions))) {
	return OMPI_ERROR;
    }

    /* Send the num_launched back to the parent */

    if (OMPI_ERROR == ompi_send_message((char *)&num_launched,
					sizeof(int), sockfd)) {
	return OMPI_ERROR;
    }

    fprintf(debug, "bootproxy: got num launched as %d \n", num_launched);
    fprintf(debug, "All done in bootproxy - over and out \n");
    fflush(debug);
    return 0;
} 
