#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ptrace.h>
#include <errno.h>

void child (int argc, char **argv)
{
  /* Non-portable call to ptrace(). YMMV. */
  long rvl = ptrace (PTRACE_TRACEME, 0, 0, 0);
  if (rvl == -1)
    {
      perror ("child: ptrace(PTRACE_TRACEME) failed");
      exit (1);
    }
  int rvi = execvp (argv[0], argv);
  perror ("child: execvp() failed");
  exit (1);
}

void parent (pid_t pid,
	     const char *program)
{
  fprintf (stderr,
	   "parent: child pid is %d\n",
	   int (pid));
  int status;
  pid_t rvp = waitpid (pid, &status, WUNTRACED);
  if (rvp == -1)
    {
      perror ("parent: waitpid(pid) failed");
      exit (1);
    }
  if (WIFSTOPPED (status))
    {
      fprintf (stderr,
	       "parent: child process stopped on signal %d\n",
	       WSTOPSIG (status));
      int rvi = kill (pid, SIGSTOP);
      if (rvi == -1)
	{
	  perror ("parent: kill(SIGSTOP) failed");
	  exit (1);
	}
      /* Non-portable call to ptrace(). YMMV. */
      long rvl = ptrace (PTRACE_DETACH, pid, 0, (void*) SIGSTOP);
      if (rvl == -1)
	{
	  perror ("child: ptrace(PTRACE_DETACH) failed");
	  exit (1);
	}
    }
  else if (WIFEXITED (status))
    {
      fprintf (stderr,
	       "parent: child process exited with status %d\n",
	       WEXITSTATUS (status));
      exit (1);
    }
  else if (WIFSIGNALED (status))
    {
      fprintf (stderr,
	       "parent: child process kill by signal %d\n",
	       WTERMSIG (status));
      exit (1);
    }
  else
    {
      fprintf (stderr,
	       "parent: child process not stopped, exited, or signaled (status==%d)\n",
	       status);
      exit (1);
    }

  fprintf (stderr,
	   "parent: here's what 'ps' says about the process:\n");
  char cmd[100];
  sprintf (cmd, "ps -u -p %d", int(pid));
  system (cmd);

  fprintf (stderr,
	   "parent: try attaching using one of the following:\n"
	   "1) gdb %s %d\n"
	   "2) totalview -pid %d %s\n",
	   program, int (pid),
	   int (pid), program);

  fprintf (stderr,
	   "parent: waiting for the child to exit\n");
  rvp = waitpid (pid, &status, 0);

  fprintf (stderr,
	   "parent: child exited/terminated, wait status 0x%x\n",
	   status);
}

int main (int argc, char **argv)
{
  if (argc < 2)
    {
      fprintf (stderr,
	       "Usage: %s <program> [<args>]\n",
	       argv[0]);
      exit (1);
    }
  pid_t pid = fork();
  if (pid == -1)
    {
      perror ("parent: fork() failed");
      exit (1);
    }
  else if (pid == 0)
    child (argc - 1, argv + 1);
  else
    parent (pid, argv[1]);
  return 0;
}
