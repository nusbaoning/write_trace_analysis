#include <unistd.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

pid_t child_pid1, child_pid2;
int child_status1, child_status2;
int data_pipe1[2], data_pipe2[2];


/* this routine handles the work of the child process. */
void do_child(int number, int data_pipe[]) {
    int c[10];	/* data received from the parent. */
    int rc,i;	/* return status of read().       */

    /* first, close the un-needed write-part of the pipe. */
    close(data_pipe[1]);

    /* now enter a loop of reading data from the pipe, and printing it */
    rc = read(data_pipe[0], &c, sizeof(c));
    for (i = 0; i < 10; ++i)
    {
    	printf("i = %d\t", c[i]*number);
    }
    printf("\n");
    /* probably pipe was broken, or got EOF via the pipe. */
    
}

/* this routine handles the work of the parent process. */
void do_parent(int data_pipe[])
{
    int c,i;	/* data received from the user. */
    int rc;	/* return status of getchar().  */

    /* first, close the un-needed read-part of the pipe. */
    close(data_pipe[0]);

    /* now enter a loop of read user input, and writing it to the pipe. */
    int data[10] = {4,5,6,7,8,9,2,3,1,44};
	/* write the character to the pipe. */
    rc = write(data_pipe[1], &data, sizeof(data));
	if (rc == -1) { /* write failed - notify the user and exit */
	    perror("Parent: write");
	    close(data_pipe[1]);
	    exit(1);
    }
    

    /* probably got EOF from the user. */
    close(data_pipe[1]); /* close the pipe, to let the child know we're done. */
    
}

int child_function(int number, pid_t pid, int pipe[]) {	
	do_child(number, pipe);	
	printf("number = %d, pid = %d\n", number, getpid());
}

int create_child_process(int number, int pipe[]) {
	pid_t pid = fork();
	switch(pid) {
		case -1:	/* fork() failed */
			perror("fork");	/* print a system-defined error message */
			exit(1);
    	case 0:	/* fork() succeeded, we're inside the child process */
			child_function(number, pid, pipe);
			exit(0);	/* here the CHILD process exits, not the parent. */
    	default:	/* fork() succeeded, we're inside the parent process */
		
			return pid;
	}	
}


int main() {
	
	int rc;
	rc = pipe(data_pipe1);
	if(rc == -1) {
		perror("pipe");
		exit(-1);
	}
	rc = pipe(data_pipe2);
	if(rc == -1) {
		perror("pipe");
		exit(-1);
	}
	
	child_pid1 = create_child_process(1, data_pipe1);
	child_pid2 = create_child_process(2, data_pipe2);
	printf("pid1 = %d, pid2 = %d\n", child_pid1, child_pid2);
	do_parent(data_pipe1);
	do_parent(data_pipe2);
	printf("child is dead?\n");
	wait(&child_status1);	/* wait till the child process exits */
	wait(&child_status1);
	wait(&child_status1);
	printf("child is dead\n");
	return 0;
}
