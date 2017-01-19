#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define HEADLINENUM 11

void dealWithOneLine(char * line, int len) {
	int i = 0;
	char * token = NULL;
	printf("parse line : %s", line);
// token 0 : task (...)
// token 1 : pid (2566)
// token 2 : cpu (000)
// token 3 : four signs (....)
// token 4 : time stamp (814.618718)
// token 5 : function name (generic_perform_write/generic_file_read_iter)
// token 6 : ino %lu
// token 7 : 0
// token 8 : 1948
// token 9 : 279161685868174
// read 
// trace_printk("%lu,%lld,%lld,%zu,%llu\n", 
// 			inode->i_ino, i_size_read(inode), *ppos, iter->count, time);
//	trace_printk("%zd,%llu\n", 
//				written, time);
// 	write
// 	trace_printk("%lu,%lld,%zu,%llu\n", 
// 					mapping->host->i_ino, pos, i->count,time);
// 	trace_printk("%zd,%llu\n", 
// 					written,time);
	while((token = strsep(&line, " ,[]<>-:"))!=NULL)
	{
		if (token!=NULL && 0!=(*token))
		{	
			//printf("%d\n", token[0]);
			printf("%d : %s\n", i++, token);	

		}		
	}
}

int main(int argc, char const *argv[])
{	
	char dir[] = "/home/baoning/Downloads/data/"; 
	char fn[100];
	char filename[] = "bigtrace_1gb_3";
	FILE* fp;
	char* line = NULL;
	size_t len = 0;
	int i,read = 0;

	//open the file
	strcpy(fn, dir);
	strcat(fn, filename);
	fp = fopen(fn, "r");
	if (fp == NULL)
	{
		exit(-1);
	}

	//read the file line by line
	while((read = getline(&line, &len, fp))!=-1 && i<20) {
		i++;
		if (i <= HEADLINENUM)
		{
			continue;

		}
		//printf("%s\n", line);
		if (i <= HEADLINENUM+6)
		{
			dealWithOneLine(line, read);
		}
		
	};
	free(line);
	fclose(fp);
	//printf("%s\n", fn);
	return 0;
}