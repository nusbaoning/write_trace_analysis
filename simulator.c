#include <stdio.h> 
#include <math.h> 
#include <errno.h>
#include <malloc.h>


#define POWER2(x) (1<<(x))
// the unit is KB
// page cache is 1GB
#define PAGE_CACHE_SIZE POWER2(20)
#define PAGE_SIZE 4

#define MAX_INODE_NUM 5000
#define MAX_INODE_SIZE 5000

#define MAX_QUEUE_LENGTH 5000

int inode_table_size = 0;
int inode_table[MAX_INODE_NUM];
 
typedef enum {FALSE, TRUE} bool;

typedef struct {
	bool dirty;
	unsigned int time;
	int inode;
	unsigned long index;
}page_descriptor;


typedef struct {
	unsigned int size;
	page_descriptor page[MAX_INODE_SIZE];
}inode;


//Q1 : How to write the structure of page cache
struct{
	unsigned int size; 
	unsigned int extra_size; 
	inode inode_array[MAX_INODE_NUM];
	page_descriptor extra_page[PAGE_CACHE_SIZE/PAGE_SIZE];
}page_cache;

typedef struct 
{
	unsigned int pid;
	unsigned int time;
}task_struct;

//should be a list
typedef struct {
	task_struct q[MAX_QUEUE_LENGTH];
} workqueue;

void init_inode_table(){
	int i=0;
	for (i = 0; i < MAX_INODE_NUM; ++i)
	{
		inode_table[i] = -1;
	}
}

void init_page_cache(){
	page_cache.size=page_cache.extra_size=0;	
}


int find_inode(int n) {
	int i = 0;
	/*find in the inode table*/
	for(i=0; i<inode_table_size; i++) {
		if (inode_table[i] == n)
		{
			return i;
		}
	}
	return -1;
}

page_descriptor * find_get_page(int inode_num, int index) {
	
	int i = find_inode(inode_num);
	int j, size = 0;
	inode * ind = NULL;
	page_descriptor * p = NULL;
	/*find the inode*/
	if(i>=0) {
		ind = page_cache.inode_array+i;
		p = ind -> page;
		for(j=0; j<ind->size; j++) {
			if (p[j].index == index)
			{
				return &p[j];
			}
		}
		/*inode page array is not full and not found, the page is not in page cache*/
		if (j==ind->size && j<MAX_INODE_SIZE)
		{
			return NULL;
		}
	}
	/*find extra*/
	size = page_cache.extra_size;
	p = page_cache.extra_page;
	for(j=0; j<size; j++) {
		if (p[j].inode == inode_num && p[j].index == index)
		{
			return &p[j];
		}
	}
	return NULL;
}

int main2() {
	int i;
	init_page_cache();
	init_inode_table();
	printf("%d\n", page_cache.size);
	printf("%d\n", inode_table_size);
	scanf("%d", &i);
	return 0;
}
