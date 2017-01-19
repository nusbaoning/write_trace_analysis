#include <stdlib.h> 
#include <stdio.h>
#include <math.h>
#include "page_cache.h"



typedef struct {
	unsigned long inode;
	//unsigned long index;     // called index in read and pos in write
	unsigned long end_index; //end_index = (isize - 1) >> PAGE_SHIFT;
	unsigned long isize; 
	unsigned long pos;	
}file_b;

extern int free_memory;

/*
* return : read length
*/
unsigned long do_generic_file_read(file_b * filp, unsigned long count, int time) {
	int j=0;
	unsigned long i = 0;
	page_descriptor * page;
	unsigned long inode, index, nr, isize = 0;
	unsigned long offset = filp->pos & ~PAGE_MASK;
	inode = filp->inode;
	index = filp->pos >> PAGE_SHIFT;
	while(i<count){
		j++;
		// if (j<5)
		// {
		// 	printf("%lu,%lu\n", index, i);
		// }
		//cond_resched();		
		page = find_get_page(inode, index);
		if(page == NULL) {
			read_cache_page(inode, index, time);
		}
		//ignore update page
		nr = PAGE_SIZE;
		if (index == filp->end_index) {
			nr = ((isize - 1) & ~PAGE_MASK) + 1;
			if (nr <= offset) {
				break;
			}
		}
		nr = nr - offset;
		//mark_page_accessed(page);
		//read one page
		//ret = copy_page_to_iter(page, offset, nr, iter);
		offset += nr;
		index += offset >> PAGE_SHIFT;
		offset &= ~PAGE_MASK;
		filp->pos += nr;
		i += nr;
	}
	//printf("%d\n", j);
	return i;
}

unsigned long generic_file_write_iter(file_b * filp, unsigned long count, unsigned long time) {
	int bytes, copied;
	unsigned long pos, 	index, inode, offset;
	written = 0;
	inode = filp -> inode;
	pos = filp->pos;
	int i = 0;
	while(count) {
		i++;
		
		index = pos >> PAGE_SHIFT;
		offset = (pos & (PAGE_SIZE - 1));
		bytes = PAGE_SIZE-offset<count?PAGE_SIZE-offset:count;
		//write_begin
		//memcpy
		
		page_descriptor * page = find_get_page(inode, index);

		// if (i == 1)
		// {
		// 	printf("debug : inode = %lu, index = %lu, page = %p\n", inode, index, page);

		// }

		if (page == NULL)
		{
			printf("NULL\n");
			exit(-1);
			int r = alloc_page();
			if (r < 0)
			{
				perror("page cache is not enough");
				exit(-1);
			}
			add_to_page_cache(inode, index, 
				DIRTY, time);
		}
		else {
			set_page_dirty(page, DIRTY, time);
			// if (i == 1)
			// {
			// 	print_page_cache();
			// 	exit(0);
			// }
		}
		copied = bytes;
		//write_end
		//cons_resched
		pos += copied;
		count -= copied;

		balance_dirty_pages_ratelimited(inode);
	}
	return 1000;
}

int main() {
	init_page_cache();
	file_b f = {3672711, 20000, 20000*PAGE_SIZE, 2927 * PAGE_SIZE};
	file_b f1 = {3672711, 20000, 20000*PAGE_SIZE, 2927 * PAGE_SIZE};
	printf("%s\n", "do read request");
	do_generic_file_read(&f, 1000*PAGE_SIZE, 0);
	print_page_cache();
	printf("%s\n", "do write request");
	generic_file_write_iter(&f1, 1000*PAGE_SIZE, 0);
	print_page_cache();
	
	return 0;
}
