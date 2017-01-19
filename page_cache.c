#include <stdio.h>
#include <stdlib.h>
#include "page_cache.h"

unsigned int free_memory;

void init_page_cache(){
	page_cache.size = page_cache.inode_num = 0;
	free_memory = PAGE_CACHE_SIZE;	
}

int search_inode(int inode) {
	int i;
	for(i=0; i<page_cache.inode_num; i++) {
		// find the inode
		if(page_cache.inodes[i].inode == inode)	{
			return i;
		}
	}
	return -1;
}
int get_page_group_index(page_group * pg, unsigned long idx) {
	int i;
	page_descriptor * p = pg->pg;
	//printf("p = %p\n", p);
	for(i=0; i<pg->size; i++) {
		if(p->index == idx)
			return i;
		p++;		
	}
	
	return -1;
}

page_descriptor * search_in_page_group(page_group * pg, unsigned long idx) {
	
	int i;
	page_descriptor * p = pg->pg;
	//printf("p = %p\n", p);
	for(i=0; i<pg->size; i++) {
		if(p->index == idx)
			return p;
		p++;		
	}
	
	return NULL;
}

page_descriptor * search_in_inode(inode_b * inode, int idx) {
	
	page_group * pg = &inode->clean_pages;
	page_descriptor * p = search_in_page_group(pg, idx);
	if(p==NULL) {
		pg = &inode->dirty_pages;
		p = search_in_page_group(pg, idx);
	}
	//printf("p = %p\n", pg.pg);
	return p;		
}

page_descriptor * find_get_page(unsigned long inode, unsigned long idx) {
	int i;	
	i = search_inode(inode);
	//printf("i = %lu\n", i);
	if(i==-1) {
		//fprintf(stderr, "%lu, %lu\t", inode, idx);
		//perror("");
		return NULL;
	}
	page_descriptor * p = search_in_inode(&page_cache.inodes[i], idx);
	//printf("end search in inode, p=%p\n", p);
	// if(p==NULL) {
	// 	fprintf(stderr, "%lu, %lu\t", inode, idx);
	// 	perror("");
	// 	print_page_cache();		
	// }
	return p;
	
}

int alloc_page() {
	//printf("debug alloc_page, free_memory = %d\n", free_memory);
	if (free_memory > MIN_FREE_MEMORY)
	{
		free_memory--;
		page_cache.size++;
		return 1;
	}
	return -1;
}

int release_page() {
	if (free_memory < PAGE_CACHE_SIZE)
	{
		free_memory++;
		page_cache.size--;
		return 1;
	}
	return -1;
}
int add_inode(int inode) {
	int  i = page_cache.inode_num;
	inode_b * node = &page_cache.inodes[i];
	if (i < MAX_INODE_NUM)
	{

		page_cache.inode_num++;
		node->inode = inode;
		node->size = 0;
		node->clean_pages.size = node->dirty_pages.size = 0;
		node->clean_pages.dirty = CLEAN;
		node->dirty_pages.dirty = DIRTY;

	}
	else {
		perror("Too many inodes : ");
		exit(-1);
	}
	return i;
}

/*
* add a page into page cache without checking whether it exists

*/
page_descriptor * add_to_page_cache(unsigned long inode, unsigned long idx, 
			bool dirty, unsigned long time) {
	int i = search_inode(inode);
	if(i==-1) {
		i = add_inode(inode);
	}
	//search_in_inode(page_cache.inodes[i], idx);
	inode_b * node = &page_cache.inodes[i];
	page_group * pg; 
	if (dirty)
	{
		pg = &node->dirty_pages;
	}
	else
		pg = &node->clean_pages;
	i = pg->size;
	if (i >= PAGE_NUM)
	{
		perror("Too many pages : ");
		exit(-1);
	}
	page_descriptor * p = (page_descriptor *)&(pg->pg) + i;
	pg->size++;
	node->size++;
	
	p->time = time;
	p->index = idx;
	p->inode = inode;
	p->dirty = dirty;
	return p;		
}

void remove_from_page_cache(page_descriptor * p) {		
	if (p == NULL)
	{
		return;
	}
	inode_b * node;
 	page_group * pg;
	int i = search_inode(p->inode);
	node = &page_cache.inodes[i];
	node->size--;
	if (p->dirty == 0)
	{
		pg = &node->clean_pages;
	}
	else
		pg = &node->dirty_pages;
	i = get_page_group_index(pg, p->index);
	page_descriptor * p_o = &pg->pg[i];
	page_descriptor * p_last = &pg->pg[pg->size-1];
	p_o -> time = p_last->time;
	p_o -> index = p_last->index;

	pg->size--;
	
	release_page();
}

// page_descriptor remove_from_page_cache(int inode, int idx) {
// 	int i = search_inode(inode);
// 	if(i==-1) {
// 		return NULL;
// 	}

// 	inode_b * node = &page_cache.inodes[i];
// 	page_group * pg = &node->clean_pages;
// 	page_descriptor * p = search_in_page_group(pg, idx);
// 	if(p==NULL) {
// 		pg = &inode->dirty_pages;
// 		p = search_in_page_group(pg, idx);
// 	}


// 	page_descriptor * p = pg->pg;
// 	//printf("p = %p\n", p);
// 	for(i=0; i<pg->size; i++) {
// 		if(p->index == idx)
// 			return p;
// 		p++;		
// 	}

// 	if (p == NULL)
// 	{
// 		return 
// 	}
// }

unsigned long read_cache_page(unsigned long inode, unsigned long idx, unsigned long time) {
	int result = alloc_page();
	if (result < 0)
	{
		perror("page cache is not enough");
		exit(-1);
	}
	add_to_page_cache(inode, idx, CLEAN, time);
	return 800;
}

void balance_dirty_pages_ratelimited(unsigned long inode) {
	return;
}

void print_page(page_descriptor p) {
	printf("%d\t%lu\t%lu\t%lu\t\n", p.dirty, p.time, p.index, p.inode);
}

void set_page_dirty(page_descriptor * page, bool dirty, unsigned long time) {		
	unsigned long inode = page->inode;
	unsigned long index = page->index;
	remove_from_page_cache(page);	
	add_to_page_cache(inode, index, dirty, time);	
}

void print_page_group(page_group p) {
	
	char tag[2][6] = {"clean", "dirty"};
	printf("%s group : size = %lu\n", tag[p.dirty], p.size);
	
	page_descriptor * pdp = p.pg;
	int i;
	unsigned long max,min,index;
	max = min = 0;
	for (i=0; i < p.size; ++i, ++pdp)
	{		
		index = pdp->index;
		if (i == 0)
		{
			max = min = index;
		}
		else {
			if (index < min)
			{
				min = index;
			}
			else if (index > max)
			{
				max = index;
			}
		}
		
	}
	printf("min index = %lu, max index = %lu",min, max);
	printf("\n");
}

void print_page_cache() {
	int j;
	printf("page cache size = %lu, total inode_num = %lu\n", page_cache.size, page_cache.inode_num);
	for (j = 0; j < page_cache.inode_num; ++j)
	{
		inode_b i = page_cache.inodes[j];
		printf("**********************************\n");
		printf("inode id = %lu, inode size = %lu\n", i.inode, i.size);
		print_page_group(i.clean_pages);
		print_page_group(i.dirty_pages);
	}
}
// int main2() {
// 	init_page_cache();
// 	printf("%lu\n", free_memory);
// 	page_descriptor * p = add_to_page_cache(1,1,1,0);
// 	printf("p = %p\n", p);
// 	add_to_page_cache(1,2,1,0);
// 	add_to_page_cache(2,1,1,0);
// 	add_to_page_cache(3,1,1,0);
// 	add_to_page_cache(4,1,0,800);
// 	printf("page_cache size = %lu\n", page_cache.size);
// 	printf("inode num = %lu\n", page_cache.inode_num);
	
	
	
// 	p = find_get_page(1,1);
// 	printf("p = %p\n", p);
// 	printf("inode = %lu, index = %lu, time = %lu\n", p->inode, p->index, p->time);
// 	p = find_get_page(2,1);
// 	printf("inode = %lu, index = %lu, time = %lu\n", p->inode, p->index, p->time);
// 	p = find_get_page(3,1);
// 	printf("inode = %lu, index = %lu, time = %lu\n", p->inode, p->index, p->time);
// 	p=find_get_page(3,0);
// 	if (p!=NULL)
// 	{
// 		printf("inode = %lu, index = %lu, time = %lu\n", p->inode, p->index, p->time);
// 	}
	
// 	p=find_get_page(4,1);
// 	printf("inode = %lu, index = %lu, time = %lu\n", p->inode, p->index, p->time);
// }
