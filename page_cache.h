#ifndef PAGE_CACHE   /* Include guard */
#define PAGE_CACHE

#define DIRTY 1
#define CLEAN 0

#define PAGE_NUM 10000
#define MAX_INODE_NUM 5000

#define POWER2(x) (1<<(x))
// the unit is B
//4KB
#define PAGE_SIZE POWER2(12)
//1GB
#define PAGE_CACHE_SIZE POWER2(30)/PAGE_SIZE
//2MB,1024 page
#define MIN_FREE_MEMORY POWER2(22)/PAGE_SIZE 

#define PAGE_MASK    (~(PAGE_SIZE-1))
#define PAGE_SHIFT 12

typedef enum {FALSE, TRUE} bool;


typedef struct {
	bool dirty;
	unsigned long time;
	unsigned long index;
	unsigned long inode;
}page_descriptor;

typedef struct {
	bool dirty;
	unsigned long size;
	page_descriptor pg[PAGE_NUM];
}page_group;

typedef struct {
	unsigned long size;
	unsigned long inode;
	page_group clean_pages;
	page_group dirty_pages;
}inode_b;

struct {
	unsigned long size;
	unsigned long inode_num;
	inode_b inodes[MAX_INODE_NUM];
}page_cache;

void init_page_cache();

/*
function : search the page in page cache
return the page address if found, otherwise return NULL
*/
page_descriptor * find_get_page(unsigned long inode, unsigned long idx);

int alloc_page();
int release_page();
page_descriptor * add_to_page_cache(unsigned long inode, unsigned long idx, 
	bool dirty, unsigned long time);
void remove_from_page_cache(page_descriptor * p);	
//page_descriptor remove_from_page_cache(unsigned long inode, unsigned long idx);

/*
function : alloc a new page and load the context from disk to the memory page
return : time needed
*/
unsigned long read_cache_page(unsigned long inode, unsigned long idx, unsigned long time);
/*
function : set the page as dirty with time
*/
void set_page_dirty(page_descriptor * page, bool dirty, unsigned long time);

void balance_dirty_pages_ratelimited(unsigned long inode);

void print_page_cache();
 
#endif
