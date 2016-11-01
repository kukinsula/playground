#ifndef __HASH_TABLE_UNIT_TEST__
#define __HASH_TABLE_UNIT_TEST__

/** Hash List test header file
 *
 * @author TEXIER-ATGER Lucas
 */

struct i_node {
	unsigned int id;
	unsigned int file_size;
	struct list_head hash_list;
};

void print_i_node (struct i_node *node);
void print_i_node_table (struct hash_list table);
int compare_i_node (struct i_node *node1, struct i_node *node2);
int compare_i_node_id (struct i_node *node1, unsigned int id);

void test_hash_list_init (void);
void test_i_node_hash_list_add (void);
void test_i_node_hash_list_del (void);
void test_i_node_hash_list_find (void);
void test_i_node_hash_list_iter (void);






struct memory_page {
	unsigned int id;
	unsigned int proc_id;
	struct list_head l;
};

#define DEFAULT_MEMORYPAGE_SIZE 4096

/** Builds a memory_page with an id multiple of 4096.
 *
 * @param page: the memory_page to build
 */
void memory_page_builder (struct memory_page *page);
void print_memory_page (struct memory_page *page);
void print_memory_page_table (struct hash_list table);
int compare_memory_page (struct memory_page* page1, struct memory_page *page2);

void test_memory_page_hash_list_add (void);

#endif