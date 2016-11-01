#include "unit_test.h"
#include "hash_list.h"

#include "hash_list_unit_test.h"

/** Hash List test exec
 *
 * Runs various tests using hash_lists.
 *
 * @author TEXIER-ATGER Lucas
 * @see hash_list.h
 */

int main (void) {
	INIT_TEST();

	test_hash_list_init();

	test_i_node_hash_list_add();
	test_i_node_hash_list_del();
	test_i_node_hash_list_find();

	test_memory_page_hash_list_add();

	PRINT_TEST_RESULT();

	return 0;
}

void test_hash_list_init (void) {
	struct hash_list table;
	struct i_node *cur = NULL;

	INIT_HASH_LIST(&table, DEFAULT_TABLE_SIZE);

	ASSERT_EQUALS(hash_list_size(table, cur), 0);
	ASSERT_TRUE(is_hash_list_empty(table, cur) );

	free_hash_list(table);
}

void test_i_node_hash_list_add (void) {
	struct i_node node1, node2, *cur = NULL;
	struct hash_list table;

	node1.id = 1;
	node2.id = 2;

	INIT_HASH_LIST(&table, DEFAULT_TABLE_SIZE);

	hash_list_add(&table, &node1.hash_list, node1.id);

	ASSERT_EQUALS(hash_list_size(table, cur), 1);
	ASSERT_FALSE(is_hash_list_empty(table, cur) );

	hash_list_add(&table, &node2.hash_list, node1.id);
	ASSERT_EQUALS(hash_list_size(table, cur), 2);

	free_hash_list(table);
}

void test_i_node_hash_list_del (void) {
	struct i_node node1, node2, node3;
	struct i_node *cur = NULL;
	struct hash_list table;

	node1.id = 1;
	node2.id = 2;
	node3.id = 3;

	INIT_HASH_LIST(&table, DEFAULT_TABLE_SIZE);

	hash_list_add(&table, &node1.hash_list, node1.id);
	hash_list_add(&table, &node2.hash_list, node2.id);
	hash_list_add(&table, &node3.hash_list, node3.id);

	ASSERT_EQUALS(hash_list_size(table, cur), 3);

	hash_list_del(&node1.hash_list);
	ASSERT_EQUALS(hash_list_size(table, cur), 2);

	hash_list_del(&node2.hash_list);
	ASSERT_EQUALS(hash_list_size(table, cur), 1);

	hash_list_del(&node3.hash_list);
	ASSERT_EQUALS(hash_list_size(table, cur), 0);

	free_hash_list(table);
}

void test_i_node_hash_list_find (void) {
	struct i_node node1, node2, node3, node4, node5, node10;
	struct i_node *cur = NULL;
	struct hash_list table;

	node1.id = 1;
	node2.id = 2;
	node3.id = 3;
	node4.id = 4;
	node5.id = 5;
	node10.id = 10;

	node1.file_size = 1024;
	node2.file_size = 4096;
	node3.file_size = 42;
	node4.file_size = 314159;
	node5.file_size = 123456789;
	node10.file_size = 100;

	INIT_HASH_LIST(&table, DEFAULT_TABLE_SIZE);

	hash_list_add(&table, &node1.hash_list, node1.id);
	hash_list_add(&table, &node2.hash_list, node2.id);
	hash_list_add(&table, &node3.hash_list, node3.id);
	hash_list_add(&table, &node4.hash_list, node4.id);
	hash_list_add(&table, &node5.hash_list, node5.id);

	hash_list_find(table, cur, hash_list, node1.id, compare_i_node_id, node1.id);
	ASSERT_TRUE(compare_i_node(cur, &node1) );

	hash_list_find(table, cur, hash_list, node5.id, compare_i_node_id, node5.id);
	ASSERT_TRUE(compare_i_node(cur, &node5) );

	hash_list_find(table, cur, hash_list, node10.id, compare_i_node_id, node10.id);
	ASSERT_TRUE( (cur == NULL) );

	/* print_i_node_table(table); */

	free_hash_list(table);
}

void print_i_node (struct i_node *node) {
	printf("[id=%d; file_size=%d] ", node->id, node->file_size);
}

void print_i_node_table (struct hash_list table) {
	struct i_node *cur = NULL;
	unsigned int i;

	printf("i_node table:\n");

	for (i = 0; i < table.size; i++) {
		printf("table[%d] => ", i);

		list_iter(cur, &table.array[i], hash_list, print_i_node);

		printf("\n");
	}
	printf("\n");
}

int compare_i_node (struct i_node *node1, struct i_node *node2) {
	return
		compare_i_node_id(node1, node2->id) && 
		node1->id == node2->id;
}

int compare_i_node_id (struct i_node *node, unsigned int id) {
	return node->id == id;
}

void test_memory_page_hash_list_add (void) {
	struct memory_page page1, page2, page3, page4;
	struct memory_page *cur = NULL;
	struct hash_list table;

	INIT_HASH_LIST(&table, DEFAULT_MEMORYPAGE_SIZE - 1);

	memory_page_builder(&page1);
	memory_page_builder(&page2);
	memory_page_builder(&page3);
	memory_page_builder(&page4);

	hash_list_add(&table, &page1.l, page1.id);
	hash_list_add(&table, &page2.l, page2.id);
	hash_list_add(&table, &page3.l, page3.id);

	hash_list_find(table, cur, l, page1.id, compare_memory_page, &page1);
	ASSERT_TRUE(compare_memory_page(cur, &page1) );

	hash_list_find(table, cur, l, page2.id, compare_memory_page, &page2);
	ASSERT_TRUE(compare_memory_page(cur, &page2) );

	hash_list_find(table, cur, l, page3.id, compare_memory_page, &page4);
	ASSERT_TRUE( (cur == NULL) );

	/* print_memory_page_table(table); */

	free_hash_list(table);
}

void memory_page_builder (struct memory_page *page) {
	static unsigned int last_id = 0;

	page->id = last_id * DEFAULT_MEMORYPAGE_SIZE;
	page->proc_id = 0;
	last_id++;
}

void print_memory_page (struct memory_page *page) {
	printf("[id=%d; proc_id=%d] ", page->id, page->proc_id);
}

void print_memory_page_table (struct hash_list table) {
	struct memory_page *cur = NULL;
	unsigned int i;

	printf("memory_page table:\n");

	for (i = 0; i < table.size; i++) {
		printf("table[%d] => ", i);

		list_iter(cur, &table.array[i], l, print_memory_page);

		printf("\n");
	}
	printf("\n");
}

int compare_memory_page (struct memory_page* page1, struct memory_page *page2) {
	return
		page1->id == page2->id &&
		page1->proc_id == page2->proc_id;
}