#ifndef __HASH_LIST__
#define __HASH_LIST__

/** Hash List header file
 *
 * @author TEXIER-ATGER Lucas
 */

#include "double_linked_circular_list.h"

/* Couldn't think of any other value */
#define DEFAULT_TABLE_SIZE 2


/** Returns the number of elements present in the hash list
 *
 * @param table: the table to measure
 * @param cur: an instance of element in the table
 * @return int
 */
#define hash_list_size(table, cur) ({                        \
	unsigned int size = 0;                                   \
	unsigned int i;                                          \
	for (i = 0; i < table.size; i++) {                       \
		size += list_size(cur, &table.array[i], hash_list);  \
	}                                                        \
	size; })

/** Returns 1 if the table contains no element, 0 otherwise
 *
 * @param table: the table to measure
 * @param cur: an instance of element in the table
 * @return int
 */
#define is_hash_list_empty(table, cur) ({ \
	(hash_list_size(table, cur) == 0); })

/** Finds an element of the table from its key and a comparator function
 *
 * @param table: the table to measure
 * @param cur: an instance of element in the table
 * @param member: the member where cur holds the list_head
 * @param key: key used by the hash_code to search in the right row
 */
#define hash_list_find(table, cur, member, key, comparator, ...) ({         \
	int index = table.hash_code(table, key);                                \
	list_find(                                                              \
		cur, &table.array[index], member, comparator, ##__VA_ARGS__);       \
	})

/** Iters func on all elements of the table
 *
 * @param table: the hash_list run through
 * @param cur: an iterator of type of elements in list head
 * @param member: the name of the member where cur holds the head
 * @param func: the function to iter
 * @param ...: the variable number of args of func
 */
#define hash_list_iter(table, cur, member, func, ...) ({               \
	unsigned int i;                                                    \
	for (i = 0; i < table.size; i++) {                                 \
		list_iter(cur, &table.array[i], member, func, ##__VA_ARGS__);  \
	} })

/** hash_list structure
 *
 * size: the size of its array of list_head
 * hash_code: function pointer to abstract the concrete hash_code implementation
 * array: the array of list_heads
 */
struct hash_list {
	unsigned int size;
	int (*hash_code)(struct hash_list table, int key);
	struct list_head *array;
};


/** Inits the hash_list table with an initial size
 *
 * @param table: the hash_list to initialize
 * @param size: the initial size of the hash_list
 */
void INIT_HASH_LIST (struct hash_list *table, int size);

/** Adds node to the table
 *
 * The node is added to the list of the table at the index calculated
 * by the hash_code function using the given key.
 *
 * @param table: the hash_list to initialize
 * @param size: the initial size of the hash_list
 */
void hash_list_add (struct hash_list *table, struct list_head *node, int key);

/** Removes node from the table
 *
 * @param table: the hash_list to initialize
 * @param node: the node to remove
 */
void hash_list_del (struct list_head *node);

/** Default hash code function for hash_list.
 *
 * You can change the hash_code function by setting it to a hash_list:
 * 		table.hash_code = &my_super_hash_code
 *
 * @param table: the hash_list to initialize
 * @param size: the initial size of the hash_list
 */
int default_hash_code (struct hash_list table, int key);

/** Frees the hash_list table
 *
 * @param table: the hash_list to free
 */
 void free_hash_list (struct hash_list table);
 
#endif