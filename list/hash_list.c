#include <stdlib.h>

#include "double_linked_circular_list.h"

#include "hash_list.h"

/** Hash List source file
 *
 * Implementation of Hash Lists.
 *
 * @author TEXIER-ATGER Lucas
 */

void INIT_HASH_LIST (struct hash_list *table, int size) {
	int i;

	table->size = size;
	table->hash_code = &default_hash_code;
	table->array = (struct list_head*) malloc(sizeof(struct list_head) * size);

	if (table->array == NULL) {
		return ;
	}

	for (i = 0; i < size; i++) {
		INIT_LIST_HEAD(&table->array[i]);
	}
}

void hash_list_add (struct hash_list *table, struct list_head *node, int key) {
	unsigned int index = table->hash_code(*table, key);
	list_add(node, &table->array[index]);
}

void hash_list_del (struct list_head *node) {
	list_del(node);
}

int default_hash_code (struct hash_list table, int key) {
	return key % table.size;
}

 void free_hash_list (struct hash_list table) {
 	free(table.array);
 }
