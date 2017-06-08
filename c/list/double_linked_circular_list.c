#include "double_linked_circular_list.h"

/** List source file
 *
 * Implementation of double linked circular lists.
 *
 * @author TEXIER-ATGER Lucas
 */

void INIT_LIST_HEAD (struct list_head *head) {
	head->next = head;
	head->previous = head;
}

void list_add (struct list_head *node, struct list_head *head) {
	head->next->previous = node;
	node->next = head->next;
	node->previous = head;
	head->next = node;
}

void list_del (struct list_head *node) {
	node->previous->next = node->next;
	node->next->previous = node->previous;
}

int is_list_empty (struct list_head *head) {
	return head->next == head->previous && head == head->next;
}