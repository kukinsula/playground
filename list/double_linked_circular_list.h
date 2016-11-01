#ifndef __DOUBLE_LINKED_CIRCULAR_LIST__
#define __DOUBLE_LINKED_CIRCULAR_LIST__

/** List header file
 *
 * @author TEXIER-ATGER Lucas
 */

#include <stddef.h>

#define typeof __typeof__


/** Returns the struct or the union containing a list_head.
 *
 * @param ptr: the address of the list_head contained by a struct or union
 * @param type: the stuct or union name
 * @param member: the stuct's or union's member name
 * @return generic element
 */
#define container_of(ptr, type, member) ({                   \
	const typeof( ( (type *)0)->member) *__mptr = (ptr);     \
	(type *)( (char *)__mptr - offsetof(type, member) ); })


/** Runs through all elements of the list head
 *
 * cur example: struct my_object *cur = NULL;
 *
 * @param cur: an iterator of type of elements in list head
 * @param head: the head of the list
 * @param member: the name of the member where cur holds the head
 *
 * example: container_of(my_object*, my_object, list_a)
 */
#define list_for_each_entry(cur, head, member)                        \
	for (cur = container_of( (head)->next, typeof(*cur), member);     \
		&cur->member != (head);                                       \
		cur = container_of(cur->member.next, typeof(*cur), member) )


/** Returns the size of the list head
 *
 * @return unsigned int
 */
#define list_size(cur, head, member) ({           \
		unsigned int size = 0;                    \
		list_for_each_entry(cur, head, member) {  \
			size++;                               \
		}                                         \
		size; })


/**
 * The folowing macros help to use lists like in a functional programming way
 * because we can, kind of, pass them a function as argument.
 */


/** Iters the function func on all elements of the list head
 *
 * @param func: the name of the function to iter
 * @param ...: func variable args (variadics macro args)
 */
#define list_iter(cur, head, member, func, ...)  \
	list_for_each_entry(cur, head, member)       \
		func(cur, ##__VA_ARGS__);

/** Returns the first element of the list head that satisfies p
 *
 * @param p: the name of the function to iter
 * @param ...: p variable args (variadics macro args)
 */
#define list_find(cur, head, member, p, ...)  ({  \
 	int find = 0;                                 \
 	list_for_each_entry(cur, head, member) {      \
 		if (p(cur, ##__VA_ARGS__) == 1) {         \
 			find = 1;                             \
 			break;                                \
 		}                                         \
 	}                                             \
 	if (find == 0) {                              \
 		cur = NULL;                               \
 	}                                             \
	cur; });


/** Returns the position of node in the list head if present, -1 if not
 *
 * The comparator function's 2 first parameters should be of the type
 * of the elements in the list and return 1 if they are equal, 0 otherwise.
 *
 * List's index start from 0.
 *
 * @param node: the node to look for in the list
 * @param comparator: the comparator function to use to find the node's index
 * @param ...: comparator variable args (variadics macro args)
 * @return int
 */
#define list_index_of(cur, head, member, node, comparator, ...) ({  \
	int i = -1;                                                     \
	int present = 0;                                                \
	list_for_each_entry(cur, head, member) {                        \
		i++;                                                        \
		if (comparator(cur, node, ##__VA_ARGS__) == 1) {            \
			present = 1;                                            \
			break;                                                  \
		}                                                           \
	}                                                               \
	present ? i : -1; })


/** Returns 1 if node appears in the list head, 0 otherwise
 *
 * The comparator function's 2 first parameters should be of the type
 * of the elements in the list and return 1 if they are equal, 0 otherwise.
 *
 * @param node: the node to look for in the list
 * @param comarator: the name of the comprator function
 * @param ...: comparator variable args (variadics macro args)
 */
#define list_contains(cur, head, member, node, comparator, ...) \
  (list_index_of(cur, head, member, node, comparator, ##__VA_ARGS__) > -1)


/** Returns 1 if all elements in head satisfy the function p, 0 otherwise
 *
 * The p function's first parameter should be of type of the
 * elements in the list and return 1 if they are equal, 0 otherwise.
 *
 * @param node: the node to look for in the list
 * @param p: the name of the comprator function
 * @param ...: p variable args (variadics macro args)
 * @return int
 */
#define list_for_all(cur, head, member, p, ...) ({     \
	int for_all = 1;                                   \
	list_for_each_entry(cur, head, member) {           \
		if (p(cur, ##__VA_ARGS__) != 1) {              \
			for_all = 0;                               \
			break;                                     \
		}                                              \
	}                                                  \
	for_all; })


/** Returns 1 if all elements in head satisfy the function p, 0 otherwise
 *
 * The p function should take at least 1 parameter of type of the elements
 * in the list and return 1 if this element satisfy p, 0 otherwise.
 *
 * @param node: the node to look for in the list
 * @param p: the name of the comprator function
 * @param ...: p variable args (variadics macro args)
 * @return int
 */
#define list_exists(cur, head, member, p, ...) ({     \
	int exists = 0;                                   \
	list_for_each_entry(cur, head, member) {          \
		if (p(cur, ##__VA_ARGS__) == 1) {             \
			exists = 1;                               \
			break;                                    \
		}                                             \
	}                                                 \
	exists; })


/** Returns the n-th element of the list head, or NULL if absent
 *
 * List's index start from 0.
 *
 * @param index: the index of the element to return
 * @return element
 */
#define list_nth(cur, head, member, index) ({  \
	int i = 0;                                 \
	list_for_each_entry(cur, head, member) {   \
		if (i == index) {                      \
			break;                             \
		}                                      \
		i++;                                   \
	}                                          \
	i == index ? cur : NULL; })


/** list_head: recursive struct
 *
 * Holds pointers to its next and previous element
 * in the the list_head to which it was added to.
 */
struct list_head {
	struct list_head *next, *previous;
};


/** Inits head be the head of a list
 *
 * @param head: the head to init
 */
void INIT_LIST_HEAD (struct list_head *head);


/** Adds node in the list head
 *
 * Node is added right after head.
 *
 * @param head: the head to init
 */
void list_add (struct list_head *node, struct list_head *head);


/** Removes node from the list head
 *
 * @param node: the node to remove
 */
void list_del (struct list_head *node);


/** Returns 1 if the list head is empty, 0 otherwise
 *
 * @param head: the list head
 * @return int
 */
int is_list_empty (struct list_head *head);

#endif
