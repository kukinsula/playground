#include <stdio.h>
#include <stdlib.h>
	
#include "double_linked_circular_list.h"
#include "unit_test.h"

#include "list_unit_test.h"

/** List test exec
 *
 * Runs various tests using list_heads.
 *
 * @author TEXIER-ATGER Lucas
 * @see double_linked_circular_list.h
 */

int main (void) {
	INIT_TEST();

	test_list_init(); 
	test_list_add(); 
	test_list_del();
	test_list_for_all(); 
	test_list_find(); 
	test_list_exists(); 
	test_list_index_of(); 
	test_list_contains(); 
	test_list_nth(); 
	test_list_iter();

	PRINT_TEST_RESULT();

	return 0;
}

void test_list_init (void) {
	struct my_object head;
	struct my_object *cur = NULL;

	INIT_LIST_HEAD(&head.list_a);
	INIT_LIST_HEAD(&head.list_b);

	ASSERT_TRUE(is_list_empty(&head.list_a) );
	ASSERT_TRUE(is_list_empty(&head.list_b) );

	ASSERT_EQUALS(list_size(cur, &head.list_a, list_a), 0);
	ASSERT_EQUALS(list_size(cur, &head.list_b, list_b), 0);
}

void test_list_add (void) {
	struct my_object head;
	struct my_object obj;
	struct my_object *cur = NULL;

	obj.value = 0;

	INIT_LIST_HEAD(&head.list_a);
	INIT_LIST_HEAD(&head.list_b);

	list_add(&obj.list_a, &head.list_a);
	list_add(&obj.list_b, &head.list_b);

	ASSERT_FALSE(is_list_empty(&head.list_a) );
	ASSERT_FALSE(is_list_empty(&head.list_b) );

	ASSERT_EQUALS(list_size(cur, &head.list_a, list_a), 1);
	ASSERT_EQUALS(list_size(cur, &head.list_b, list_b), 1);
}

void test_list_del (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;

	INIT_LIST_HEAD(&head.list_a);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);

	ASSERT_EQUALS(list_size(cur, &head.list_a, list_a), 4);
	ASSERT_TRUE(
		list_contains(cur, &head.list_a, list_a, &obj1, compare_my_object) );
	ASSERT_EQUALS(
		list_index_of(cur, &head.list_a, list_a, &obj1, compare_my_object), 3);

	list_del(&obj1.list_a);

	ASSERT_EQUALS(list_size(cur, &head.list_a, list_a), 3);
	ASSERT_FALSE(
		list_contains(cur, &head.list_a, list_a, &obj1, compare_my_object) );
	ASSERT_EQUALS(
		list_index_of(cur, &head.list_a, list_a, &obj1, compare_my_object), -1);
}

void test_list_find (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;

	INIT_LIST_HEAD(&head.list_a);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);

	list_find(cur, &head.list_a, list_a, my_object_value_lower_to, 4);
	ASSERT_EQUALS(cur->value, obj3.value);

	list_find(cur, &head.list_a, list_a, my_object_value_lower_to, -1);
	ASSERT_TRUE( (cur == NULL) );

	list_find(cur, &head.list_a, list_a, my_object_value_even);
	ASSERT_EQUALS(cur->value, obj4.value);
}

void test_list_for_all (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;

	INIT_LIST_HEAD(&head.list_a);
	INIT_LIST_HEAD(&head.list_b);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);

	ASSERT_TRUE(
		list_for_all(
			cur, &head.list_a, list_a, my_object_value_lower_to, 5)
	);

	ASSERT_FALSE(
		list_for_all(
			cur, &head.list_a, list_a, my_object_value_even)
	);

	ASSERT_TRUE(
		list_for_all(
			cur, &head.list_b, list_b, my_object_value_lower_to, 5)
	);

	ASSERT_TRUE(
		list_for_all(
			cur, &head.list_b, list_b, my_object_value_even)
	);
}

void test_list_exists (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;

	INIT_LIST_HEAD(&head.list_a);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);

	ASSERT_TRUE(
		list_exists(
			cur, &head.list_a, list_a, my_object_value_lower_to, 5)
	);

	ASSERT_TRUE(
		list_exists(
			cur, &head.list_a, list_a, my_object_value_even)
	);
}

void test_list_index_of (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4, obj5;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;
	obj5.value = 5;

	INIT_LIST_HEAD(&head.list_a);
	INIT_LIST_HEAD(&head.list_b);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);

	ASSERT_EQUALS(
		list_index_of(
			cur, &head.list_a, list_a, &obj4, compare_my_object), 0);

	ASSERT_EQUALS(
		list_index_of(
			cur, &head.list_a, list_a, &obj3, compare_my_object), 1);

	ASSERT_EQUALS(
		list_index_of(
			cur, &head.list_a, list_a, &obj2, compare_my_object), 2);

	ASSERT_EQUALS(
		list_index_of(
			cur, &head.list_a, list_a, &obj1, compare_my_object), 3);

	ASSERT_EQUALS(
		list_index_of(
			cur, &head.list_a, list_a, &obj5, compare_my_object), -1);

	ASSERT_EQUALS(
		list_index_of(
			cur, &head.list_b, list_b, &obj1, compare_my_object), -1);

	ASSERT_EQUALS(
		list_index_of(
			cur, &head.list_b, list_b, &obj3, compare_my_object), -1);
}

void test_list_contains (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4, obj5;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;
	obj5.value = 5;

	INIT_LIST_HEAD(&head.list_a);
	INIT_LIST_HEAD(&head.list_b);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);
	
	ASSERT_TRUE(
		list_contains(cur, &head.list_a, list_a, &obj1, compare_my_object) );

	ASSERT_TRUE(
		list_contains(cur, &head.list_a, list_a, &obj2, compare_my_object) );

	ASSERT_TRUE(
		list_contains(cur, &head.list_a, list_a, &obj3, compare_my_object) );

	ASSERT_TRUE(
		list_contains(cur, &head.list_a, list_a, &obj4, compare_my_object) );

	ASSERT_FALSE(
		list_contains(cur, &head.list_a, list_a, &obj5, compare_my_object) );

	ASSERT_FALSE(
		list_contains(cur, &head.list_b, list_b, &obj1, compare_my_object) );

	ASSERT_FALSE(
		list_contains(cur, &head.list_b, list_b, &obj3, compare_my_object) );

	ASSERT_FALSE(
		list_contains(cur, &head.list_b, list_b, &obj5, compare_my_object) );
}

void test_list_nth (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;

	INIT_LIST_HEAD(&head.list_a);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);

	ASSERT_EQUALS( (list_nth(cur, &head.list_a, list_a, 0) )->value, 4);
	ASSERT_EQUALS( (list_nth(cur, &head.list_a, list_a, 1) )->value, 3);
	ASSERT_EQUALS( (list_nth(cur, &head.list_a, list_a, 2) )->value, 2);
	ASSERT_EQUALS( (list_nth(cur, &head.list_a, list_a, 3) )->value, 1);

	ASSERT_TRUE( (list_nth(cur, &head.list_a, list_a, 5) == NULL) );
}

void test_list_iter (void) {
	struct my_object head;
	struct my_object *cur = NULL;
	struct my_object obj1, obj2, obj3, obj4;

	obj1.value = 1;
	obj2.value = 2;
	obj3.value = 3;
	obj4.value = 4;

	INIT_LIST_HEAD(&head.list_a);

	list_add(&obj1.list_a, &head.list_a);
	list_add(&obj2.list_a, &head.list_a);
	list_add(&obj3.list_a, &head.list_a);
	list_add(&obj4.list_a, &head.list_a);

	ASSERT_FALSE(
		list_for_all(cur, &head.list_a, list_a, my_object_value_even) );

	list_iter(cur, &head.list_a, list_a, double_my_object_value);

	ASSERT_TRUE(
		list_for_all(cur, &head.list_a, list_a, my_object_value_even) );
}

void print_my_object (struct my_object *obj) {
	printf("value = %d\n", obj->value);
}

int compare_my_object (struct my_object *obj1, struct my_object *obj2) {
	return obj1->value == obj2->value;
}

void double_my_object_value (struct my_object *obj) {
	obj->value *= 2;
}

int my_object_value_lower_to (struct my_object *obj, int value) {
	return obj->value < value;
}

int my_object_value_even (struct my_object *obj) {
	return obj->value % 2 == 0;
}