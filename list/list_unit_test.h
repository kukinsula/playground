#ifndef __LIST_UNIT_TEST__
#define __LIST_UNIT_TEST__

/** List test header file
 *
 * @author TEXIER-ATGER Lucas
 */

void test_list_init (void);
void test_list_add (void);
void test_list_del (void);
void test_list_find (void);
void test_list_for_all (void);
void test_list_exists (void);
void test_list_index_of (void);
void test_list_contains (void);
void test_list_nth (void);
void test_list_iter (void);

struct my_object {
	int value;
	struct list_head list_a;
	struct list_head list_b;
};

void print_my_object (struct my_object *obj);
int compare_my_object (struct my_object *obj1, struct my_object *obj2);
void double_my_object_value (struct my_object *obj);
int my_object_value_lower_to (struct my_object *obj, int value);
int my_object_value_even (struct my_object *obj);

#endif