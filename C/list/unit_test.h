#ifndef __UNIT_TEST__
#define __UNIT_TEST__

/** My unit test framework
 *
 * This is a little framework of unit tests: it allows to easily assert values.
 *
 * bool: ASSERT_TRUE(1) and ASSERT_FALSE(0)
 * int: ASSERT_EQUALS(1, 1) and ASSERT_NOT_EAQUALS(1, 0)
 *
 * To start a suite test call the MACRO INIT_TEST() and to finish it just
 * call the MACRO PRINT_TEST_RESULT(). Between these two calls you use
 * ASSERT_* MACROS and it will keep track of passed and failed asserts. 
 *
 * If an assert succeeds a succint green message appears. But if it fails
 * it prints a red message containing information about that failure.
 *
 * @author TEXIER-ATGER Lucas
 * @see list_unit_test.c
 * @see hash_list_unit_test.c
 */

#include <stdio.h>
#include <string.h>

#define MESSAGE(format, ...) printf(format, ##__VA_ARGS__);

/**
 * Global variables for test information.
 */
char current_test_name[128] = "";
int current_test, passed_tests, failed_tests;
int asserts, passed_asserts, failed_asserts;
int asserts_in_test, passed_asserts_in_test, failed_asserts_in_test;
char *color;

#define INIT_TEST()              \
	current_test = 0;            \
	passed_tests = 0;            \
	failed_tests = 0;            \
	asserts = 0;                 \
	passed_asserts = 0;          \
	failed_asserts = 0;          \
	INIT_UNIT_TEST();

#define INIT_UNIT_TEST()         \
	asserts_in_test = 0;         \
	passed_asserts_in_test = 0;  \
	failed_asserts_in_test = 0;

#define PRINT_TEST_RESULT()                                               \
	if (passed_tests == current_test) {                                   \
		color = "\033[42m";                                               \
	}                                                                     \
	else if (passed_tests < 3 * current_test / 4) {                       \
		color = "\033[41m";                                               \
	}                                                                     \
	else {                                                                \
		color = "\033[43m";                                               \
	}                                                                     \
	MESSAGE("\n\033[1mUnit tests result:\t");                              \
	MESSAGE("%s[Passed / Failed / Total] = [%d / %d / %d]\033[0m\n",      \
		color, passed_tests, current_test - passed_tests, current_test);  \
	MESSAGE("\033[1mAsserts result: ");                                   \
	MESSAGE("\t%s[Passed / Failed / Total] = [%d / %d / %d]\033[0m\n",      \
		color, passed_asserts, failed_asserts, asserts);

#define ASSERT(value, expected_value, message) ({                                          \
	asserts_in_test++;                                                                     \
	if (strcmp(current_test_name, "") == 0) {                                              \
	}                                                                                      \
	if (strcmp(current_test_name, __FUNCTION__) != 0) {                                    \
		strcpy(current_test_name, __FUNCTION__);                                           \
		current_test++;                                                                    \
		asserts += asserts_in_test;                                                        \
		passed_asserts += passed_asserts_in_test;                                          \
		failed_asserts += failed_asserts_in_test;                                          \
		if (failed_asserts_in_test > 0) {                                                  \
			failed_tests++;                                                                \
			MESSAGE(                                                                       \
				"\033[41m\033[1m#%d unit test %s failed %d asserts out of %d\033[0m\n\n",  \
				current_test, __FUNCTION__, failed_asserts_in_test, asserts_in_test);      \
		}                                                                                  \
		else {                                                                             \
			passed_tests++;                                                                \
			MESSAGE(                                                                       \
				"\033[1;32m#%d unit test %s passed %d asserts\033[0m\n",                   \
				current_test, __FUNCTION__, passed_asserts_in_test);                       \
		}                                                                                  \
		INIT_UNIT_TEST()                                                                   \
	}                                                                                      \
	if (value == expected_value) {                                                         \
		passed_asserts_in_test++;                                                          \
	}                                                                                      \
	else {                                                                                 \
		failed_asserts_in_test++;                                                          \
		MESSAGE(                                                                           \
			"\033[41m\033[1m#%d unit test %s failed asserting %s in file %s at line %d:\n\n\tError: expected %d instead of %d\033[0m\n", \
			current_test, __FUNCTION__, message, __FILE__, __LINE__, expected_value, value); \
	} })

#define ASSERT_TRUE(value) \
	ASSERT(value, 1, "true")

#define ASSERT_FALSE(value) \
	ASSERT(value, 0, "false")

#define ASSERT_EQUALS(value, expected_value) \
	ASSERT(value, expected_value, "equals")

#define ASSERT_NOT_EQUALS(value, expected_value) \
	ASSERT(value, expected_value, "not equals")

#endif