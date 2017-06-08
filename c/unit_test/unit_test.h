#ifndef __UNIT_TEST__
#define __UNIT_TEST__

#include "../logger/logger.h"

#define FUNCTION_NAME_LEN 128

/** Unit test framework.
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
 */

char __current_test_name[FUNCTION_NAME_LEN] = "";
int __first_unit_test;
int __current_test, __passed_tests, __failed_tests;
int __asserts, __passed_asserts, __failed_asserts;
int __asserts_in_test, __passed_asserts_in_test, __failed_asserts_in_test;

void INIT_TEST();
void INIT_UNIT_TEST();
void EXIT_TEST();
void __assert(int value, int expected_value, char* message, char* filename, int line, const char* function);

void INIT_TEST() {
  __first_unit_test = 1;

  __current_test = 0;
  __passed_tests = 0;
  __failed_tests = 0;

  __passed_asserts = 0;
  __failed_asserts = 0;

  __asserts = 0;

  INIT_UNIT_TEST();
}

void INIT_UNIT_TEST() {
  __asserts_in_test = 0;
  __passed_asserts_in_test = 0;
  __failed_asserts_in_test = 0;
}

#define ASSERT(value, expected_value, msg) \
  __assert(value, expected_value, msg, __FILE__, __LINE__, __func__)

#define ASSERT_TRUE(value) ASSERT(value, 1, "ASSERT_TRUE")
#define ASSERT_FALSE(value) ASSERT(value, 0, "ASSERT_FALSE")
#define ASSERT_EQUALS(value, expected_value) ASSERT(value, expected_value, "ASSERT_EQUALS")
#define ASSERT_NOT_EQUALS(value, expected_value) ASSERT(value, expected_value, "ASSERT_NOT_EQUALS")

void __assert(int value, int expected_value, char* message, char* filename, int line, const char* function) {
  if (__first_unit_test) {
    __first_unit_test = 0;

    strncpy(__current_test_name, function, FUNCTION_NAME_LEN);
  }

  __asserts++;
  
  if (value == expected_value) {
    __passed_asserts_in_test++;
  } else {
    __failed_asserts_in_test++;

    redf("[ FAILED ASSERT ] %s:%d in %s:\n\n\t", filename, line, function);
    redf("%s expected %d instead of %d", message, expected_value, value);
  }

  /* if new unit test */
  if (strncmp(__current_test_name, function, strlen(function)) != 0) {
    if (__failed_asserts_in_test > 0) {
      __failed_tests++;
      redf("#%d unit test %s failed %d asserts out of %d\n",
        __current_test, function, __failed_asserts_in_test, __asserts_in_test);
    } else {
      __passed_tests++;
      greenf("#%d unit test %s passed %d asserts",
        __current_test, function, __passed_asserts_in_test);
    }

    strncpy(__current_test_name, function, FUNCTION_NAME_LEN);

    __current_test++;
    __passed_asserts += __passed_asserts_in_test;
    __failed_asserts += __failed_asserts_in_test;

    INIT_UNIT_TEST();
  } else {
    __asserts_in_test++;
  }
}

void EXIT_TEST() {
  int color;

  if (__passed_tests == __current_test) {
    color = Green;
  } else if (__passed_tests < 3 * __current_test / 4) {
    color = Yellow;
  } else {
    color = Red;
  }

  whitef("\n\nUnit tests result:\t", NULL);
  colorf(color, "[Passed / Failed / Total] = [%d / %d / %d]\n",
    __passed_tests, __current_test - __passed_tests, __current_test);

  whitef("Asserts result: ", NULL);
  colorf(color, "\t[Passed / Failed / Total] = [%d / %d / %d]\n",
    __passed_asserts, __failed_asserts, __asserts);
}

#endif
