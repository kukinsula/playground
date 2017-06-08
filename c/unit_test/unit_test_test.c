#include "unit_test.h"

void test_a();

int main() {
  INIT_TEST();

  test_a();

  EXIT_TEST();
}

void test_a() {
  ASSERT_TRUE(1);
  ASSERT_TRUE(1);
}
