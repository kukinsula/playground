#include "unit_test.h"

int main() {
  INIT_TEST();

  ASSERT_TRUE(1);
  ASSERT_FALSE(0);

  PRINT_TESTS_RESULT();
}
