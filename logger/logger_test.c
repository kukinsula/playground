#include "logger.h"

int main() {
  colorf(RED, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(GREEN, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(YELLOW, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(BLUE, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(MAGENTA, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(CYAN, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(WHITE, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);

  errorf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  successf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  infof("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);

  logf(Success, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Info, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Debug, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Warning, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Error, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Fatal, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");

  error_exitf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
}
