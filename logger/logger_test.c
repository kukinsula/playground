#include "logger.h"

int main() {
  /* colorf */
  colorf(RED, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(GREEN, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(YELLOW, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(BLUE, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(MAGENTA, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(CYAN, "int %d, char* %s, float %f", 42, "azerty", 3.14159);
  colorf(WHITE, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);

  /* colorf shortcuts */
  redf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  greenf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  yellowf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  bluef("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  magentaf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  cyanf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
  whitef("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);

  /* logf */
  logf(Success, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Info, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Debug, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Warning, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Error, "logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  logf(Fatal, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");

  /* logf shortcuts */
  log_successf("logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  log_infof("logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  log_debugf("logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  log_warningf("logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  log_errorf("logf int %d float %f char* %s", 42, 3.14159, "qwerty");
  log_fatalf("logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");

  /* exit */
  error_exitf("int %d, char* %s, float %f", 42, "azerty", 3.14159);
}
