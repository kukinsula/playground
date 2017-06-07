#include "logger.h"

int main() {
  /* colorf */
  colorf(RED, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  colorf(GREEN, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  colorf(YELLOW, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  colorf(BLUE, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  colorf(MAGENTA, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  colorf(CYAN, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  colorf(WHITE, "int %d, char* %s, float %f\n", 42, "azerty", 3.14159);

  /* colorf shortcuts */
  cyanf("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  magentaf("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  bluef("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  yellowf("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  greenf("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);
  redf("int %d, char* %s, float %f\n", 42, "azerty", 3.14159);

  /* logf */
  logf(SUCCESS, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  logf(INFO, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  logf(DEBUG, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  logf(WARNING, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  logf(ERROR, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  logf(FATAL, "logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");

  /* logf shortcuts */
  errorf("logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  warningf("logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  debugf("logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  infof("logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");
  successf("logf int %d float %f char* %s\n", 42, 3.14159, "qwerty");

  /* perror_exit */
  fperror_exitf(stdout, "perror_exit's message:\n\tint %d float %f char* %s\n",
    42, 3.14159, "qwerty");

  return 0;
}
