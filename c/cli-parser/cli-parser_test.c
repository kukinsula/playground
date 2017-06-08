#include "cli-parser.h"

int main(int argc, char** argv) {
  Option* options[1];
  int i;

  options[0]->name = "--test";
  options[0]->description = "This is a test !";

  cli_parse(argc, argv, options, 1);

  for (i = 0; i < 1; ++i) {
    printf("options[i]->value = %s\n", options[i]->value);
  }
}
