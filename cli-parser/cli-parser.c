#include "cli-parser.h"

Options* new_options(int len) {
  options = malloc(sizeof(Option) * len);
  if (options == NULL) {
    return NULL
  }

  return options;
}

void cli_parse(int argc, char** argv, Option* options[], int options_len) {
  int i, j;

  for (i = 1; i < argc; ++i) {
    printf("argv[%d] = %s\n", i, argv[i]);

    for (j = 0; j < options_len; ++j) {
      if (strncmp(argv[i], options[j]->name, strlen(argv[i])) == 0) {
	printf("strncmp found !\n");

	if (i == argc - 1) {
	  printf("Error: %s expected an argument\n", argv[i]);
	  return ;
	}

	options[j]->value = argv[i + 1];
      }
    }
  }
}
