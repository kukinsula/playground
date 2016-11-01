#ifndef __CLI_PARSER__
#define __CLI_PARSER__

#include <stdio.h>
#include <string.h>

typedef struct {
  char *value, *name, *description;
} Option;

typedef Option* Options;

void new_options(int len);

/* #define string_var(str, option, description) 1; */

void cli_parse(int argc, char** argv, Option* option[], int options_len);

#endif
