#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct Options {
  bool create;
  bool remove;
  bool pretend;
  bool overwrite;
  bool info;
};

static struct option long_options[] = {
    //NAME    ARGUMENT      FLAG  SHORTNAME */
    {"to", required_argument, NULL, 't'},
    {"from", required_argument, NULL, 'f'},
    {"create", no_argument, NULL, 'c'},
    {"remove", no_argument, NULL, 'r'},
    {"pretend", no_argument, NULL, 'p'},
    {"overwrite", no_argument, NULL, 'o'},
    {"info", no_argument, NULL, 'i'},
    {NULL, 0, NULL, 0}};

struct Options options;

void option_parser(int argc, char **argv);

int main(int argc, char **argv) {
  option_parser(argc, argv);

  if (options.create) {
    printf("creating");
  }

  if (options.remove) {
    printf("removing");
  }

  if (options.pretend) {
    printf("dry-run");
  }

  if (options.overwrite) {
    printf("over writing");
  }

  if (options.info) {
    printf("info: ");
  }

  exit(0);
}

void option_parser(int argc, char **argv) {
  int c;

  // getopt_long stores the option index here.
  int option_index = 0;

  while (true) {
    c = getopt_long(argc, argv, "crpoht:f:", long_options, &option_index);

    if (c == -1)
      break;

    switch (c) {
    case 'h':
      printf("option %s", long_options[option_index].name);
      if (optarg) {
        printf(" with arg %s", optarg);
      }
      printf("\n");
      exit(0);

      break;
    case 'c':
      options.create = true;

      break;

    case 'r':
      options.remove = true;

      break;

    case 'p':
      options.pretend = true;

      break;

    case 'o':
      options.overwrite = true;

      break;

    case 'i':
      options.info = true;
      break;

    default:
      printf("errado, mae da foca!");
      abort();
    }
  }

  if (optind < argc) {
    printf("non-option ARGV-elements: ");
    while (optind < argc) {
      printf("%s ", argv[optind++]);
    }
    printf("\n");
  }
}
