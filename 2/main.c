#include "main.h"

/* there is a memory leak in this program. I don't give a shit. Fuck C forever
 * and ever amen */

/* TODO generalize */
char **get_lines(char *filename, size_t *length) {
  FILE *fp = fopen(filename, "r");
  char *line = NULL;
  size_t len = 0;
  ssize_t read;

  if (fp == NULL) {
    exit(1);
  }

  char *c;
  size_t lines_size = 0;
  size_t nbytes;

  while ((nbytes = getline(&c, &lines_size, fp)) != -1) {
    len++;
  }

  char **lines = malloc(len * sizeof(char *));
  size_t char_count = 0;

  fclose(fp);
  fp = fopen(filename, "r");

  if (fp == NULL) {
    exit(1);
  }

  size_t getline_len;

  while ((read = getline(&line, &getline_len, fp)) != -1) {
    lines[char_count] = strdup(line);
    if (lines[char_count][getline_len - 1] == '\n') {
      lines[char_count][getline_len - 1] = '\0';
    }

    char_count++;
  }

  free(line);
  fclose(fp);

  *length = (size_t)len;

  return lines;
}

enum left_strat ch_to_left_strat(char ch) {
  switch (ch) {
  case 'A':
    return A;
  case 'B':
    return B;
  case 'C':
    return C;
  }

  return 0;
}

enum right_strat ch_to_right_strat(char ch) {
  switch (ch) {
  case 'X':
    return X;
  case 'Y':
    return Y;
  case 'Z':
    return Z;
  }

  return 0;
}

Strategy *get_strategies(char **file_contents, size_t file_length) {
  Strategy *strategies = malloc(sizeof(Strategy) * file_length);

  for (int i = 0; i < file_length; i++) {
    char left_char = file_contents[i][0];
    char right_char = file_contents[i][2];

    Strategy strat = {ch_to_left_strat(left_char),
                      ch_to_right_strat(right_char)};

    strategies[i] = strat;
  }

  return strategies;
}

enum outcome get_outcome(enum left_strat left, enum right_strat right) {
  /* there's a better way to do this, right? */
  switch (right) {
  case X:
    switch (left) {
    case A:
      return DRAW;
    case B:
      return LOSE;
    case C:
      return WIN;
    }
  case Y:
    switch (left) {
    case A:
      return WIN;
    case B:
      return DRAW;
    case C:
      return LOSE;
    }
    break;
  case Z:
    switch (left) {
    case A:
      return LOSE;
    case B:
      return WIN;
    case C:
      return DRAW;
    }
  }
}

int get_score_for_strat(Strategy strat) {
  enum outcome result = get_outcome(strat.left, strat.right);

  return result + strat.right;
}

int part_2_get_other_strat(enum left_strat left, enum right_strat right) {
  switch (right) {
  case X:
    switch (left) {
    case A:
      return C;
    case B:
      return A;
    case C:
      return B;
    }
  case Y:
    return left;
  case Z:
    switch (left) {
    case A:
      return B;
    case B:
      return C;
    case C:
      return A;
    }
  }
}

int part_2_get_score_for_strat(Strategy strat) {
  int other_strat = part_2_get_other_strat(strat.left, strat.right);
  switch (strat.right) {
  case X:
    return LOSE + other_strat;
  case Y:
    return DRAW + other_strat;
  case Z:
    return WIN + other_strat;
  }
}

int main(int argc, char *argv[]) {
  if (argc > 1) {
    char *path = argv[1];
    size_t filesize;
    char **contents = get_lines(path, &filesize);

    Strategy *strategies = get_strategies(contents, filesize);

    int part_1_score = 0;
    int part_2_score = 0;

    for (int i = 0; i < filesize; i++) {
      Strategy strat = strategies[i];
      int score = get_score_for_strat(strat);
      part_1_score += score;

      int score_2 = part_2_get_score_for_strat(strat);
      part_2_score += score_2;
    }

    printf("part 1 score: %d\n", part_1_score);
    printf("part 2 score: %d\n", part_2_score);

    free(contents);
    free(strategies);

    return 0;
  }

  printf("expected file path");
  return 1;
}
