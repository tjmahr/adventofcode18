#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// // [[Rcpp::export]]
// IntegerVector wrap_around_c(IntegerVector xs, IntegerVector y) {
//   int length_y = y.length();
//   int length_xs = xs.length();
//
//   xs = ifelse(xs <= 0, xs + 1, xs);
//   xs = xs - 1;
//
//   for(int i = 0; i < length_xs; ++i) {
//     // add divisor to remainder and go again to handle negative numbers
//     xs[i] = ((xs[i] % length_y) + length_y) % length_y;
//   }
//
//   return xs + 1;
// }

//' @export
// [[Rcpp::export]]
int wrap_around_c2(int x, IntegerVector y) {
  int length_y = y.length();

  if (x <= 0) {
    x = x + 1;
  }
  x = x - 1;
  x = ((x % length_y) + length_y) % length_y;
  x = x + 1;
  return x;
}

//' @export
// [[Rcpp::export]]
IntegerVector run_marbles_c_scores(int x, int marbles) {
  IntegerVector board(1);
  int game_length = marbles / 23;
  IntegerVector history(game_length);

  int position = 0;
  int current_position = 1;
  int next_step;
  int to_remove;
  int bonus;

  for (int i = 1; i <= marbles; ++i) {
    if (i % 23 == 0) {
      to_remove = wrap_around_c2(current_position - 7, board);
      bonus = board[to_remove - 1];
      board.erase(to_remove - 1);
      position = wrap_around_c2(current_position - 7, board);
      history[(i / 23) - 1] = i + bonus;
    } else {
      next_step = current_position + 2;
      position = wrap_around_c2(next_step, board);
      board.insert(position - 1, i);
    }

    current_position = position;
  }

  return history;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//
/*** R
run_marbles_c_scores(10, 300)

wrap_around_c2(1, 0:1)
wrap_around_c2(2, 0:1)
wrap_around_c2(3, 0:1)
*/
