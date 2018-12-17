#include <purescript.h>

PURS_FFI_FUNC_1(Effect_Random_random, _, {
  // foreign import random :: Number -> Effect Number
  //
  double x = (double) rand() / (double) RAND_MAX;
  return purs_any_num_new(x);
});
