#include "Math.h"

#include <purescript.h>

PURS_FFI_VALUE(Global_nan, PURS_ANY_NUM(NAN));

PURS_FFI_FUNC_1(Global_isNaN, x, {
  return purs_any_bool(isnan(purs_any_get_num(x)));
});

PURS_FFI_VALUE(Global_infinity, PURS_ANY_NUM(INFINITY));

PURS_FFI_FUNC_1(Global_isFinite, x, {
  return purs_any_bool(isfinite(purs_any_get_num(x)));
});

PURS_FFI_FUNC_2(Global_readInt, r, s, {
  return purs_any_num_new(strtol(purs_any_get_string(s), NULL, purs_any_get_int(r)));
});

PURS_FFI_FUNC_1(Global_readFloat, x_, {
  double x = atof(purs_any_get_string(x_));
  return purs_any_num_new(x);
});

