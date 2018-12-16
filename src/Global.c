
#include <purescript.h>

PURS_FFI_FUNC_1(Global_readFloat, x, {
  double xx = atof(purs_any_get_string(x));
  return purs_any_num_new(xx);
});

PURS_FFI_FUNC_1(Global_isFinite, x, {
  double xx = purs_any_get_num(x);
  return purs_any_bool(isfinite(xx));
});

