#include <purescript.h>

/* PURS_FFI_FUNC_1(Prelude_Extended_fromCharArrayImpl, x, { */
/*       return purs_any_string_new( */
/*       afmt("\"%s\"", purs_any_get_string(x))); */
/* }); */

PURS_FFI_FUNC_1(Prelude_Extended_showCharImpl, x, {
  utf8_int32_t chr = purs_any_get_char(x);
  char * s = 0;
  size_t bytes = utf8codepointsize(chr);
  s = (char *) malloc(bytes + 1);
  utf8catcodepoint(s, chr, bytes);
  s[bytes + 1] = '\0';
  const purs_any_t * out = purs_any_string_new(afmt("%s", s));
  free(s);
  return out;
});


