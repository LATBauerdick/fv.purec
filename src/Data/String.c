#include <purescript.h>

PURS_FFI_FUNC_1(Data_String_lengthImpl, x, {
    return purs_any_int_new(strlen( purs_any_get_string(x)));
});

PURS_FFI_FUNC_1(Data_String_fromCharArrayImpl, xs, {
  const purs_vec_t * zs = purs_any_get_array(xs);
  const purs_any_t * tmp;
  int i;
  int mxbytes = 16;
  char * out = (char *) malloc(mxbytes*(zs->length) + 1);
  out[0] = '\0';
  char * s = (char *) malloc(mxbytes + 1);
  purs_vec_foreach(zs, tmp, i) {
    utf8_int32_t chr = purs_any_get_char(tmp);
    size_t bytes = utf8codepointsize(chr);
    utf8catcodepoint(s, chr, bytes);
    s[bytes + 1] = '\0';
    strcat(out, s);
  }
  free(s);
  return purs_any_string_new(out);
});

