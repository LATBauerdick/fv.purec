#include <purescript.h>

PURS_FFI_FUNC_1(Data_String_CodeUnits_length, x, {
    return purs_any_int_new(strlen( purs_any_get_string(x)));
});

PURS_FFI_FUNC_1(Data_String_CodeUnits_fromCharArray, xs, {
  const purs_vec_t * zs = purs_any_get_array(xs);
  const purs_any_t * tmp;
  int i;
  const int mxbytes = 4; // ???? I have no idea...
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

PURS_FFI_FUNC_1(Data_String_CodeUnits_toCharArray, s, {
  const char * ts = purs_any_get_string(s);
  const purs_any_int_t count = strlen(ts);
  purs_vec_t * result = (purs_vec_t *) purs_vec_new();
  for (purs_any_int_t i = 0; i < count; i++) {
    char c = ts[i];
    purs_vec_push_mut(result, purs_any_char_new(c));
  }
  return purs_any_array_new(result);
});

PURS_FFI_FUNC_1(Data_String_CodeUnits_singleton, c, {
  const int mxbytes = 4; // ???? I have no idea...
  char * s = (char *) malloc(mxbytes + 1);
  utf8_int32_t chr = purs_any_get_char(c);
  size_t bytes = utf8codepointsize(chr);
  utf8catcodepoint(s, chr, bytes);
  s[bytes + 1] = '\0';
  return purs_any_string_new(s);
});

PURS_FFI_FUNC_2(Data_String_CodeUnits_drop, n0, s0, { //??? does not work with unicode chars
  size_t n = purs_any_get_int(n0);
  if (n <= 0) return s0; //???? not sure this is ok re/ memory allocation etc?
  const char * s = purs_any_get_string(s0);
  size_t sl = strlen(s);
  if (n >= sl) return purs_any_string_new("");
  size_t srl = sl-n;
  char * sr = (char *) malloc(srl + 1);
  strlcpy(sr, s+n, srl+1); // strlcpy makes sure there's a null character at the end
  const purs_any_t * out = purs_any_string_new(sr);
  free(sr);
  return out;
});

PURS_FFI_FUNC_2(Data_String_CodeUnits_take, n0, s0, { //??? does not work with unicode chars
  size_t n = purs_any_get_int(n0);
  if (n <= 0) return purs_any_string_new("");
  const char * s = purs_any_get_string(s0);
  size_t sl = strlen(s);
  if (n >= sl) n = sl;
  char * sr = (char *) malloc(n + 1);
  strlcpy(sr, s, n+1); // strlcpy makes sure there's a null character at the end
  const purs_any_t * out = purs_any_string_new(sr);
  free(sr);
  return out;
});

PURS_FFI_FUNC_2(Data_String_CodeUnits_countPrefix, f, s0, { // only works with ASCII...
 // foreign import countPrefix :: (Char -> Boolean) ->  String -> Int
  const char * s = purs_any_get_string(s0);
  int i = 0;
  while ( i<strlen(s) && purs_any_is_true(purs_any_app(f, purs_any_char_new(s[i])))) {
    i++;
  }
  return purs_any_int_new(i);
});

PURS_FFI_FUNC_4(Data_String_CodeUnits__indexOf, just, nothing, p0, s0, { // only works with ASCII...
// foreign import _indexOf :: (forall a. a -> Maybe a) -> (forall a. Maybe a) -> Pattern -> String -> Maybe Int
  const char * p = purs_any_get_string(p0);
  const char * s = purs_any_get_string(s0);
  const char * found = strstr(s, p);
  if (!found) return nothing;
  return purs_any_app(just, purs_any_int_new(found-s));
});

/* PURS_FFI_FUNC_1(Data_Show_showNumberImpl, x, { */
/*   static char str[32]; */
/*   sprintf(str, "%.16g", purs_any_get_num(x)); */
/*   return purs_any_string_new(strchr(str, '.') || strchr(str, 'e') ? str : strcat(str, ".0")); //this is not locale clean */
/* }); */

/* size_t mbstowcs(schar_t *pwcs, const char *str, size_t n) */

/* Converts the string of multibyte characters pointed to by the argument str */ 
/* to the array pointed to by pwcs. */
