#include <purescript.h>

PURS_FFI_FUNC_3(Data_String_Common_replace, s1, s2, s3, {
  const char * s = purs_any_get_string(s3);
  const char * sf = purs_any_get_string(s1);
  const char * found = strstr(s, sf);
  if (!found) return purs_any_string_new(s);
  char * sh = (char *) malloc(found-s + 1);
  strncpy(sh, s, found-s);
  sh[found-s] = '\0';
  const char * sr = purs_any_get_string(s2);
  const char * st = found + strlen(sf);
  const purs_any_t * out = purs_any_string_new(afmt("%s%s%s", sh, sr, st));
  free(sh);
  return out;
});
