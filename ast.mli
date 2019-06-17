type regex_t =
| Empty
| Char of char
| Concat of regex_t * regex_t
| Choice of regex_t * regex_t
| Star of regex_t
