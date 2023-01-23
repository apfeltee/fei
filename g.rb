words=<<__eos__
    TOKEN_LEFTPAREN,
    TOKEN_RIGHTPAREN,
    TOKEN_LEFTBRACE,
    TOKEN_RIGHTBRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    TOKEN_MODULO,
    TOKEN_SHIFTLEFT,
    TOKEN_SHIFTRIGHT,
    TOKEN_BITXOR,
    TOKEN_BITNOT,
    TOKEN_BITOR,
    TOKEN_BITAND,
    TOKEN_LOGICALNOT,
    TOKEN_NOTEQUAL,
    TOKEN_ASSIGN,
    TOKEN_EQUAL,
    TOKEN_GREATERTHAN,
    TOKEN_GREATEREQUAL,
    TOKEN_LESSTHAN,
    TOKEN_LESSEQUAL,
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    TOKEN_KWAND,
    TOKEN_KWCLASS,
    TOKEN_KWELF,
    TOKEN_KWELSE,
    TOKEN_KWFALSE,
    TOKEN_KWFOR,
    TOKEN_KWFUN,
    TOKEN_KWIF,
    TOKEN_KWNULL,
    TOKEN_KWOR,
    TOKEN_KWPRINT,
    TOKEN_KWRETURN,
    TOKEN_KWSUPER,
    TOKEN_KWSWITCH,
    TOKEN_KWDEFAULT,
    TOKEN_KWCASE,
    TOKEN_KWTHIS,
    TOKEN_KWTRUE,
    TOKEN_KWVAR,
    TOKEN_KWWHILE,
    TOKEN_KWBREAK,
    TOKEN_KWCONTINUE,
    TOKEN_KWDO,
    TOKEN_KWREPEAT,
    TOKEN_KWUNTIL,
    TOKEN_KWFROM,
    TOKEN_ERROR,
    TOKEN_EOF
__eos__
table =
{
}
sp = (" " * 4) * 2
begin
  files = Dir.glob("*.{h,c}")
  if table.empty? then
    words.strip.split(",").map(&:strip).reject(&:empty?).each do |w|
      printf("%scase %s: return %p;\n", " "*8, w, w.gsub(/^TOKEN_/i, "").downcase)
    end
  else
  end
end
