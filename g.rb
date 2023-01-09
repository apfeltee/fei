
words=<<__eos__
    OBJ_BOUND_METHOD,
    OBJ_INSTANCE,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE
__eos__

table =
{

}

sp = (" " * 4) * 2

begin
  files = Dir.glob("*.{h,c}")
  if table.empty? then
    words.strip.split(",").map(&:strip).reject(&:empty?).each do |w|
      printf("%scase %s: return %p;\n", " "*8, w, w.gsub(/^OBJ_/i, "").downcase)
    end
  else
  
  end
end
