
cproto "$@" | perl -pe 's/\b_Bool\b/bool/g'

