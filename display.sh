countries="$@"
args=$(eval echo flags/{${countries// /,}}.png)
open $args
