for fname; do
  flags=$(sort -k3 -t';' -n $fname | cut -f2 -d';')
  printf "%s " $flags | fold -w 70
done
