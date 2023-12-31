for fname; do
  flags=$(sort -k3,3n -k4,4n -k5,5n -t';' -n $fname | cut -f2 -d';')
  printf "%s " $flags | fold -w 70
  printf "\n\n"
done
