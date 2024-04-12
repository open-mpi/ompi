
for ((i=0; i < 1; i++)); do
	prte --no-ready-msg --report-uri foo$i.txt &
	prun --map-by ppr:1:node --dvm-uri file:foo$i.txt --wait-to-connect 2 /Users/rhc/pmix/prrte2/examples/client2
	pterm --dvm-uri file:foo$i.txt
done
