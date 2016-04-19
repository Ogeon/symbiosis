RED='\033[0;31m'
GREEN='\033[0;32m'
DEFAULT='\033[0m'

cargo run

for dir in tests/*; do
	echo "checking '$dir'"

	printf " - Rust dynamic: "
	diff=`diff -N "$dir/expected.html" "$dir/dynamic.html"`
	if [ $? -eq 0 ]; then
		echo -e "${GREEN}ok${DEFAULT}"
	else
		echo -e "${RED}failed${DEFAULT}\n$diff\n"
	fi

	printf " - Rust static: "
	diff=`diff -N "$dir/expected.html" "$dir/static.html"`
	if [ $? -eq 0 ]; then
		echo -e "${GREEN}ok${DEFAULT}"
	else
		echo -e "${RED}failed${DEFAULT}\n$diff\n"
	fi

	printf " - JavaScript: "
	phantomjs "$dir/main.js"
	diff=`diff -N "$dir/expected.html" "$dir/javascript.html"`
	if [ $? -eq 0 ]; then
		echo -e "${GREEN}ok${DEFAULT}"
	else
		echo -e "${RED}failed${DEFAULT}\n$diff\n"
	fi
done
