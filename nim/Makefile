all: clean compile

clean:
	rm -f dist/proton
	rm -rf src/nimcache

compile:
	nimble install

test:
	nim c -p:. -r tests/basic_test
