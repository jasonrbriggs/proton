all: clean compile

clean:
	rm -f dist/proton


compile:
	nimble build
	
test:
	nim c -p:. -r tests/basic_test