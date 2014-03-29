make:
	ghc -isrc --make src/Main.hs
	mv src/Main hs-vm

clean:
	find src -type f -name "*.hi" -exec rm -f {} \;
	find src -type f -name "*.o" -exec rm -f {} \;

clean_all:
	find src -type f -name "*.hi" -exec rm -f {} \;
	find src -type f -name "*.o" -exec rm -f {} \;
	rm hs-vm
