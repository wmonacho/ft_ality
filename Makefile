.PHONY: all clean

all: build
	cp ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/ft-ality-0.1.0.0/x/ft-ality/build/ft-ality/ft-ality ft_ality

build:
	cabal build

clean:
	cabal clean
	rm ft_ality