LOGIN=xmocar00
COMPILER=ghc
FLAGS=-Wall --make
NAME=flp22-fun
MAIN=Main.hs

build:
	cd ./src; $(COMPILER) $(FLAGS) ./*.hs -o $(NAME)
	mv ./src/$(NAME) .

clean:
	rm ./src/*.hi ./src/*.o
	rm ./$(NAME)

zip:
	zip -r flp-fun-$(LOGIN).zip ./src Makefile
