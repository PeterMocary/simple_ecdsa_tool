LOGIN=xmocar00
COMPILER=stack ghc --
FLAGS=-Wall --make
NAME=flp22-fun
MAIN=Main.hs

build:
	cd ./src; $(COMPILER) $(FLAGS) ./$(MAIN) -o $(NAME)
	mv ./src/$(NAME) .

clean:
	rm ./src/*.hi ./src/*.o
	rm ./$(NAME)

zip:
	zip -r $(NAME)-$(LOGIN).zip .
