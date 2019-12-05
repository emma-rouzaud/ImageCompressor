NAME	=	imageCompressor

SRC	=	app/Main.hs

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .
	mv debruijn-exe imageCompressor

clean:
	stack clean
	rm -rf .stack-work debruijn.cabal

fclean:	clean
	$(RM) $(NAME)

re:	fclean all
