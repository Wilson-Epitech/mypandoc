##
## MYPANDOC PROJECT, 2024
## day2
## File description:
## Makefile
##

STACK	?=	stack

BIN_PATH	=	$(STACK) path --local-install-root

RM		?=	rm -f

NAME	=	mypandoc

all:	$(NAME)

$(NAME):
	$(STACK) build && cp `$(BIN_PATH)`/bin/$(NAME)-exe ./$(NAME)

clean:
	$(STACK) clean

fclean:	clean
	$(RM) $(NAME)
	$(STACK) purge

re:	fclean all

tests_run:	all
	$(STACK) test

.PHONY: all clean fclean re tests_run
