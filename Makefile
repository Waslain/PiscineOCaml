NAME = exec

SOURCES = 

#les compilateurs
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

#Règles
all: depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

#Règle spéciale pour générer la version optimisée et la version byte code du programme
opt: $(NAME).opt
byt: $(NAME).byt

#Les listes d'objets en fonction du type de compilation
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

#Linking
$(NAME).byt:	$(OBJS)
		$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

#Name linking optimisée
$(NAME).opt:	$(OPTOBJS)
		$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

#Compilation
.SUFFIIXES:
.SUFFIIXES: .ml .mli .cmo .cmi .cmx

#Règle specifique de compilation des fichiers sources en objet
.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

######## Nettoyage
clean:
	rm -f *.cm[iox] *~ .*~
	rm -f $(NAME).o

fclean:
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

#Rèle de création de dépendances a partir des sources
depend: .depend
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend