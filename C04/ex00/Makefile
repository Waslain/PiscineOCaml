NAME = a.out

SOURCES = micronap.ml

LIBS = unix.cma

#les compilateurs
CAMLC = ocamlc
CAMLOPT = ocamlopt.opt
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
		$(CAMLOPT) -o $(NAME) $(LIBS:.cma=.cmxa) $(OPTOBJS)

#Compilation
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

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
	rm -f $(SOURCES:.ml=.o)

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

#Rèle de création de dépendances a partir des sources
.depend: $(SOURCES)
	$(CAMLDEP) $(SOURCES) > .depend

depend: .depend

run: opt clean
re: fclean all

include .depend
