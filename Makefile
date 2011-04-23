FILES = skapong.ml
RESULT  = skapong

LIBDIR = lib
LIBS = graphics unix bigarray

WIN_SOURCE=$(LIBDIR)/win_stub.c $(LIBDIR)/win.mli $(LIBDIR)/win.ml
SDL_SOURCE=$(LIBDIR)/sdl_stub.c $(LIBDIR)/sdl.mli $(LIBDIR)/sdl.ml
GL_SOURCE=$(LIBDIR)/glcaml_stub.c $(LIBDIR)/glcaml.mli $(LIBDIR)/glcaml.ml

CLIBS = SDL GL

SOURCES = ${WIN_SOURCE} ${SDL_SOURCE} ${GL_SOURCE} ${FILES}


all: byte-code
include OCamlMakefile

