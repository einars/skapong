FILES  = checked.ml common.ml bff.ml spaudio.ml debug.ml skapong.ml
LIBDIR = lib
LIBS   = graphics unix bigarray

WIN_SOURCE = $(LIBDIR)/win_stub.c $(LIBDIR)/win.mli $(LIBDIR)/win.ml
SDL_SOURCE = $(LIBDIR)/sdl_mixer_stub.c $(LIBDIR)/sdl_stub.c $(LIBDIR)/sdl.mli $(LIBDIR)/sdl.ml  $(LIBDIR)/sdl_mixer.ml
GL_SOURCE  = $(LIBDIR)/glcaml_stub.c $(LIBDIR)/glcaml.mli $(LIBDIR)/glcaml.ml
SOURCES = ${WIN_SOURCE} ${SDL_SOURCE} ${GL_SOURCE} ${FILES}
RESULT  = skapong

ifdef SYSTEMROOT
	CLIBS   = SDL opengl32 gdi32
	WIN32   = 1
	LIBDIRS = /usr/lib/mingw-libs
	TARGET  = native-code
	POST    = mkwinapp/mkwinapp.exe skapong.exe
else
	CLIBS   = SDL SDL_mixer GL
	TARGET  = debug-code
	POST    =
endif

all:$(TARGET)
	$(POST)

run:$(TARGET)
	OCAMLRUNPARAM=b ./skapong


-include OCamlMakefile
