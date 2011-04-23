Skapong, a boring ocaml pong
----------------------------

A small "write a fucking game" project for the bugpipe.com, my
half-dead weblog,

    a/z     - move first player,
    up/down - move second player
    space   - restart
    1       - toggle first player player/computer
    2       - toggle second player player/computer
    Esc     - quit


To build you'll need OCaml (obviously) and my fork of glcaml:

    git clone git://github.com/einars/skapong.git
    git clone git://github.com/einars/glcaml.git
    cd skapong
    ln -s ../glcaml/lib
    make
    ./skapong

Background picture of the clouds - a desaturized image by Skyblue2u of
colourlovers.com,
http://www.colourlovers.com/pattern/101050/powdered_sugar_cloud
