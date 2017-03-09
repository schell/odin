odin
====
Here I'm exploring building a roguelike and 2d engine.

batteries included
------------------
* [odin-engine](https://github.com/schell/odin/tree/master/odin-engine) is a high
  level 2d game engine. It provides a novel effects system built on top of
  [freer](https://gitlab.com/queertypes/freer), as well as an immediate mode gui
  with a number of [preexisting widgets](https://github.com/schell/odin/tree/master/odin-engine/src/Odin/Engine/GUI).

* [gelatin-sdl2](https://github.com/schell/gelatin/tree/master/gelatin-sdl2)
  is the OpenGL/SDL2 rendering backend.

* [varying](https://github.com/schell/varying) provides smooth FRP-based
  animation.

* [shapes](https://github.com/ublubu/shapes) provides a purely function 2d
  collision detection and physics system.

* [mapmaker](https://github.com/schell/odin/blob/master/app/MapMaker.hs) is an
  example application written with `odin-engine`.

installation
------------
The easiest way to get up and running on the Haskell toolchain is to download
[stack](https://docs.haskellstack.org/en/stable/README/). All of odin's projects
are spec'd out with stack.yaml build files, so picking the correct versions of
libraries is not needed if you follow the stack path.

Download [sdl2](http://libsdl.org/download-2.0.php), following your distro's
instructions.

Then...

    git clone https://github.com/schell/odin.git

    cd odin

If you just installed stack, run

    stack setup

Go make some ☕ and then...

    stack build mapMaker

Drink your ☕, take a walk and then run...

    stack exec mapMaker

Please follow the respective guide for installing sdl2 on your platform below.

### Mac OS X
Simply download the `dmg` from [sdl2](http://libsdl.org/download-2.0.php) and
follow the instructions.

### Ubuntu
To install sdl2 use something like

    apt-get install libsdl2-dev

### Windows
Coming soon

contributions
-------------
I welcome any and all contributions. This means bug reports, pull requests or
even [donations through patreon](https://www.patreon.com/schell).
