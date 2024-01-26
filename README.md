# factorio-blueprints

Converts images into pixel-art blueprints for the game [Factorio][factorio].

Table of Contents:

- [Example](#example)
- [Output](#output)
- [Executable](#executable)
- [Blueprint Reference](#blueprint-reference)
- [Build / Develop](#build--develop)

## Example

Input image (taken by the James Webb Space Telescope, found [here][jw-link],
and used as per their [usage policy][jw-usage]):

![Input Image](data/original.png)

Command to run:

```sh
$ cabal run blueprints --         \
    --image data/original.png     \
    --height 200                  \
    --set tile-dect               \
    --dither atkin                \
    --preview blueprint.png       \
    --output str > blueprint.txt
```

Output blueprint, shown in-game (on the minimap):

![Output Image - Minimap (Dectorio)](data/ingame-minimap.png)

And here is how it looks like in the world, next to the player - here you
can see the individual 'pixels' in the output:

![Output Image - In Game (Dectorio)](data/ingame-player.png)

As you may guess, this particular combination of input image and tileset
(`--set tile-dect`, the [Dectorio][dectorio] tileset) happen to work
rather well together. This is because the colour palette of the original
image was well-covered by the specified tileset.

Results may not be so good when the tileset we are using is limited or is
missing any colours close to important ones that are present in large
amounts - there is a fundamental limit on how good our approximations can
be with limited and fixed colours available.

Dithering helps a lot to interpolate colours and avoid colour banding (at the
cost of adding some noise). The above image used Atkinson dithering
with `--dither atkin`, if we omit that option to turn all dithering off it
looks like this instead (notice the colour-banding in the light blues):

![Output Image (Dectorio)](data/out-dect-no-dither.png)

For another comparison, here is the same image run with dithering and using
the vanilla/base-game tileset `--set all-base`, which is (roughly) monochrome:

![Output Image (Base)](data/out-base.png)

And finally here is another tileset  `--set all-kras` using the extra tiles
from [Krastorio][krastorio], which makes for another (roughly) monochrome
tileset with better lowlights and an extra midtone:

![Output Image (Krastorio)](data/out-kras.png)

## Output

Continuing on with the same example as above, here is a snippet of the
output string  after encoding, as would be suitable for copy-pasting in-game.
This can be obtained by adding `--output str` on the command line, and is what
most users will want to simply get the output in-game.

```
0eNqkvduyHbexbfsvfl6KKCQSt/UrJ/YDlzxlMY5FKijKOvaK...
```

Additionally here is a snippet of the blueprint JSON. This is how the blueprint
is structured internally. This output can be similarly obtained by adding
`--output json` as an option on the command line, which will then print this
JSON to stdout. (This should not usually be needed and is provided mainly for
debugging.)

```json
{
    "blueprint": {
        "item": "blueprint",
        "entities": [],
        "tiles": [
            {
                "name": "black-refined-concrete",
                "position": {
                    "x": 0,
                    "y": 0
                }
            },
            ...
        ]
    }
}
```

## Executable

We provide an executable with a user-friendly interface, as shown below.
It can be invoked by running `cabal run blueprints` after downloading
this repository, and options can be provided after a double dash `--` as
shown in the [Example](#example) section above. The options in parentheses
`(...)` are required, while those in square brackets `[...]` are optional.

```
Usage:
  blueprints (-i|--image FILE)
             [-p|--preview FILE]
             [--set SET]
             [-o|--output FORMAT]
             [-d|--dither METHOD]
             [--width INT | --height INT | --scale FLOAT]

  Generate a Factorio Blueprint from a given image

Available options:
  -h,--help                Show this help text
  -i,--image FILE          the input image to use
  -p,--preview FILE        output a preview of the blueprint to the given file
  --set SET                the tileset/palette to use - should be one of
                           {tile-base, tile-kras, tile-dect, all-base, all-kras}
  -o,--output FORMAT       print the blueprint in the given format - one of
                           {str, json}
  -d,--dither METHOD       how (if at all) to dither the preview image - one of
                           {fs, mae, atkin, none} (default: none)
  --width INT              target width (in pixels) to resize the image to
  --height INT             target height (in pixels) to resize the image to
  --scale FLOAT            ratio to scale the image to - scale=1 means preserve
                           size, scale=0.5 means half scale, etc.
```

## Blueprint Reference

How the blueprint format works in all technical details is explained
[here][wiki] on the official game wiki page.

## Build / Develop

### Standard Method

To build the project, simply clone the repo and:

- Run `cabal build` to build the whole project. (Expect this to take a bit of
  time on first use, due to building the project dependencies.)
- Run `cabal repl` to jump into a REPL where you can experiment and
  interactively play with the codebase.
- Run `cabal run blueprints -- ...` as a convenient shorthand
  to build (if necessary) and then run the executable as described in
  [this](#executable) section.

Note that this project is set up for ghc v9.4.7.

### With Nix

A `flake.nix` is provided to build the project and handle all dependencies with
nix. To use this, run `nix develop`
(or `nix develop --command <your-preferred-shell>`) to enter a 'virtual
environment' / 'sub-shell' with ghc, cabal, HLS, etc. of appropriate versions
all available. From there, all the [above](#standard-method) cabal commands can
be used.

Unfortunately, due to what appears to be a bug somewhere in the haskell nix
infrastructure, the project cannot be directly built with `nix build` or run
with e.g. `nix run github:lawbel/factorio-blueprints`. This seems to be due to
using 'data files' at compile time via Template Haskell. (We do this to handle
generating a very large data type and associated functions which would be
tedious and error prone to write out by hand.) No workaround has been found at
time of writing to fix this issue.

[jw-link]: https://webbtelescope.org/contents/media/images/2022/031/01G77PKB8NKR7S8Z6HBXMYATGJ
[jw-usage]: https://webbtelescope.org/copyright
[wiki]: https://wiki.factorio.com/Blueprint_string_format
[dectorio]: https://mods.factorio.com/mod/Dectorio
[krastorio]: https://mods.factorio.com/mod/Krastorio2
[factorio]: https://factorio.com/
