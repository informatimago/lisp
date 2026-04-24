# draw-random

*A small Common Lisp program that generates abstract pictures.*

`draw-random` draws a handful of random lines on a blank canvas. The
lines partition the canvas into areas. Each area is then filled with
random dots whose density decreases along a random direction chosen
independently for that area. The result is written as a PNG file.

![Example output: black lines crossing a 512x512 canvas, with each
delineated area stippled with blue dots fading along a different
direction.](example.png)

*Example output (512×512): black lines, blue dots, white background.*

## How it works

1. Pick a random number of lines (6 to 8 by default), each with
   endpoints on the canvas border so that it fully crosses the canvas.
2. Rasterize the lines onto a bitmap mask using Bresenham's algorithm.
3. Identify connected components of non-line pixels via 4-connected
   flood fill; each component is an *area*.
4. For each area, pick a random direction (a uniformly random angle)
   and compute, for every pixel, its projection along that direction.
   Normalize to `[0, 1]` over the area.
5. Place a blue dot at a pixel with probability
   `max-density × (1 - s)`, so density decays linearly from one end of
   the area to the other.
6. Paint the line pixels in the line color on top.
7. Save the resulting image as a PNG via
   [zpng](https://www.xach.com/lisp/zpng/).

## Dependencies

The only external dependency is
[zpng](https://www.xach.com/lisp/zpng/), available in Quicklisp. The
program has been tested with SBCL.

## Usage

From a REPL with Quicklisp available:

```lisp
(push #P"/path/to/com-informatimago/small-cl-pgms/draw-random/"
      asdf:*central-registry*)

(ql:quickload "com.informatimago.small-cl-pgms.draw-random")

(com.informatimago.small-cl-pgms.draw-random:draw-random
   :pathname "example.png")
```

All parameters are keyword arguments with sensible defaults:

```lisp
(com.informatimago.small-cl-pgms.draw-random:draw-random
   :width       512
   :height      512
   :background  #(255 255 255)   ; white
   :line-color  #(0   0   0)     ; black
   :dot-color   #(0   0   255)   ; blue
   :min-lines   6
   :max-lines   8
   :max-density 0.25
   :pathname    "example.png")
```

### Parameters

| Keyword         | Default              | Meaning                                                      |
|-----------------|----------------------|--------------------------------------------------------------|
| `:width`        | `512`                | Canvas width in pixels.                                      |
| `:height`       | `512`                | Canvas height in pixels.                                     |
| `:background`   | `#(255 255 255)`     | Background color, as an RGB vector of bytes.                 |
| `:line-color`   | `#(0 0 0)`           | Color used to draw the lines.                                |
| `:dot-color`    | `#(0 0 255)`         | Color used to stipple the areas.                             |
| `:min-lines`    | `6`                  | Lower bound on the number of random lines.                   |
| `:max-lines`    | `8`                  | Upper bound on the number of random lines.                   |
| `:max-density`  | `0.25`               | Fraction of pixels stippled at the dense end of each area.   |
| `:pathname`     | `"draw-random.png"`  | Output PNG file path.                                        |

## Files

- [`com.informatimago.small-cl-pgms.draw-random.asd`](com.informatimago.small-cl-pgms.draw-random.asd) — ASDF system definition.
- [`draw-random.lisp`](draw-random.lisp) — the whole program.
- [`example.png`](example.png) — the example output shown above.

## License

Copyright © Pascal J. Bourguignon 2026. Distributed under the
[GNU Affero General Public License, version 3](https://www.gnu.org/licenses/agpl-3.0.html).
