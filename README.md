Parallax
========

The file `parallax.hs` provides a [fay](https://github.com/faylang) implementation to change the scroll-speed of html elements. For example, by adding the class `movable` and the attributes `data-speed="-1.2" data-offset="500"` to an element, the position of the element is calculated by `-1.2*x + 500` where `x` is the scroll-offset. The page [carinamitc.de](http://www.carinamitc.de) uses this library for its implementation.

This library is tested with `fay-0.14.5.0`, `fay-base-0.14.3.2`, and `fay-jquery-0.3.0.0`.
