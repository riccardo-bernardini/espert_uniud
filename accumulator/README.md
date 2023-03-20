= What is this?

This folder contains a program that "integrates" the events generated
by the camera.

= How do I install it?

Quite easy:

1. Install the GNAT FSF tools. Some Linux/BSD distributions or msys2 for Windows provide builds of GNAT FSF. 
   * Debian/ubuntu: `sudo apt install gnat gprbuild`
   * Arch Linux: `sudo pacman -S gcc-ada gprbuild`
   * msys2 for Windows: `sudo pacman -S mingw-w64-x86_64-gcc-ada mingw-w64-x86_64-gprbuild`
2. Open a command line and go to the folder containing this README
3. Do `gprbuild`
4. At the end you should find executable `main` (yeah... not really creative) in the folder `obj`

= How do I use it?

The syntax is as follows

```
main decay sampling filename-fmt [event-filename]
```

where

* **decay** specifies how the pixel memory "decays" with time.  It can be
  * `step` (or `s`) No decay
  * `linear:t` (or `lin:t` or `l:t`) linear decays until the pixel value reaches 0.  Time `t` is the time required to go from 1 to 0.  See section *Time syntax* for the syntax used to specify times.
  * `exponential:t` (or `exp:t` or `e:t`) Exponential decay, `t` is the time constant. See section *Time syntax* for the syntax used to specify times.
* **sampling** time, that is, the interval between two frames.  See section *Time syntax* for the syntax used to specify times.
* **filename-fmt** String that is used to create the names of the files with the frames generated by this program.  String `%d` will be replaced by the frame number, left justified to 5 digit, padded with '0'.  By default, the image is in the primitive *raw format* described later. In the future maybe we will support more image formats.
* **event-filename** Optional.  The filename with the events generated by the camera, in formato CSV.  If this parameter is missing, the events are read from the standard input.

== Time format

Durations can be specified as
* **timestamp**, in this case a simple number with no unit is used (e.g., 42)
* **seconds** (and sub-multiples); in this case the number is followed by `s` or `ms` or `us` or `ns`, e.g., `1ms`
* **frame rate**; in this case the number is followed by `fps`, e.g., `1000fps` (which is equivalent to `1ms`)

**NO SPACE** must be between the number and the unit; that is, `12ms` is OK, `12 ms` is **NOT**

== Raw image format

Very simple.  The file contains
* An unsigned 16 bit integer (Intel endianess) with the `x_size`, that is, the number of columns
* An unsigned 16 bit integer (Intel endianess) with the `y_size`, that is, the number of rows
* `x_size * y_size` 8 bit unsigned integers with the pixel values in the range 0 .. 255
