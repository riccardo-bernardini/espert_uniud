# What is this?

This folder contains a program that "integrates" the events generated
by the camera.

# How do I install it?

Quite easy:

1. Install the GNAT FSF tools. Some Linux/BSD distributions or msys2 for Windows provide builds of GNAT FSF. 
   * Debian/ubuntu: `sudo apt install gnat gprbuild`
   * Arch Linux: `sudo pacman -S gcc-ada gprbuild`
   * msys2 for Windows: `sudo pacman -S mingw-w64-x86_64-gcc-ada mingw-w64-x86_64-gprbuild`
2. Open a command line and go to the folder containing this README
3. Do `gprbuild`
4. At the end you should find executable `main` (yeah... not really creative) in the folder `obj`

# How do I use it?

The syntax is very simple

```

main parameter1 parameter2 ... [options] decay sampling filename-fmt [event-filename]
```
where every parameter has the form
```bash
name=value
```
or simply
```bash
name
```

where `name` can be

* **decay** specifies how the pixel memory "decays" with time. **Mandatory**. Its value can be
  * `no-decay` (or `none`) No decay
  * `linear:tau` (or `lin:tau` or `l:tau`) linear decays until the pixel value reaches 0.  Time `tau` is the inverse of the `decayparameter` of the DV software.  See section *Time syntax* for the syntax used to specify times.  If `I0` is the intensity at time 0 the intensity at time `t` is equal to

```
max(I0 - t/tau, N)
```
if `I0 > N` where `N` is the neutral level.  If `I0 < N` the new intensity is
```
min(I0 + t/tau, N)
```
  * `exponential:t` (or `exp:t` or `e:t`) Exponential decay, `t` is the time constant. See section *Time syntax* for the syntax used to specify times.
  * `reset` or `step`: reset the image to the neutral level at every sampling instant
* **sampling** The value is the interval between two frames.  See section *Time syntax* for the syntax used to specify times.
* **framerate** or **frame-rate**: the value is the number of frame per seconds.  It is the inverse of *sampling* 
* **template** or **output** (**mandatory**) String that is used to create the names of the files with the frames generated by this program.  String `%d` will be replaced by the frame number, left justified to 5 digit, padded with '0'.  The format of the image is determined by the extension as follows
   * **No extension** or **.raw**:  the primitive *raw format* described later is used 
   * **.pgm**: the [PGM (Portable Gray Map)](https://netpbm.sourceforge.net/doc/pgm.html) format is used
   * **.png**  the image is saved as PNG
* **input** Optional.  The filename with the events generated by the camera, in formato CSV.  If this parameter is missing, the events are read from the standard input.
* **min-value** or **min** set the minimum value of a pixel
* **max-value** or **max** set the maximum value of a pixel
*  **neutral-value** or **neutral** or **zero-level** set the reset value of a pixel used by `linear` and `reset` decay
* **weight** or **event-contribution**  gives the contribution is applied to a pixel (saturating at `min` and `max` values) when an event arrives
* **rectify** convert negative pulse to positive one. No parameters
* **lazy-decay** Usually pixel decays as time passes; with this option, the decay is applied only if an event happens.  It is the negation of the *Synchronous decay* option in the DV software.  No parameters

The following excerpt taken from [DV documentation](https://inivation.gitlab.io/dv/dv-docs/docs/accumulator-module/#accumulator-settings-overview)  explains how to interpreter min and max

> The frame the module generates is an unsigned 8-bit grayscale image, normalized between Min potential and Max potential. A pixel with the value Min potential corresponds to a pixel with the value 0 in the output frame. A pixel with the value Max potential corresponds to a pixel with the value 255 in the output frame.

| Name | Value              | Mandatory | Default | Notes |
|------|-------|-----------|---------|---|
| decay | none, linear:tau, exp:tau, reset | **Yes**   |         |       |
| sampling | Time between frames | No  | 1000fps | See *Time Syntax*. Incompatible with *frame-rate* |
| frame-rate | Number of frames per second | No | 1000 | Incompatible with *sampling* |
| output     | Template for the output filenames | **Yes** |  |  |
| input      | Input filename | No | standard input |  |
| start      | Starting time  | No | First event | See *Time Syntax* |
| stop       | Stop time  | No | Last event | See *Time Syntax* |
| min        | Minimum value | No | 0.0 |  |
| max        | Maximum value | No | 1.0 |  |
| neutral    | Neutral value | No | (min+max)/2 | used by reset and linear  |
| weight     | Contrinute of each event | No| (max-min)/2 | |
| rectify    | none | No |  |  |
| lazy-decay | none | No |  |  |


accepted options are
* `--min:value` set the minimum value of a pixel
* `--max:value` set the maximum value of a pixel
* `--neutral:value` or `--N:value` set the reset value of a pixel used by `linear` and `reset` decay
* `--weight:value` or `--event-contribution:value`  gives the contribution is applied to a pixel (saturating at `min` and `max` values) when an event arrives
* `--rectify` convert negative pulse to positive one
* `--lazy-decay` Usually pixel decays as time passes; with this option, the decay is applied only if an event happens.  It is the negation of the *Synchronous decay* option in the DV software

The following excerpt taken from [DV documentation](https://inivation.gitlab.io/dv/dv-docs/docs/accumulator-module/#accumulator-settings-overview)  explains how to interpreter min and max

> The frame the module generates is an unsigned 8-bit grayscale image, normalized between Min potential and Max potential. A pixel with the value Min potential corresponds to a pixel with the value 0 in the output frame. A pixel with the value Max potential corresponds to a pixel with the value 255 in the output frame.

## Time syntax

Durations can be specified as
* **timestamp**, in this case a simple number with no unit is used (e.g., 42)
* **seconds** (and sub-multiples); in this case the number is followed by `s` or `ms` or `us` or `ns`, e.g., `1ms`
* **frame rate**; in this case the number is followed by `fps`, e.g., `1000fps` (which is equivalent to `1ms`)

**Note:** number and unit **must not be** separated by space, that is, `12ms` is OK, `12 ms` is **NOT**

## Raw image format

Very simple.  The file contains
* An unsigned 16 bit integer (Intel endianess) with the `x_size`, that is, the number of columns
* An unsigned 16 bit integer (Intel endianess) with the `y_size`, that is, the number of rows
* `x_size * y_size` 8 bit unsigned integers with the pixel values in the range 0 .. 255

