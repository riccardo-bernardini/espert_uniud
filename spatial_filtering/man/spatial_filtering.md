# spatial_filtering

## What is this?

This is a script ruby that applies a *spatial filtering* to a sequence of camera events.

## Syntax

* The input event sequence is read from the standard input.  The sequence is expected to be in *event camera CSV format*
* The output event sequence is written to standard output.  The output is in *weighted CSV format*
* The filter to be used is specified on the command line.  It can assume two forms
	* **name:param1:param2:...:paramN** where **name** is the type of filter that is parameterized by parameters **param1**, ..., **paramN**.  The currently recognized filter types are
	 * **circle:radius** 
* **@filename** Read the impulse response from **filename** (currently in beta)

### Example

Using a circle with radius = 10 pixels
```sh
    spatial_filtering circle:10 < original.csv > filtered.csv
```

Reading the impulse response from a file
```sh
    spatial_filtering @funny-filter < original.csv > filtered.csv
```

### Difference between *event camera* and *weighted* CSV format

The difference between the two format is how the *weight* of an event is represented
* In the original format only weight +1 and -1 are possible, represented in the CSV file by, respectively, 1 and 0 (most probably a Boolean is used internally)
* The output of filtering may need a larger range of weights; therefore the weight in this case is integer number represented in the CSV format by... the integer itself (surprise!)

## Filter format

A file with an impulse response will have the following format
* It is a text file in ASCII encoding
* Any row whose first non-space character is `#` is considered a  comment and ignored
* Any row with only space is ignored
* It has N (non-comment) rows of integer values (base 10, of course), every row has the same number M of values
* **One and only one** value is marked with a `!` after the value.  That value is considered the *origin* of the impulse response 


  