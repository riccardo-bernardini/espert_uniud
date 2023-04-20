/*
 * A simple libpng example program
 * http://zarb.org/~gc/html/libpng.html
 *
 * Modified by Yoshimasa Niwa to make it much simpler
 * and support all defined color_type.
 *
 * To build, use the next instruction on OS X.
 * $ brew install libpng
 * $ clang -lz -lpng16 libpng_test.c
 *
 * Copyright 2002-2010 Guillaume Cottenceau.
 *
 * This software may be freely redistributed under the terms
 * of the X11 license.
 *
 */

#include <png.h>
#include <stdio.h>
#include <stdlib.h>

void
fill_row_pointers (png_bytepp row_pointers, png_bytep image, png_uint_32 width,
                   png_uint_32 height)
{
  png_bytep row = image;

  for (int i = 0; i < height; i++)
    {
      row_pointers[i] = row;
      row += width;
    }
}

int
write_png_file (char *filename, png_bytep image, png_uint_32 width,
                png_uint_32 height, int depth, int color_type)
{
  int y;

  FILE *fp = fopen (filename, "wb");
  if (!fp)
    {
      goto die;
    }

  png_structp png
    = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png)
    {
      goto die;
    }

  png_infop info = png_create_info_struct (png);
  if (!info)
    {
      goto die;
    }

  if (setjmp (png_jmpbuf (png)))
    {
      goto die;
    }

  png_init_io (png, fp);

  /* png_set_IHDR, (png_const_structrp png_ptr,
     png_inforp info_ptr, png_uint_32 width, png_uint_32 height, int bit_depth,
     int color_type, int interlace_method, int compression_method,
     int filter_method
  */

  png_set_IHDR (png, info, width, height, depth, color_type, PNG_INTERLACE_NONE,
                PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

  png_write_info (png, info);


  {
    png_bytep row_pointers[height];

    fill_row_pointers (row_pointers, image, width, height);

    png_write_image (png, row_pointers);
  }

  png_write_end (png, NULL);

  fclose (fp);

  if (png && info)
    {
      png_destroy_write_struct (&png, &info);
    }

  return 0;

 die:
  if (png != NULL)
    {
      png_destroy_write_struct (&png, &info);
    }

  if (fp != NULL)
    {
      fclose (fp);
    }

  return -1;
}
