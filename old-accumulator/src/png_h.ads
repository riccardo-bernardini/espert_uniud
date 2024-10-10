package Png_H is
   PNG_LIBPNG_VER_STRING     : aliased constant String :=
                                 "1.2.44" & ASCII.NUL;

   PNG_HEADER_VERSION_STRING : aliased constant String :=
                                 " libpng version 1.2.44 - June 26, 2010" & ASCII.LF & "" & ASCII.NUL;

   PNG_LIBPNG_VER_SONUM      : constant := 0;
   PNG_LIBPNG_VER_DLLNUM     : constant := 13;

   PNG_LIBPNG_VER_MAJOR      : constant := 1;
   PNG_LIBPNG_VER_MINOR      : constant := 2;
   PNG_LIBPNG_VER_RELEASE    : constant := 44;

   PNG_LIBPNG_VER_BUILD      : constant := 0;

   PNG_LIBPNG_BUILD_ALPHA               : constant := 1;
   PNG_LIBPNG_BUILD_BETA                : constant := 2;
   PNG_LIBPNG_BUILD_RC                  : constant := 3;
   PNG_LIBPNG_BUILD_STABLE              : constant := 4;
   PNG_LIBPNG_BUILD_RELEASE_STATUS_MASK : constant := 7;

   PNG_LIBPNG_BUILD_PATCH    : constant := 8;

   PNG_LIBPNG_BUILD_PRIVATE  : constant := 16;

   PNG_LIBPNG_BUILD_SPECIAL  : constant := 32;


   PNG_LIBPNG_VER            : constant := 10244;

   PNG_TEXT_COMPRESSION_NONE_WR : constant := -3;
   PNG_TEXT_COMPRESSION_ZTXt_WR : constant := -2;
   PNG_TEXT_COMPRESSION_NONE    : constant := -1;
   PNG_TEXT_COMPRESSION_ZTXt    : constant := 0;
   PNG_ITXT_COMPRESSION_NONE    : constant := 1;
   PNG_ITXT_COMPRESSION_ZTXt    : constant := 2;
   PNG_TEXT_COMPRESSION_LAST    : constant := 3;

   PNG_CHUNK_NAME_LENGTH     : constant := 5;

   PNG_COLOR_MASK_PALETTE    : constant  := 1;
   PNG_COLOR_MASK_COLOR      : constant  := 2;
   PNG_COLOR_MASK_ALPHA      : constant  := 4;
   PNG_COLOR_TYPE_GRAY       : constant  := 0;
   PNG_COLOR_TYPE_PALETTE    : constant  := PNG_COLOR_MASK_COLOR + PNG_COLOR_MASK_PALETTE;

   PNG_COLOR_TYPE_RGB       : constant := PNG_COLOR_MASK_COLOR;
   PNG_COLOR_TYPE_RGB_ALPHA :  constant := PNG_COLOR_MASK_COLOR + PNG_COLOR_MASK_ALPHA;

   PNG_COLOR_TYPE_GRAY_ALPHA : constant  := PNG_COLOR_MASK_ALPHA;
   PNG_COLOR_TYPE_RGBA       : constant  := PNG_COLOR_TYPE_RGB_ALPHA;
   PNG_COLOR_TYPE_GA         : constant  := PNG_COLOR_TYPE_GRAY_ALPHA;

   PNG_COMPRESSION_TYPE_BASE    : constant := 0;
   PNG_COMPRESSION_TYPE_DEFAULT : constant := PNG_COMPRESSION_TYPE_BASE;

   PNG_FILTER_TYPE_BASE        : constant := 0;
   PNG_INTRAPIXEL_DIFFERENCING : constant := 64;
   PNG_FILTER_TYPE_DEFAULT     : constant := PNG_FILTER_TYPE_BASE;

   PNG_INTERLACE_NONE        : constant := 0;
   PNG_INTERLACE_ADAM7       : constant := 1;
   PNG_INTERLACE_LAST        : constant := 2;

   PNG_OFFSET_PIXEL          : constant := 0;
   PNG_OFFSET_MICROMETER     : constant := 1;
   PNG_OFFSET_LAST           : constant := 2;

   PNG_EQUATION_LINEAR       : constant := 0;
   PNG_EQUATION_BASE_E       : constant := 1;
   PNG_EQUATION_ARBITRARY    : constant := 2;
   PNG_EQUATION_HYPERBOLIC   : constant := 3;
   PNG_EQUATION_LAST         : constant := 4;

   PNG_SCALE_UNKNOWN         : constant := 0;
   PNG_SCALE_METER           : constant := 1;
   PNG_SCALE_RADIAN          : constant := 2;
   PNG_SCALE_LAST            : constant := 3;

   PNG_RESOLUTION_UNKNOWN    : constant := 0;
   PNG_RESOLUTION_METER      : constant := 1;
   PNG_RESOLUTION_LAST       : constant := 2;

   PNG_SRGB_INTENT_PERCEPTUAL : constant := 0;
   PNG_SRGB_INTENT_RELATIVE   : constant := 1;
   PNG_SRGB_INTENT_SATURATION : constant := 2;
   PNG_SRGB_INTENT_ABSOLUTE   : constant := 3;
   PNG_SRGB_INTENT_LAST       : constant := 4;

   PNG_KEYWORD_MAX_LENGTH    : constant := 79;

   PNG_MAX_PALETTE_LENGTH    : constant := 256;

   PNG_INFO_GAMA             : constant := 16#0001#;
   PNG_INFO_SBIT             : constant := 16#0002#;
   PNG_INFO_CHRM             : constant := 16#0004#;
   PNG_INFO_PLTE             : constant := 16#0008#;
   PNG_INFO_TRNS             : constant := 16#0010#;
   PNG_INFO_BKGD             : constant := 16#0020#;
   PNG_INFO_HIST             : constant := 16#0040#;
   PNG_INFO_PHYs             : constant := 16#0080#;
   PNG_INFO_OFFs             : constant := 16#0100#;
   PNG_INFO_TIME             : constant := 16#0200#;
   PNG_INFO_PCAL             : constant := 16#0400#;
   PNG_INFO_SRGB             : constant := 16#0800#;
   PNG_INFO_ICCP             : constant := 16#1000#;
   PNG_INFO_SPLT             : constant := 16#2000#;
   PNG_INFO_SCAL             : constant := 16#4000#;
   PNG_INFO_IDAT             : constant := 16#8000#;

   PNG_TRANSFORM_IDENTITY     : constant := 16#0000#;
   PNG_TRANSFORM_STRIP_16     : constant := 16#0001#;
   PNG_TRANSFORM_STRIP_ALPHA  : constant := 16#0002#;
   PNG_TRANSFORM_PACKING      : constant := 16#0004#;
   PNG_TRANSFORM_PACKSWAP     : constant := 16#0008#;
   PNG_TRANSFORM_EXPAND       : constant := 16#0010#;
   PNG_TRANSFORM_INVERT_MONO  : constant := 16#0020#;
   PNG_TRANSFORM_SHIFT        : constant := 16#0040#;
   PNG_TRANSFORM_BGR          : constant := 16#0080#;
   PNG_TRANSFORM_SWAP_ALPHA   : constant := 16#0100#;
   PNG_TRANSFORM_SWAP_ENDIAN  : constant := 16#0200#;
   PNG_TRANSFORM_INVERT_ALPHA : constant := 16#0400#;
   PNG_TRANSFORM_STRIP_FILLER : constant := 16#0800#;

   PNG_TRANSFORM_STRIP_FILLER_BEFORE : constant := 16#0800#;
   PNG_TRANSFORM_STRIP_FILLER_AFTER  : constant := 16#1000#;

   PNG_TRANSFORM_GRAY_TO_RGB : constant := 16#2000#;

   PNG_FLAG_MNG_EMPTY_PLTE   : constant := 16#01#;
   PNG_FLAG_MNG_FILTER_64    : constant := 16#04#;
   PNG_ALL_MNG_FEATURES      : constant := 16#05#;



   PNG_FILLER_BEFORE         : constant := 0;
   PNG_FILLER_AFTER          : constant := 1;

   PNG_BACKGROUND_GAMMA_UNKNOWN : constant := 0;
   PNG_BACKGROUND_GAMMA_SCREEN  : constant := 1;
   PNG_BACKGROUND_GAMMA_FILE    : constant := 2;
   PNG_BACKGROUND_GAMMA_UNIQUE  : constant := 3;

   PNG_CRC_DEFAULT           : constant := 0;
   PNG_CRC_ERROR_QUIT        : constant := 1;
   PNG_CRC_WARN_DISCARD      : constant := 2;
   PNG_CRC_WARN_USE          : constant := 3;
   PNG_CRC_QUIET_USE         : constant := 4;
   PNG_CRC_NO_CHANGE         : constant := 5;

   PNG_NO_FILTERS            : constant := 16#00#;
   PNG_FILTER_NONE           : constant := 16#08#;
   PNG_FILTER_SUB            : constant := 16#10#;
   PNG_FILTER_UP             : constant := 16#20#;
   PNG_FILTER_AVG            : constant := 16#40#;
   PNG_FILTER_PAETH          : constant := 16#80#;
   PNG_ALL_FILTERS           : constant :=
                                 PNG_FILTER_NONE
                                   + PNG_FILTER_SUB
                                 + PNG_FILTER_UP
                                   + PNG_FILTER_AVG
                                 + PNG_FILTER_PAETH;

   PNG_FILTER_VALUE_NONE     : constant := 0;
   PNG_FILTER_VALUE_SUB      : constant := 1;
   PNG_FILTER_VALUE_UP       : constant := 2;
   PNG_FILTER_VALUE_AVG      : constant := 3;
   PNG_FILTER_VALUE_PAETH    : constant := 4;
   PNG_FILTER_VALUE_LAST     : constant := 5;

   PNG_FILTER_HEURISTIC_DEFAULT    : constant := 0;
   PNG_FILTER_HEURISTIC_UNWEIGHTED : constant := 1;
   PNG_FILTER_HEURISTIC_WEIGHTED   : constant := 2;
   PNG_FILTER_HEURISTIC_LAST       : constant := 3;

   PNG_DESTROY_WILL_FREE_DATA : constant := 1;
   PNG_SET_WILL_FREE_DATA     : constant := 1;
   PNG_USER_WILL_FREE_DATA    : constant := 2;

   PNG_FREE_HIST             : constant := 16#0008#;
   PNG_FREE_ICCP             : constant := 16#0010#;
   PNG_FREE_SPLT             : constant := 16#0020#;
   PNG_FREE_ROWS             : constant := 16#0040#;
   PNG_FREE_PCAL             : constant := 16#0080#;
   PNG_FREE_SCAL             : constant := 16#0100#;
   PNG_FREE_UNKN             : constant := 16#0200#;
   PNG_FREE_LIST             : constant := 16#0400#;
   PNG_FREE_PLTE             : constant := 16#1000#;
   PNG_FREE_TRNS             : constant := 16#2000#;
   PNG_FREE_TEXT             : constant := 16#4000#;
   PNG_FREE_ALL              : constant := 16#7fff#;
   PNG_FREE_MUL              : constant := 16#4220#;

   PNG_HANDLE_CHUNK_AS_DEFAULT : constant := 0;
   PNG_HANDLE_CHUNK_NEVER      : constant := 1;
   PNG_HANDLE_CHUNK_IF_SAFE    : constant := 2;
   PNG_HANDLE_CHUNK_ALWAYS     : constant := 3;

   PNG_ASM_FLAG_MMX_SUPPORT_COMPILED  : constant := 16#01#;
   PNG_ASM_FLAG_MMX_SUPPORT_IN_CPU    : constant := 16#02#;
   PNG_ASM_FLAG_MMX_READ_COMBINE_ROW  : constant := 16#04#;
   PNG_ASM_FLAG_MMX_READ_INTERLACE    : constant := 16#08#;
   PNG_ASM_FLAG_MMX_READ_FILTER_SUB   : constant := 16#10#;
   PNG_ASM_FLAG_MMX_READ_FILTER_UP    : constant := 16#20#;
   PNG_ASM_FLAG_MMX_READ_FILTER_AVG   : constant := 16#40#;
   PNG_ASM_FLAG_MMX_READ_FILTER_PAETH : constant := 16#80#;
   PNG_ASM_FLAGS_INITIALIZED          : constant := 16#80000000#;
   --  unsupported macro: PNG_MMX_READ_FLAGS ( PNG_ASM_FLAG_MMX_READ_COMBINE_ROW | PNG_ASM_FLAG_MMX_READ_INTERLACE | PNG_ASM_FLAG_MMX_READ_FILTER_SUB | PNG_ASM_FLAG_MMX_READ_FILTER_UP | PNG_ASM_FLAG_MMX_READ_FILTER_AVG | PNG_ASM_FLAG_MMX_READ_FILTER_PAETH )

   PNG_MMX_WRITE_FLAGS                : constant := ( 0 );
   --  unsupported macro: PNG_MMX_FLAGS ( PNG_ASM_FLAG_MMX_SUPPORT_COMPILED | PNG_ASM_FLAG_MMX_SUPPORT_IN_CPU | PNG_MMX_READ_FLAGS | PNG_MMX_WRITE_FLAGS )

   PNG_SELECT_READ                    : constant := 1;
   PNG_SELECT_WRITE                   : constant := 2;
   --  arg-macro: procedure png_composite (composite, fg, alpha, bg)
   --    { png_uint_16 temp := (png_uint_16)((png_uint_16)(fg) * (png_uint_16)(alpha) + (png_uint_16)(bg)*(png_uint_16)(255 - (png_uint_16)(alpha)) + (png_uint_16)128); (composite) := (png_byte)((temp + (temp >> 8)) >> 8); }
   --  arg-macro: procedure png_composite_16 (composite, fg, alpha, bg)
   --    { png_uint_32 temp := (png_uint_32)((png_uint_32)(fg) * (png_uint_32)(alpha) + (png_uint_32)(bg)*(png_uint_32)(65535 - (png_uint_32)(alpha)) + (png_uint_32)32768); (composite) := (png_uint_16)((temp + (temp >> 16)) >> 16); }

   PNG_HAVE_IHDR                      : constant := 16#01#;
   PNG_HAVE_PLTE                      : constant := 16#02#;
   PNG_HAVE_IDAT                      : constant := 16#04#;
   PNG_AFTER_IDAT                     : constant := 16#08#;
   PNG_HAVE_IEND                      : constant := 16#10#;

end Png_H;
