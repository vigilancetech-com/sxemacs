dnl sxe-mm.m4 -- Multimedia goodness

dnl MM tests
dnl ========

AC_DEFUN([SXE_MM_CHECK_XPM], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for xpm support])
	AC_MSG_RESULT([])

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_PRE_LIBS $X_LIBS $libs_x"
	LIBS="$LIBS -lXpm -lX11"

	SXE_CHECK_HEADERS([X11/xpm.h], [:], [MM_FAIL])

	AC_MSG_CHECKING([for Xpm (more recent than 3.4f)])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#define XPM_NUMBERS
#include <X11/xpm.h>
int main(int c, char **v)
{
	return c == 1 ? 0 :
		XpmIncludeVersion != XpmLibraryVersion() ? 1 :
		XpmIncludeVersion < 30406 ? 2 : 0 ;
}]])], [./conftest dummy_arg; xpm_status=$?;
		if test "$xpm_status" = "0"; then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_RESULT([no])
			MM_FAIL
			if test "$xpm_status" = "1"; then
				AC_MSG_WARN([dnl
Xpm library version and header file version don't match!
I have disabled xpm on your behalf.])
			elif test "$xpm_status" = "2"; then
				AC_MSG_WARN([dnl
Xpm library version is too old!
I have disabled xpm on your behalf.])
			else
				AC_MSG_WARN([dnl
Internal xpm detection logic error!])
			fi
		fi], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL])
	xe_check_libs=
	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_XPM

AC_DEFUN([SXE_MM_CHECK_XFACE], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for xface support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	dnl SXE_PREPEND_UNDUP([-I$x_includes], [c_switch_site])
	dnl SXE_PREPEND_UNDUP([-L$x_libraries], [ld_switch_site])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_CHECK_HEADERS([compface.h], [:], [MM_FAIL])
	AC_CHECK_LIB([compface], [UnGenFace], [:], [MM_FAIL])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_XFACE

AC_DEFUN([SXE_MM_CHECK_GIF], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for gif support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_GIF

AC_DEFUN([SXE_MM_CHECK_JPEG], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for jpeg support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	dnl SXE_PREPEND_UNDUP([-I$x_includes], [c_switch_site])
	dnl SXE_PREPEND_UNDUP([-L$x_libraries], [ld_switch_site])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_CHECK_HEADERS([jpeglib.h], [:], [MM_FAIL])
	AC_CHECK_LIB([jpeg], [jpeg_destroy_decompress], [:], [MM_FAIL], [$INFLATE_LIB])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_JPEG


AC_DEFUN([_SXE_MM_CHECK_PNG_HEADERS], [dnl
	## defines sxe_cv_feat_png_headers
	## also defines and substs PNG_CPPFLAGS

	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	PNG_CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	CPPFLAGS="$PNG_CPPFLAGS"
	SXE_CHECK_HEADERS([png.h])
	SXE_RESTORE_LIBS

	if test "${ac_cv_header_png_h}" = "yes"; then
		sxe_cv_feat_png_headers="yes"
	else
		sxe_cv_feat_png_headers="no"
	fi

	AC_SUBST([PNG_CPPFLAGS])
])dnl _SXE_MM_CHECK_PNG_HEADERS

AC_DEFUN([_SXE_MM_CHECK_PNG_LIBS], [dnl
	## defines sxe_cv_feat_png_libs
	## also defines and substs PNG_LIBS and PNG_LDFLAGS

	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	PNG_LDFLAGS="$LDFLAGS $X_LIBS"
	LDFLAGS="$PNG_LDFLAGS"
	AC_CHECK_LIB([png], [png_read_image], [], [], [$INFLATE_LIB])
	SXE_RESTORE_LIBS

	if test "${ac_cv_lib_png_png_read_image}" = "yes"; then
		sxe_cv_feat_png_libs="yes"
		PNG_LIBS="$INFLATE_LIB -lpng"
	else
		sxe_cv_feat_png_libs="no"
		PNG_LIBS=
	fi

	AC_SUBST([PNG_LIBS])
	AC_SUBST([PNG_LDFLAGS])
])dnl _SXE_MM_CHECK_PNG_LIBS

AC_DEFUN([_SXE_MM_CHECK_PNG_VERSION], [dnl
	## defines sxe_cv_tmp_png_status to yes if version info seems okay
	AC_REQUIRE([_SXE_MM_CHECK_PNG_HEADERS])
	AC_REQUIRE([_SXE_MM_CHECK_PNG_LIBS])

	SXE_MSG_CHECKING([for workable png version information])

	SXE_DUMP_LIBS
	CPPFLAGS="$PNG_CPPFLAGS"
	LDFLAGS="$PNG_LDFLAGS"
	LIBS="$PNG_LIBS"
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#if defined HAVE_PNG_H
# include <png.h>
#endif /* HAVE_PNG_H */

int main(int c, char **v)
{
	if (c == 1) {
		return 0;
	}
	if (strcmp(png_libpng_ver, PNG_LIBPNG_VER_STRING) != 0) {
		return 1;
	}
	return (PNG_LIBPNG_VER < 10002) ? 2 : 0 ;
}]])], [./conftest dummy_arg; png_status=$?;
		if test "$png_status" = "0"; then
			sxe_cv_tmp_png_status="yes"

		elif test "$png_status" = "1"; then
			sxe_cv_tmp_png_status="yes"
			AC_MSG_WARN([dnl
PNG library version and header file don't match!
This is odd but I think I give it a whirl.
If things work out badly I suggest to turn off your computer forever.])

		elif test "$png_status" = "2"; then
			sxe_cv_tmp_png_status="no"
			AC_MSG_WARN([dnl
PNG library version too old (pre 1.0.2)!
I have disabled PNG support on your behalf.])

		else
			sxe_cv_tmp_png_status="no"
			AC_MSG_ERROR([dnl
Whatever happened just now, I'm completely fucked.])
		fi], [sxe_cv_tmp_png_status="no"], [sxe_cv_tmp_png_status="no"])
	SXE_MSG_RESULT([${sxe_cv_tmp_png_status}])
	SXE_RESTORE_LIBS
])dnl _SXE_MM_CHECK_PNG_VERSION

AC_DEFUN([SXE_MM_CHECK_PNG], [
	## SXE_MM_CHECK_PNG(<action-if-found>, <action-if-not-found>)
	## defines HAVE_PNG in case png is usable
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	SXE_MSG_CHECKING([for PNG support])
	SXE_MSG_RESULT([])

	_SXE_MM_CHECK_PNG_HEADERS
	_SXE_MM_CHECK_PNG_LIBS
	_SXE_MM_CHECK_PNG_VERSION

	## final judgement
	## we want to see a header and png_read_image really
	if test "${sxe_cv_feat_png_headers}" = "yes" -a \
		"${sxe_cv_feat_png_libs}" = "yes" -a \
		"${sxe_cv_tmp_png_status}" = "yes"; then
		sxe_cv_feat_png="yes"
		AC_DEFINE([HAVE_PNG], [1], [Define to 1 if png is usable.])
		MM_SUCC
	else
		sxe_cv_feat_png="no"
		MM_FAIL
	fi
])dnl SXE_MM_CHECK_PNG

AC_DEFUN([SXE_MM_CHECK_TIFF], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for TIFF support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	dnl SXE_PREPEND_UNDUP([-I$x_includes], [c_switch_site])
	dnl SXE_PREPEND_UNDUP([-L$x_libraries], [ld_switch_site])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_CHECK_HEADERS([tiffio.h], [:], [MM_FAIL])
	AC_CHECK_LIB([tiff], [TIFFClientOpen], [:], [MM_FAIL], [$INFLATE_LIB])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_TIFF

AC_DEFUN([SXE_MM_SEARCH_INFLATE], [
	dnl Too many stupid linkers can't detect cascaded lib dependencies
	dnl  until runtime. So we always search for libz compression support.
	AC_SEARCH_LIBS([inflate], [c z gz], [
		if test "$ac_cv_lib_c_inflate" = "yes"; then
			INFLATE_LIB="c"
		elif test "$ac_cv_lib_z_inflate" = "yes"; then
			INFLATE_LIB="z"
		elif test "$ac_cv_lib_gz_inflate" = "yes"; then
			INFLATE_LIB="gz"
		fi], [INFLATE_LIB=])
	if test -n "$INFLATE_LIB"; then
		SXE_PREPEND([$INFLATE_LIB], [MM_LIBS])
	fi
])dnl SXE_MM_SEARCH_INFLATE

AC_DEFUN([SXE_MM_CHECK_SNDFILE], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([sndfile], [sndfile], [1.0.12], [dnl
		sf_open sf_close sf_readf_short sf_readf_int dnl
		sf_readf_float sf_seek sf_open_virtual],
		[sndfile.h], [$1], [$2])
])dnl SXE_MM_CHECK_SNDFILE

AC_DEFUN([SXE_MM_CHECK_FFMPEG], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	sxe_cv_feat_ffmpeg=

	## Minimum versions required matches FFmpeg stable v2.0.1
	## released 2013-08-13
	SXE_PC_CHECK_VERSION_ATLEAST([libavformat], [55.12.100])
	SXE_PC_CHECK_VERSION_ATLEAST([libavcodec], [55.18.102])
	SXE_PC_CHECK_VERSION_ATLEAST([libavutil], [52.38.100])

	SXE_CHECK_FFMPEG_HEADERS
	SXE_CHECK_FFMPEG_LIBS

	## make sure we have at least one of the headers
	if test "$ac_cv_header_avformat_h" = "yes" -o \
		"$ac_cv_header_ffmpeg_avformat_h" = "yes" -o \
		"$ac_cv_header_libavformat_avformat_h" = "yes"; then
		sxe_cv_feat_ffmpeg_headers="yes"
	else
		sxe_cv_feat_ffmpeg_headers="no"
	fi

	if test "$sxe_cv_feat_ffmpeg_headers" = "yes"; then
		if test "$ac_cv_header_avcodec_h" = "yes" -o \
                        "$ac_cv_header_ffmpeg_avcodec_h" = "yes" -o \
		 	"$ac_cv_header_libavcodec_avcodec_h" = "yes"; then
			sxe_cv_feat_ffmpeg_headers="yes"
		else
			sxe_cv_feat_ffmpeg_headers="no"
		fi
	fi

	if test "$sxe_cv_feat_ffmpeg_headers" = "yes"; then
	        if test "$ac_cv_header_ffmpeg_dict_h" = "yes" -o \
			"$ac_cv_header_libavutil_dict_h" = "yes"; then
			sxe_cv_feat_ffmpeg_headers="yes"
		else
			sxe_cv_feat_ffmpeg_headers="no"
		fi
	fi

	if test "$sxe_cv_feat_ffmpeg_headers" = "yes"; then
	        if test "$ac_cv_header_ffmpeg_time_h" = "yes" -o \
			"$ac_cv_header_libavutil_time_h" = "yes"; then
			sxe_cv_feat_ffmpeg_headers="yes"
		else
			sxe_cv_feat_ffmpeg_headers="no"
		fi
	fi

        ## make sure either decode_audio is there
	if test "$ac_cv_lib_avcodec_avcodec_decode_audio" = "yes" -o \
	        "$ac_cv_lib_avcodec_avcodec_decode_audio2" = "yes" -o \
	 	"$ac_cv_lib_avcodec_avcodec_decode_audio3" = "yes"; then
		sxe_cv_feat_ffmpeg_decoders="yes"
	fi

	if test "$sxe_cv_feat_ffmpeg_headers" = "yes" -a \
		"$sxe_cv_feat_ffmpeg_decoders" = "yes" -a \
		"$ac_cv_lib_avformat_avformat_alloc_context" = "yes" -a \
		"$ac_cv_lib_avformat_avformat_free_context" = "yes" -a \
		"$ac_cv_lib_avformat_avio_alloc_context" = "yes" -a \
		"$ac_cv_lib_avformat_avio_size" = "yes" -a \
		"$ac_cv_lib_avformat_avio_feof" = "yes" -a \
		"$ac_cv_lib_avformat_avformat_open_input" = "yes" -a \
		"$ac_cv_lib_avformat_avformat_close_input" = "yes" -a \
		"$ac_cv_lib_avformat_avformat_find_stream_info" = "yes" -a \
		"$ac_cv_lib_avformat_av_probe_input_format" = "yes" -a \
		"$ac_cv_lib_avformat_av_iformat_next" = "yes" -a \
		"$ac_cv_lib_avformat_av_read_frame" = "yes" -a \
		"$ac_cv_lib_avformat_av_seek_frame" = "yes" -a \
		"$ac_cv_lib_avformat_av_register_all" = "yes" -a \
		"$ac_cv_lib_avformat_av_dump_format" = "yes" -a \
		"$ac_cv_lib_avformat_av_get_sample_fmt_string" = "yes" -a \
		"$ac_cv_lib_avformat_av_get_bytes_per_sample" = "yes" -a \
		"$ac_cv_lib_avcodec_avcodec_find_decoder" = "yes" -a \
		"$ac_cv_lib_avcodec_avcodec_open2" = "yes" -a \
		"$ac_cv_lib_avutil_av_dict_get" = "yes"; then
		sxe_cv_feat_ffmpeg="yes"
	else
		sxe_cv_feat_ffmpeg="no"
	fi

	if test "$sxe_cv_feat_ffmpeg" = "yes"; then
		:
		ACTION_IF_FOUND
	else
		:
		ACTION_IF_NOT_FOUND
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_MM_CHECK_FFMPEG

AC_DEFUN([SXE_CHECK_FFMPEG_HEADERS], [dnl
	FFMPEG_CPPFLAGS="$(${PKG_CONFIG} --cflags libavformat libavcodec libavutil)"

	## backup current configuration
	SXE_DUMP_LIBS
	CPPFLAGS="${CPPFLAGS} ${FFMPEG_CPPFLAGS}"
	AC_CHECK_HEADERS([avformat.h avcodec.h])
	AC_CHECK_HEADERS([ffmpeg/avformat.h ffmpeg/avcodec.h])
	AC_CHECK_HEADERS([ffmpeg/dict.h ffmpeg/time.h])
	AC_CHECK_HEADERS([libavformat/avformat.h libavcodec/avcodec.h])
	AC_CHECK_HEADERS([libavutil/dict.h libavutil/time.h])

	## restore configuration
	SXE_RESTORE_LIBS
])dnl SXE_CHECK_FFMPEG_HEADERS

AC_DEFUN([SXE_CHECK_FFMPEG_LIBS], [dnl
	FFMPEG_LDFLAGS="$(${PKG_CONFIG} --libs-only-other libavformat libavcodec libavutil) \
		$(${PKG_CONFIG} --libs-only-L libavformat libavcodec libavutil)"
	FFMPEG_LIBS="$(${PKG_CONFIG} --libs-only-l libavformat libavcodec libavutil)"

	## backup current configuration
	SXE_DUMP_LIBS
	LDFLAGS="${LDFLAGS} ${FFMPEG_LDFLAGS}"

	AC_CHECK_LIB([avcodec], [avcodec_find_decoder], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avcodec], [avcodec_open2], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avcodec], [avcodec_decode_audio], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avcodec], [avcodec_decode_audio2], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avcodec], [avcodec_decode_audio3], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avcodec], [avcodec_decode_audio4], [:], [:], [${FFMPEG_LIBS}])

	AC_CHECK_LIB([avformat], [avformat_open_input], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [avformat_close_input], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [avformat_find_stream_info], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_iformat_next], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_probe_input_format], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_read_frame], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_seek_frame], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_register_all], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_dump_format], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_get_bytes_per_sample], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [av_get_sample_fmt_string], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [avformat_alloc_context], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [avformat_free_context], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [avio_alloc_context], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [avio_size], [:], [:], [${FFMPEG_LIBS}])
	AC_CHECK_LIB([avformat], [avio_feof], [:], [:], [${FFMPEG_LIBS}])

	AC_CHECK_LIB([avutil], [av_dict_get], [:], [:], [${FFMPEG_LIBS}])

	if test "$ac_cv_lib_avcodec_avcodec_decode_audio" = "yes"; then
		AC_DEFINE([HAVE_AVCODEC_DECODE_AUDIO], [1],
		          [Define to 1 if avcodec_decode_audio is usable.])
	fi
	if test "$ac_cv_lib_avcodec_avcodec_decode_audio2" = "yes"; then
		AC_DEFINE([HAVE_AVCODEC_DECODE_AUDIO2], [1],
		          [Define to 1 if avcodec_decode_audio2 is usable.])
	fi
	if test	"$ac_cv_lib_avcodec_avcodec_decode_audio3" = "yes"; then
		AC_DEFINE([HAVE_AVCODEC_DECODE_AUDIO3], [1],
		          [Define to 1 if avcodec_decode_audio is usable.])
	fi
	if test	"$ac_cv_lib_avcodec_avcodec_decode_audio4" = "yes"; then
		AC_DEFINE([HAVE_AVCODEC_DECODE_AUDIO4], [1],
		          [Define to 1 if avcodec_decode_audio is usable.])
	fi

	## restore configuration
	SXE_RESTORE_LIBS
])dnl SXE_CHECK_FFMPEG_LIBS


dnl
dnl SoX
dnl ===
AC_DEFUN([SXE_MM_CHECK_SOX], [dnl
	## call like this SXE_MM_CHECK_SOX([<if-found>], [<if-not-found>])
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for SoX support], [sxe_cv_feat_sox], [_SXE_CHECK_SOX])

	if test "$sxe_cv_feat_sox" = "yes"; then
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_MM_CHECK_SOX

AC_DEFUN([_SXE_CHECK_SOX], [dnl
	AC_REQUIRE([SXE_CHECK_SOX_LOCATIONS])

	if test "$have_smelly_sox" = "yes"; then
		sxe_cv_feat_sox="no"
	else
		SXE_CHECK_SOX_HEADERS
		SXE_CHECK_SOX_LIBS
		SXE_CHECK_SOX_STRUCTS

		if test "$ac_cv_lib_sox_sox_open_read" = "yes" -a \
			"$ac_cv_lib_sox_sox_close" = "yes" -a \
			"$ac_cv_lib_sox_sox_seek" = "yes" -a \
			"$ac_cv_header_sox_h" = "yes" -a \
			"$ac_cv_type_sox_format_t" = "yes" -a \
			"$ac_cv_type_struct_sox_format_t" = "yes" -a \
			"$sxe_cv_mm_sox_open_read_fooked" != "yes"; then
			sxe_cv_feat_sox="yes"
		else
			sxe_cv_feat_sox="no"
		fi
		:
	fi
])dnl _SXE_CHECK_SOX

AC_DEFUN([SXE_CHECK_SOX_LOCATIONS], [dnl
	PKG_CHECK_MODULES([SOX], [sox >= 14.1.0], [dnl
		have_smelly_sox="no"
		sox_cppflags="$SOX_CFLAGS"
		sox_ldflags=
		sox_libs="$SOX_LIBS"], [dnl
		have_smelly_sox="yes"
		AC_MSG_WARN([Your SoX is too old or non-existant. You need >= 14.1.0])])
])dnl SXE_CHECK_SOX_PLACES

AC_DEFUN([SXE_PUMP_SOX_LOCATIONS], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $sox_cppflags"
	LDFLAGS="$LDFLAGS $sox_ldflags"
	LIBS="$LIBS $sox_libs"
])dnl SXE_PUMP_SOX_LOCATIONS

AC_DEFUN([SXE_DUMP_SOX_LOCATIONS], [dnl
	SXE_RESTORE_LIBS
])dnl SXE_DUMP_SOX_LOCATIONS

AC_DEFUN([SXE_CHECK_SOX_HEADERS], [dnl
	AC_REQUIRE([SXE_CHECK_SOX_LOCATIONS])
	SXE_PUMP_SOX_LOCATIONS
	AC_CHECK_HEADERS([sox.h])
	SXE_DUMP_SOX_LOCATIONS
])dnl SXE_CHECK_SOX_HEADERS

AC_DEFUN([SXE_CHECK_SOX_LIBS], [dnl
	AC_REQUIRE([SXE_CHECK_SOX_LOCATIONS])

	echo "void cleanup(void) {}" > cleanup.c
	$CC -c -o cleanup.o cleanup.c

	SXE_PUMP_SOX_LOCATIONS
	## checks for the spankin' new sox
	AC_CHECK_LIB([sox], [sox_open_read], [:], [:],
		[cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([sox], [sox_close], [:], [:], [cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([sox], [sox_read], [:], [:], [cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([sox], [sox_seek], [:], [:], [cleanup.o $sox_ldflags $sox_libs])

	SXE_DUMP_SOX_LOCATIONS

	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS ${SOX_CPPFLAGS}"
	SXE_LANG_WERROR([on])
	SXE_MSG_CHECKING([whether sox_open_read() takes 4 arguments])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if defined HAVE_ERRNO_H
# include <errno.h>
#endif
#if defined HAVE_SOX_H
# include <sox.h>
#endif
		]], [
		sox_open_read("tmp", NULL, NULL, NULL);
		])],
		[sxe_cv_mm_sox_open_read_4args="yes"],
		[sxe_cv_mm_sox_open_read_4args="no"])
	SXE_MSG_RESULT([$sxe_cv_mm_sox_open_read_4args])

	SXE_MSG_CHECKING([whether sox_open_read() takes 3 arguments])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if defined HAVE_ERRNO_H
# include <errno.h>
#endif
#if defined HAVE_SOX_H
# include <sox.h>
#endif
		]], [
		sox_open_read("tmp", NULL, NULL);
		])],
		[sxe_cv_mm_sox_open_read_3args="yes"],
		[sxe_cv_mm_sox_open_read_3args="no"])
	SXE_MSG_RESULT([$sxe_cv_mm_sox_open_read_3args])
	SXE_RESTORE_LIBS

	if test "$sxe_cv_mm_sox_open_read_3args" = "yes"; then
		AC_DEFINE([HAVE_SOX_OPEN_READ_3ARGS], [1],
			[Whether sox_open_read() takes 3 arguments.])
		AC_DEFINE([HAVE_SOX_OPEN_READ_NARGS], [3],
			[How many arguments sox_open_read() takes.])
	elif test "$sxe_cv_mm_sox_open_read_4args" = "yes"; then
		AC_DEFINE([HAVE_SOX_OPEN_READ_4ARGS], [1],
			[Whether sox_open_read() takes 4 arguments.])
		AC_DEFINE([HAVE_SOX_OPEN_READ_NARGS], [4],
			[How many arguments sox_open_read() takes.])
	else
		AC_MSG_NOTICE([
Very weird SoX you've got there.
I better disable SoX on your behalf.
			])
		sxe_cv_mm_sox_open_read_fooked="yes"
	fi

	## clean up our cleanup snack
	rm -f cleanup.c cleanup.o
])dnl SXE_CHECK_SOX_LIBS

AC_DEFUN([SXE_CHECK_SOX_STRUCTS], [dnl
	## the new structs
	AC_CHECK_TYPES([sox_format_t], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([sox_ssize_t], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([sox_sample_t], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([sox_signalinfo_t], [:], [:], [
#if defined HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([struct sox_format_t], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_MEMBERS([struct sox_format_t.signal], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])

	AC_CHECK_MEMBERS([sox_signalinfo_t.precision], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
])dnl SXE_CHECK_SOX_STRUCTS


AC_DEFUN([SXE_MM_CHECK_MAD], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for mad support])
	AC_MSG_RESULT([])

	MM_SUCC
	SXE_CHECK_HEADERS([mad.h], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_synth_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_stream_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_frame_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_synth_frame], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_stream_buffer], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_frame_decode], [:], [MM_FAIL])

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_MAD

AC_DEFUN([SXE_MM_CHECK_MAGIC], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for libmagic/file support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	MM_SUCC
	AC_CHECK_LIB([magic], [magic_open], [:], [MM_FAIL])
	SXE_CHECK_HEADERS([magic.h], [:], [MM_FAIL])
	SXE_CHECK_HEADERS([file.h])
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_MAGIC


AC_DEFUN([SXE_MM_CHECK_OSS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for OSS support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	MM_FAIL
	SXE_CHECK_HEADERS([machine/soundcard.h sys/soundcard.h linux/soundcard.h soundcard.h])
	if test "$ac_cv_header_linux_soundcard_h"="yes" -o	\
		"$ac_cv_header_machine_soundcard_h"="yes" -o	\
		"$ac_cv_header_soundcard_h"="yes" -o		\
		"$ac_cv_header_sys_soundcard_h"="yes"
	then
		AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#if defined HAVE_MACHINE_SOUNDCARD_H && HAVE_MACHINE_SOUNDCARD_H
#include <machine/soundcard.h>
#elif defined HAVE_SYS_SOUNDCARD_H && HAVE_SYS_SOUNDCARD_H
#include <sys/soundcard.h>
#elif defined HAVE_LINUX_SOUNDCARD_H && HAVE_LINUX_SOUNDCARD_H
#include <linux/soundcard.h>
#else
#include <soundcard.h>
#endif
]],
		[[ ioctl(0, SNDCTL_DSP_RESET, 0); ]])],
			[oss_ioctl_linked=yes],
			[oss_ioctl_linked=no])
		if test	"${oss_ioctl_linked}" = "no"
		then
			if test "${opsys}" = "netbsd"
			then
				AC_CHECK_LIB([ossaudio],[_oss_ioctl],
					[
						# netbsd needs this Linux OSS emulator library, just a ioctl
						# wrapper really
						MM_SUCC
						SXE_PREPEND([-lossaudio], [MM_LIBS])
					])
			fi
		else
			MM_SUCC
		fi
	fi
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_OSS

AC_DEFUN([SXE_MM_CHECK_PULSE], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	PKG_CHECK_MODULES([PULSE], [libpulse >= 2.0.0], [$1], [$2])
])dnl SXE_MM_CHECK_PULSE

AC_DEFUN([SXE_MM_CHECK_JACK], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([jack], [jack], [0.98.0], [dnl
		jack_client_open jack_get_ports jack_port_register dnl
		jack_set_process_callback jack_set_error_function dnl
		jack_on_shutdown jack_activate jack_connect jack_disconnect dnl
		jack_client_close jack_port_get_buffer], [jack/jack.h], [$1], [$2])
])dnl SXE_MM_CHECK_JACK

AC_DEFUN([SXE_MM_CHECK_AO], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([ao], [ao], [0.6], [dnl
		ao_initialize ao_driver_id ao_default_driver_id dnl
		ao_open_live ao_close ao_shutdown ao_play], [ao/ao.h], [$1], [$2])
])dnl SXE_MM_CHECK_AO

AC_DEFUN([SXE_MM_CHECK_ESD], [
	## arg #1: action on success
	## arg #2: action on failure
	SXE_SEARCH_CONFIG_PROG([esd-config])

	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for ESD support])
	AC_MSG_RESULT([])

	if test "$have_esd_config" = "no" -o -z "$ESD_CONFIG"; then
		AS_MESSAGE([*** esd-config not found.])
		AS_MESSAGE([*** Cannot check for ESD.])
		have_esd_config=no
		ESD_CONFIG=
		MM_FAIL
	fi

	if test "$have_esd_config" = "yes"; then
		SXE_DUMP_LIBS
		ESD_CPPFLAGS="`$ESD_CONFIG --cflags`"
		ESD_LDFLAGS="-L`$ESD_CONFIG --libs`"
		dnl SXE_APPEND_UNDUP([$esd_c_switch], [c_switch_site])
		dnl SXE_PREPEND([$esd_libs], [LIBS])
		CPPFLAGS="$CPPFLAGS $ESD_CPPFLAGS"
		LDFLAGS="$LDFLAGS $ESD_LDFLAGS"

		MM_SUCC
		SXE_CHECK_HEADERS([esd.h], [:], [MM_FAIL])
		AC_CHECK_LIB([esd], [esd_play_stream], [:], [MM_FAIL], [$esd_libs])

		## restore anything
		SXE_RESTORE_LIBS
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_ESD

AC_DEFUN([SXE_MM_CHECK_ALSA], [
	## call like this SXE_MM_CHECK_ALSA([<if-found>], [<if-not-found>])
	## defines HAVE_ALSA, HAVE_ALSA_SOUND
	## and sxe_cv_feat_alsa
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for ALSA support],
		[sxe_cv_feat_alsa], [_SXE_MM_CHECK_ALSA])

	if test "$sxe_cv_feat_alsa" = "yes"; then
		AC_DEFINE([HAVE_ALSA], [1],
			[Whether ALSA can be used as sound device])
		AC_DEFINE([HAVE_ALSA_SOUND], [1],
			[Whether ALSA can be used as sound device])
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi
	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_MM_CHECK_ALSA

AC_DEFUN([_SXE_MM_CHECK_ALSA], [dnl

	AC_REQUIRE([SXE_CHECK_ALSA_HEADERS])
	AC_REQUIRE([SXE_CHECK_ALSA_VERSION])
	AC_REQUIRE([SXE_CHECK_ALSA_LIBS])

	if test "$ac_cv_header_alsa_input_h" = "yes" -a \
		"$ac_cv_header_alsa_output_h" = "yes" -a \
		"$ac_cv_header_alsa_global_h" = "yes" -a \
		"$ac_cv_header_alsa_conf_h" = "yes" -a \
		"$ac_cv_header_alsa_pcm_h" = "yes" -a \
		"$ac_cv_header_alsa_error_h" = "yes" -a \
		\
		"$sxe_mm_alsa_version_supported_p" = "yes" -a \
		\
		"$ac_cv_lib_asound_snd_pcm_close" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_free" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_any" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_free" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_set_access" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_set_channels" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_set_format" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_set_rate_near" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_test_channels" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_hw_params_test_format" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_open" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_prepare" = "yes" -a \
		"$ac_cv_lib_asound_snd_pcm_writei" = "yes"; then
		sxe_cv_feat_alsa="yes"
	else
		sxe_cv_feat_alsa="no"
	fi
])dnl _SXE_MM_CHECK_ALSA

AC_DEFUN([SXE_CHECK_ALSA_HEADERS], [dnl
	## dump the current configuration
	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	SXE_CHECK_HEADERS([alsa/input.h alsa/output.h alsa/global.h])
	SXE_CHECK_HEADERS([alsa/conf.h], [:], [:], [[
#include <stdlib.h>
#include <stdio.h>
#if defined HAVE_ALSA_INPUT_H
# include <alsa/input.h>
#endif
#if defined HAVE_ALSA_OUTPUT_H
# include <alsa/output.h>
#endif
#if defined HAVE_ALSA_GLOBAL_H
# include <alsa/global.h>
#endif
]])
	SXE_CHECK_HEADERS([alsa/pcm.h alsa/error.h alsa/version.h], [:], [:], [[
#include <stdlib.h>
#include <stdio.h>
#if defined HAVE_ALSA_INPUT_H
# include <alsa/input.h>
#endif
#if defined HAVE_ALSA_OUTPUT_H
# include <alsa/output.h>
#endif
#if defined HAVE_ALSA_GLOBAL_H
# include <alsa/global.h>
#endif
#if defined HAVE_ALSA_CONF_H
# include <alsa/conf.h>
#endif
]])
	## restor everything
	SXE_RESTORE_LIBS
])dnl SXE_CHECK_ALSA_HEADERS

AC_DEFUN([SXE_CHECK_ALSA_VERSION], [dnl
	AC_REQUIRE([SXE_CHECK_ALSA_HEADERS])

	## dump the configuration
	SXE_DUMP_LIBS
	SXE_LANG_WERROR([off])
	AC_MSG_CHECKING([for alsa version])

	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <stdio.h>
#if defined HAVE_ALSA_INPUT_H
# include <alsa/input.h>
#endif
#if defined HAVE_ALSA_OUTPUT_H
# include <alsa/output.h>
#endif
#if defined HAVE_ALSA_GLOBAL_H
# include <alsa/global.h>
#endif
#if defined HAVE_ALSA_CONF_H
# include <alsa/conf.h>
#endif
#if defined HAVE_ALSA_PCM_H
# include <alsa/pcm.h>
#endif
#if defined HAVE_ALSA_ERROR_H
# include <alsa/error.h>
#endif
#if defined HAVE_ALSA_CONF_H
# include <alsa/version.h>
#endif

int main(int c, char *v[])
{
	fprintf(stdout, SND_LIB_VERSION_STR);
	return 0;
}]])], dnl [./conftest; alsa_subminor=$?],[alsa_subminor=$?],[alsa_subminor=0]
	[sxe_mm_alsa_version=$(./conftest)],
	[:], [sxe_mm_alsa_version="undeterminable"])

	case "${sxe_mm_alsa_version}" in
	0.*.* | 1.0.3* | 1.0.9* )
		AC_MSG_RESULT([${sxe_mm_alsa_version} (known to break)])
		AC_MSG_WARN([Your ALSA version is _KNOWN_ to fail! Do not say we have not warned you!])
		sxe_mm_alsa_version_supported_p="no"
		;;
	1.0.2 | 1.0.4* | 1.0.5* | 1.0.6* | 1.0.7* | 1.0.8* )
		AC_MSG_RESULT([${sxe_mm_alsa_version} (suspicious to break)])
		AC_MSG_WARN([Your ALSA version has not been tested. Do not be surprised if it fails!])
		sxe_mm_alsa_version_supported_p="no"
		;;
	1.0.10* | 1.0.11* | 1.0.12* | 1.0.13* | 1.0.14* | 1.0.15* | 1.0.16* )
		AC_MSG_RESULT([${sxe_mm_alsa_version} (sane)])
		sxe_mm_alsa_version_supported_p="yes"
		;;
	1.* )
		AC_MSG_RESULT([${sxe_mm_alsa_version} (unknown)])
		AC_MSG_NOTICE([Your ALSA version is unknown, however we are confident that it works.])
		AC_MSG_NOTICE([However, if we screwed up something, please report back!])
		sxe_mm_alsa_version_supported_p="yes"
		;;
	* )
		AC_MSG_RESULT([${sxe_mm_alsa_version} (unknown)])
		AC_MSG_WARN([Your ALSA version is unknown hence not supported!])
		sxe_mm_alsa_version_supported_p="no"
		;;
	esac

	## restore everything
	SXE_RESTORE_LIBS
])dnl SXE_CHECK_ALSA_VERSION

AC_DEFUN([SXE_CHECK_ALSA_LIBS], [dnl
	## dump the current configuration
	SXE_DUMP_LIBS

	AC_CHECK_LIB([asound], [snd_pcm_open], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_close], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_free], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_any], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_access], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_free], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_test_channels], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_test_format], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_channels], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_format], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_rate_near], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_prepare], [:], [:])
	AC_CHECK_LIB([asound], [snd_pcm_writei], [:], [:])

	## restore everything
	SXE_RESTORE_LIBS
])dnl SXE_CHECK_ALSA_LIBS



AC_DEFUN([SXE_MM_CHECK_NAS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for NAS support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS

	MM_SUCC
	## NAS is often stored inside the X hierarchy, so ...
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"
	SXE_CHECK_HEADERS([audio/audiolib.h], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuOpenServer], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuCloseServer], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuCreateFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuStartFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuStopFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuScanForTypedEvent], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuDispatchEvent], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetErrorHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetIOErrorHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuWriteElement], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetElements], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuRegisterEventHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetElementParameters], [:], [MM_FAIL])

	dnl If the nas library does not contain the error jump point,
	dnl then we force safer behavior.
	AC_EGREP_HEADER([AuXtErrorJump], [audio/Xtutil.h], [], [old_nas=yes])
	if test "$old_nas" = "yes"; then
		AC_DEFINE([NAS_NO_ERROR_JUMP], [1], [Description here!])
	fi

	## restore anything
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_NAS

dnl sxe-mm.m4 ends here
