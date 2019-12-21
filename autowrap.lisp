(in-package #:freetype-ffi)

(setf autowrap:*c2ffi-program* "/usr/local/bin/c2ffi")

(autowrap:c-include
 '(freetype autowrap-spec "freetype.h")
 :spec-path '(freetype autowrap-spec)
 :sysincludes '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/11.0.0/include/")
 :exclude-sources ("/usr/include/")
 :exclude-definitions ("va_list"
                       #+windows
                       "ptrdiff_t"
                       #+windows
                       "size_t")
 :no-accessors t
 :no-functions t)


