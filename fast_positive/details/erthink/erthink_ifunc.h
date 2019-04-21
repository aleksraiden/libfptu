﻿/*
 *  Copyright (c) 1994-2019 Leonid Yuriev <leo@yuriev.ru>.
 *  https://github.com/leo-yuriev/erthink
 *  ZLib License
 *
 *  This software is provided 'as-is', without any express or implied
 *  warranty. In no event will the authors be held liable for any damages
 *  arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute it
 *  freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented; you must not
 *     claim that you wrote the original software. If you use this software
 *     in a product, an acknowledgement in the product documentation would be
 *     appreciated but is not required.
 *  2. Altered source versions must be plainly marked as such, and must not be
 *     misrepresented as being the original software.
 *  3. This notice may not be removed or altered from any source distribution.
 */

#pragma once
#include "erthink_defs.h"

//------------------------------------------------------------------------------

/* GNU ELF indirect functions usage control. For more info please see
 * https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
 * and https://sourceware.org/glibc/wiki/GNU_IFUNC */

#ifndef ERTHINK_USE_ELF_IFUNC
#if __has_attribute(ifunc) &&                                                  \
    defined(__ELF__) /* ifunc is broken on Darwin/OSX */
/* Use ifunc/gnu_indirect_function if corresponding attribute is available,
 * Assuming compiler will generate properly code even when
 * the -fstack-protector-all and/or the -fsanitize=address are enabled. */
#define ERTHINK_USE_ELF_IFUNC 1
#elif defined(__ELF__) && !defined(__SANITIZE_ADDRESS__) &&                    \
    !defined(__SSP_ALL__)
/* ifunc/gnu_indirect_function will be used on ELF, but only if both
 * -fstack-protector-all and -fsanitize=address are NOT enabled. */
#define ERTHINK_USE_ELF_IFUNC 1
#else
#define ERTHINK_USE_ELF_IFUNC 0
#endif
#endif /* ERTHINK_USE_ELF_IFUNC */

//------------------------------------------------------------------------------

#if ERTHINK_USE_ELF_IFUNC
#define ERTHINK_IFUNC_RESOLVER_API(API_VISIBILITY) __extern_C

#define ERTHINK_DECLARE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,               \
                              DECLARGS_PARENTHESIZED, CALLARGS_PARENTHESIZED,  \
                              RESOLVER)                                        \
  __extern_C API_VISIBILITY RESULT_TYPE NAME DECLARGS_PARENTHESIZED;

#if __has_attribute(ifunc) ||                                                  \
    (defined(__ELF__) && __GLIBC_PREREQ(2, 11) && __GNUC_PREREQ(4, 6))

#define ERTHINK_DEFINE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,                \
                             DECLARGS_PARENTHESIZED, CALLARGS_PARENTHESIZED,   \
                             RESOLVER)                                         \
                                                                               \
  ERTHINK_IFUNC_RESOLVER_API(API_VISIBILITY)                                   \
  RESULT_TYPE(*RESOLVER(void)) DECLARGS_PARENTHESIZED;                         \
                                                                               \
  RESULT_TYPE NAME DECLARGS_PARENTHESIZED                                      \
      __attribute__((ifunc(STRINGIFY(RESOLVER))));

#else

/* *INDENT-OFF* */
/* clang-format off */
#define ERTHINK_DEFINE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,                \
                          DECLARGS_PARENTHESIZED,                              \
                          CALLARGS_PARENTHESIZED, RESOLVER)                    \
                                                                               \
    __extern_C RESULT_TYPE (*RESOLVER(void)) DECLARGS_PARENTHESIZED;           \
                                                                               \
    __asm__("\t.globl\t" STRINGIFY(NAME) "\n"                                  \
            "\t.type\t" STRINGIFY(NAME) ", %gnu_indirect_function\n"           \
            "\t.set\t" STRINGIFY(NAME) "," STRINGIFY(RESOLVER)                 \
      )
/* *INDENT-ON* */
/* clang-format on */

#endif /* __has_attribute(ifunc) */

#else /* ERTHINK_USE_ELF_IFUNC */
#define ERTHINK_IFUNC_RESOLVER_API(API) static

#ifdef __cplusplus

#define ERTHINK_DECLARE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,               \
                              DECLARGS_PARENTHESIZED, CALLARGS_PARENTHESIZED,  \
                              RESOLVER)                                        \
                                                                               \
  __extern_C API_VISIBILITY RESULT_TYPE(*const NAME##_iFuncPtr)                \
      DECLARGS_PARENTHESIZED;                                                  \
                                                                               \
  static inline RESULT_TYPE NAME DECLARGS_PARENTHESIZED {                      \
    return NAME##_iFuncPtr CALLARGS_PARENTHESIZED;                             \
  }

#define ERTHINK_DEFINE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,                \
                             DECLARGS_PARENTHESIZED, CALLARGS_PARENTHESIZED,   \
                             RESOLVER)                                         \
                                                                               \
  ERTHINK_IFUNC_RESOLVER_API(API_VISIBILITY)                                   \
  RESULT_TYPE(*RESOLVER(void)) DECLARGS_PARENTHESIZED;                         \
                                                                               \
  RESULT_TYPE(*const NAME##_iFuncPtr) DECLARGS_PARENTHESIZED = RESOLVER();

#else /* __cplusplus */

#define ERTHINK_DECLARE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,               \
                              DECLARGS_PARENTHESIZED, CALLARGS_PARENTHESIZED,  \
                              RESOLVER)                                        \
                                                                               \
  __extern_C API_VISIBILITY RESULT_TYPE(*NAME##_iFuncPtr)                      \
      DECLARGS_PARENTHESIZED;                                                  \
                                                                               \
  static __inline RESULT_TYPE NAME DECLARGS_PARENTHESIZED {                    \
    return NAME##_iFuncPtr CALLARGS_PARENTHESIZED;                             \
  }

#if __GNUC_PREREQ(4, 0) || __has_attribute(constructor)

#define ERTHINK_DEFINE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,                \
                             DECLARGS_PARENTHESIZED, CALLARGS_PARENTHESIZED,   \
                             RESOLVER)                                         \
                                                                               \
  RESULT_TYPE(*NAME##_iFuncPtr) DECLARGS_PARENTHESIZED;                        \
                                                                               \
  ERTHINK_IFUNC_RESOLVER_API(API_VISIBILITY)                                   \
  RESULT_TYPE(*RESOLVER(void)) DECLARGS_PARENTHESIZED;                         \
                                                                               \
  static __cold void __attribute__((constructor)) NAME##_iFunc_init(void) {    \
    NAME##_iFuncPtr = RESOLVER();                                              \
  }

#else /* __has_attribute(constructor) */

#define ERTHINK_DEFINE_IFUNC(API_VISIBILITY, RESULT_TYPE, NAME,                \
                             DECLARGS_PARENTHESIZED, CALLARGS_PARENTHESIZED,   \
                             RESOLVER)                                         \
                                                                               \
  ERTHINK_IFUNC_RESOLVER_API(API_VISIBILITY)                                   \
  RESULT_TYPE(*RESOLVER(void)) DECLARGS_PARENTHESIZED;                         \
                                                                               \
  static __cold RESULT_TYPE NAME##_proxy DECLARGS_PARENTHESIZED {              \
    NAME##_iFuncPtr = RESOLVER();                                              \
    return NAME##_iFuncPtr CALLARGS_PARENTHESIZED;                             \
  }                                                                            \
                                                                               \
  RESULT_TYPE(*NAME##_iFuncPtr) DECLARGS_PARENTHESIZED = NAME##_proxy;

#endif /* __has_attribute(constructor) */

#endif /* __cplusplus */

#endif /* ERTHINK_USE_ELF_IFUNC */
