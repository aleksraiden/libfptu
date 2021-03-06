/*
 *  Fast Positive Tuples (libfptu), aka Позитивные Кортежи
 *  Copyright 2016-2020 Leonid Yuriev <leo@yuriev.ru>
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

#pragma once

#include "fast_positive/erthink/erthink_arch.h"
#include "fast_positive/erthink/erthink_ifunc.h"
#include "fast_positive/tuples/api.h"
#include "fast_positive/tuples/config.h"
#include "fast_positive/tuples/details/field.h"
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace fptu {
namespace details {

typedef const field_loose *(*scan_func_t)(const field_loose *begin,
                                          const field_loose *end,
                                          const uint16_t genus_and_id);

/* Artless reference implementation for testing & verification */
__extern_C FPTU_API const field_loose *
fptu_scan_referential(const field_loose *begin, const field_loose *end,
                      const uint16_t genus_and_id);

__extern_C FPTU_API const field_loose *
fptu_scan_unroll(const field_loose *begin, const field_loose *end,
                 const uint16_t genus_and_id);

#if defined(__ia32__) || defined(__SSE2__)
__extern_C FPTU_API const field_loose *
fptu_scan_SSE2(const field_loose *begin, const field_loose *end,
               const uint16_t genus_and_id);
#endif /* __ia32__ || __SSE2__ */

#if defined(__ia32__) || defined(__AVX__)
__extern_C FPTU_API const field_loose *
fptu_scan_AVX(const field_loose *begin, const field_loose *end,
              const uint16_t genus_and_id);
#endif /* __ia32__ || __AVX__ */

#if defined(__ia32__) || defined(__AVX2__)
__extern_C FPTU_API const field_loose *
fptu_scan_AVX2(const field_loose *begin, const field_loose *end,
               const uint16_t genus_and_id);
#endif /* __ia32__ || __AVX2__ */

#if FPTU_HAVE_AVX512 || defined(__AVX512BW__)
__extern_C FPTU_API const field_loose *
fptu_scan_AVX512(const field_loose *begin, const field_loose *end,
                 const uint16_t genus_and_id);
#endif /* FPTU_HAVE_AVX512 */

#if defined(__AVX512BW__)
#define fptu_scan(begin, end, genus_and_id)                                    \
  fptu_scan_AVX512(begin, end, genus_and_id)
#elif defined(__AVX2__)
#define fptu_scan(begin, end, genus_and_id)                                    \
  fptu_scan_AVX2(begin, end, genus_and_id)
#elif defined(__AVX__)
#define fptu_scan(begin, end, genus_and_id)                                    \
  fptu_scan_AVX(begin, end, genus_and_id)
#elif defined(__SSE2__)
#define fptu_scan(begin, end, genus_and_id)                                    \
  fptu_scan_SSE2(begin, end, genus_and_id)
#elif defined(__ia32__)
ERTHINK_DECLARE_IFUNC(FPTU_API, const field_loose *, fptu_scan,
                      (const field_loose *begin, const field_loose *end,
                       const uint16_t genus_and_id),
                      (begin, end, genus_and_id), fptu_scan_resolver)
#else
#define fptu_scan(begin, end, genus_and_id)                                    \
  fptu_scan_unroll(begin, end, genus_and_id)
#endif

//------------------------------------------------------------------------------

static __pure_function inline const field_loose *
lookup(bool sorted, const field_loose *begin, const field_loose *end,
       const tag_t tag) cxx11_noexcept {
  assert(is_loose(tag));
  static_assert(sizeof(field_loose) == 4 && sizeof(unit_t) == 4, "WTF?");
  (void)/* TODO: use search() for sorted tuples */ sorted;
  return fptu_scan(begin, end, uint16_t(tag));
}

static __pure_function inline const field_loose *
next(const field_loose *current, const field_loose *end,
     const tag_t tag) cxx11_noexcept {
  assert(is_loose(tag));
  static_assert(sizeof(field_loose) == 4 && sizeof(unit_t) == 4, "WTF?");
  return current ? fptu_scan(current + 1, end, uint16_t(tag)) : current;
}

} // namespace details
} // namespace fptu
