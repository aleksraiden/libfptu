/*
 *  Fast Positive Tuples (libfptu), aka Позитивные Кортежи
 *  Copyright 2021 Leonid Yuriev <leo@yuriev.ru>
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

#ifndef __AVX512BW__
#warning "The -mavx512bw or /arch:AVX512 compiler's option is required"
#else

#include "fast_positive/erthink/erthink_intrin.h"
#include "fast_positive/tuples/api.h"
#include "fast_positive/tuples/details/field.h"
#include "fast_positive/tuples/details/scan.h"
#include <cassert>
#include <cstddef>
#include <cstdint>

#include "fast_positive/erthink/erthink_optimize4speed.h"

#if defined(_MSC_VER) && _MSC_VER < 1920
#pragma warning(disable : 4310) /* cast truncates constant value */
#endif                          /* _MSC_VER < 1920 */

namespace fptu {
namespace details {

static __always_inline unsigned cmp2mask(const __m512i pattern,
                                         const field_loose *scan) {
  return _mm512_cmpeq_epi16_mask(pattern,
                                 _mm512_loadu_si512((const __m512i *)scan));
}

#if __SANITIZE_ADDRESS__
static __m512i __attribute__((no_sanitize_address, noinline))
bypass_asan_read_m512(const void *src) {
  return _mm512_loadu_si512((const __m512i *)src);
}
#define cmp2mask_bypass_asan(pattern, scan)                                    \
  ({                                                                           \
    __m512i copy = bypass_asan_read_m512(scan);                                \
    cmp2mask(pattern, (field_loose *)&copy);                                   \
  })
#else
#define cmp2mask_bypass_asan(pattern, scan) cmp2mask(pattern, scan)
#endif

static __always_inline bool mask2ptr(unsigned mask, const field_loose *&ptr) {
  unsigned long index;
#ifdef _MSC_VER
  if (!_BitScanForward(&index, mask))
    return false;
#else
  if (likely(!mask))
    return false;
  index = __builtin_ctz(mask);
#endif
  assert((index - 1) % 2 == 0);
  ptr = (const field_loose *)((char *)ptr + index * 2 - 2);
  return true;
}

#define STEP_x16                                                               \
  do {                                                                         \
    if (mask2ptr(0xAAAAAAAAu & cmp2mask(pattern, scan), scan)) {               \
      assert(scan->genus_and_id == genus_and_id);                              \
      return scan;                                                             \
    }                                                                          \
    scan += 16;                                                                \
  } while (0)

__hot const field_loose *fptu_scan_AVX512(const field_loose *begin,
                                          const field_loose *end,
                                          const uint16_t genus_and_id) {
  const ptrdiff_t bytes = (char *)end - (char *)begin;
  assert(bytes % 4 == 0);

  const __m512i pattern = _mm512_set1_epi16(genus_and_id);
  const field_loose *scan = begin;

  if (unlikely(bytes < 16 * 4)) {
    if (unlikely(bytes < 4))
      /* empty or hollow tuple case */
      return nullptr;

    unsigned mask = 0xAAAAAAAAu;
    if (likely(/* check for enough on-page offset for '-64' */ 0xfc0 &
               (uintptr_t)scan)) {
      mask &= cmp2mask_bypass_asan(pattern, end - 16);
      mask >>= ((64 - bytes) >> 1);
    } else {
      mask >>= ((64 - bytes) >> 1);
      mask &= cmp2mask_bypass_asan(pattern, scan);
    }
    if (mask2ptr(mask, scan)) {
      assert(scan->genus_and_id == genus_and_id);
      return scan;
    }
    return nullptr;
  }

  std::size_t chunks = (bytes + 63) >> 6;
  while (1)
    switch (chunks) {
    default:
      do {
        STEP_x16;
        STEP_x16;
        STEP_x16;
        STEP_x16;
        STEP_x16;
        STEP_x16;
        STEP_x16;
        STEP_x16;
      } while (scan < end - 127);
      assert((bytes & 511) == ((char *)end - (char *)scan));
      chunks = ((bytes & 511) + 63) >> 6;
      assert((ptrdiff_t)chunks == ((char *)end - (char *)scan + 63) / 64);
      continue;

    case 8:
      STEP_x16;
      __fallthrough /* fall through */;
    case 7:
      STEP_x16;
      __fallthrough /* fall through */;
    case 6:
      STEP_x16;
      __fallthrough /* fall through */;
    case 5:
      STEP_x16;
      __fallthrough /* fall through */;
    case 4:
      STEP_x16;
      __fallthrough /* fall through */;
    case 3:
      STEP_x16;
      __fallthrough /* fall through */;
    case 2:
      STEP_x16;
      __fallthrough /* fall through */;
    case 1:
      scan = end - 16;
      assert(begin <= scan);
      STEP_x16;
      __fallthrough /* fall through */;
    case 0:
      return nullptr;
    }
}

} // namespace details
} // namespace fptu
#endif /* __AVX512BW__ */
