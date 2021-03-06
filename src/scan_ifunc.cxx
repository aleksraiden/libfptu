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

#include "fast_positive/erthink/erthink_defs.h"
#include "fast_positive/tuples/details/cpu_features.h"
#include "fast_positive/tuples/details/field.h"
#include "fast_positive/tuples/details/scan.h"
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace fptu {
namespace details {

#ifndef fptu_scan

ERTHINK_DEFINE_IFUNC(FPTU_API, const field_loose *, fptu_scan,
                     (const field_loose *begin, const field_loose *end,
                      const uint16_t genus_and_id),
                     (begin, end, genus_and_id), fptu_scan_resolver)

ERTHINK_IFUNC_RESOLVER_API(FPTU_API)
__cold scan_func_t fptu_scan_resolver() {
#ifndef __SANITIZE_ADDRESS__
#if defined(__ia32__)
#if FPTU_HAVE_AVX512
  if (cpu_features.has_AVX512_BW())
    return fptu_scan_AVX512;
#endif /* FPTU_HAVE_AVX512 */
  if (cpu_features.has_AVX2())
    return fptu_scan_AVX2;
  if (cpu_features.has_AVX())
    return fptu_scan_AVX;
  if (cpu_features.has_SSE2())
    return fptu_scan_SSE2;
#else
#warning "FIXME: Support for other architectures"
#endif
#endif /* ! __SANITIZE_ADDRESS__ */

  return fptu_scan_unroll;
}

#endif /* fptu_scan */

} // namespace details
} // namespace fptu
