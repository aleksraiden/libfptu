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

#include "fast_positive/tuples/internal.h"
#include "fast_positive/tuples/legacy.h"

struct FPTU_API_TYPE fptu_rw : public fptu::details::tuple_rw {
#ifdef FRIEND_TEST
  FRIEND_TEST(Fetch, Base);
  FRIEND_TEST(Fetch, Variate);
  FRIEND_TEST(Remove, Base);
#endif
  using base = fptu::details::tuple_rw;
  const fptu_field *begin_index() const cxx11_noexcept {
    const auto begin = base::begin_index();
    return static_cast<const fptu_field *>(begin);
  }
  const fptu_field *end_index() const cxx11_noexcept {
    const auto end = base::end_index();
    return static_cast<const fptu_field *>(end);
  }
};

using last_error = std::pair<fptu_error, std::string>;

const last_error &fptu_set_error(fptu_error code, const char *message);
const last_error &fptu_set_error(const std::exception &e);

struct error_guard {
  int *ptr;

  error_guard(int *error) : ptr(error) {}
  ~error_guard() {
    if (ptr)
      *ptr = FPTU_OK;
  }

  void feed(fptu_error code, const char *message) {
    fptu_set_error(code, message);
    if (ptr) {
      *ptr = code;
      ptr = nullptr;
    }
  }

  void feed(const std::exception &e) {
    fptu_error code = fptu_set_error(e).first;
    if (ptr) {
      *ptr = code;
      ptr = nullptr;
    }
  }
};

//------------------------------------------------------------------------------

__extern_C FPTU_API const float fptu_fp32_denil_value;
static __inline float fptu_fp32_denil(void) { return fptu_fp32_denil_value; }
#define FPTU_DENIL_FP32 fptu_fp32_denil()
#define FPTU_DENIL_FP32_BIN UINT32_C(0xFFFFffff)

__extern_C FPTU_API const double fptu_fp64_denil_value;
static __inline double fptu_fp64_denil(void) { return fptu_fp64_denil_value; }
#define FPTU_DENIL_FP64 fptu_fp64_denil()
#define FPTU_DENIL_FP64_BIN UINT64_C(0xFFFFffffFFFFffff)

#define FPTU_DENIL_CSTR nullptr
#define FPTU_DENIL_FIXBIN nullptr
#define FPTU_DENIL_DATETIME fptu::datetime_t::from_fixedpoint_32dot32(0)

//------------------------------------------------------------------------------

namespace fptu_legacy {

inline unsigned get_colnum(fptu_tag_t tag) {
  return fptu::details::tag2id(legacy2tag(tag));
}

inline fptu_type get_type(fptu_tag_t tag) {
  const fptu::genus type = fptu::details::tag2genus(tag);
  return genus2legacy(type);
}

inline bool tag_is_fixedsize(fptu_tag_t tag) {
  return fptu::details::is_fixed_size(tag);
}

inline size_t tag_value_fixedsize(fptu_tag_t tag) {
  const fptu::genus type = fptu::details::tag2genus(tag);
  return fptu::value_fixed_size(type);
}

inline bool tag_is_dead(fptu_tag_t tag) {
  return fptu::details::tag2genus(tag) == fptu::genus::hole;
}

inline fptu_tag_t make_tag(unsigned column, fptu_type type) {
  return genus2legacy(fptu::details::tag2genus(type), column);
}

} // namespace fptu_legacy
