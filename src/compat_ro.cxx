﻿/*
 * Copyright 2016-2019 libfptu authors: please see AUTHORS file.
 *
 * This file is part of libfptu, aka "Fast Positive Tuples".
 *
 * libfptu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libfptu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libfptu.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * libfptu = { Fast Positive Tuples, aka Позитивные Кортежи }
 *
 * The kind of lightweight linearized tuples, which are extremely handy
 * to machining, including cases with shared memory.
 * Please see README.md at https://github.com/leo-yuriev/libfptu
 *
 * The Future will Positive. Всё будет хорошо.
 *
 * "Позитивные Кортежи" дают легковесное линейное представление небольших
 * JSON-подобных структур в экстремально удобной для машины форме,
 * в том числе при размещении в разделяемой памяти.
 */

#include "fast_positive/details/legacy_compat.h"

const char *fptu_check_ro_ex(fptu_ro ro, bool holes_are_not_allowed) noexcept {
  return fptu::details::tuple_ro::audit(ro.sys.iov_base, ro.sys.iov_len,
                                        nullptr, holes_are_not_allowed);
}

static inline constexpr bool is_hollow(const fptu_ro &ro) noexcept {
  return ro.sys.iov_len < sizeof(fptu::details::unit_t) ||
         static_cast<const fptu::details::tuple_ro *>(ro.sys.iov_base)
                 ->size() != ro.sys.iov_len;
}

bool fptu_is_empty_ro(fptu_ro ro) noexcept {
  return is_hollow(ro)
             ? true
             : static_cast<const fptu::details::tuple_ro *>(ro.sys.iov_base)
                       ->index_size() < 1;
}

static inline const fptu::details::tuple_ro *impl(const fptu_ro &ro) {
  if (unlikely(is_hollow(ro)))
    fptu::throw_tuple_hollow();
  return static_cast<const fptu::details::tuple_ro *>(ro.sys.iov_base);
}

const fptu_field *fptu_begin_ro(fptu_ro ro) noexcept {
  if (unlikely(is_hollow(ro)))
    return nullptr;

  const auto tuple =
      static_cast<const fptu::details::stretchy_value_tuple *>(ro.sys.iov_base);
  return static_cast<const fptu_field *>(tuple->begin_index());
}

const fptu_field *fptu_end_ro(fptu_ro ro) noexcept {
  if (unlikely(is_hollow(ro)))
    return nullptr;

  const auto tuple =
      static_cast<const fptu::details::stretchy_value_tuple *>(ro.sys.iov_base);
  return static_cast<const fptu_field *>(tuple->end_index());
}

const fptu_field *fptu_lookup_ro(fptu_ro ro, unsigned column,
                                 fptu_type_or_filter type_or_filter) noexcept {
  return fptu_first(fptu_begin_ro(ro), fptu_end_ro(ro), column, type_or_filter);
}

//------------------------------------------------------------------------------

#define FPTU_GET_IMPL(LEGACY, NAME, GENUS, RETURN_TYPE, THUNK_TYPE, DENIL)     \
  RETURN_TYPE fptu_get_##LEGACY(fptu_ro ro, unsigned column,                   \
                                int *error) noexcept {                         \
    error_guard raii(error);                                                   \
    try {                                                                      \
      const fptu::token id(fptu::genus::GENUS, column, false);                 \
      return THUNK_TYPE(impl(ro)->get_##NAME(id));                             \
    } catch (const std::exception &e) {                                        \
      raii.feed(e);                                                            \
      return DENIL;                                                            \
    }                                                                          \
  }

FPTU_GET_IMPL(uint16, u16, u16, uint_fast16_t, uint_fast16_t, FPTU_DENIL_UINT16)
FPTU_GET_IMPL(bool, bool, boolean, bool, bool, false)
FPTU_GET_IMPL(int32, i32, i32, int_fast32_t, int_fast32_t, FPTU_DENIL_SINT32)
FPTU_GET_IMPL(uint32, u32, u32, uint_fast32_t, uint_fast32_t, FPTU_DENIL_UINT32)
FPTU_GET_IMPL(int64, i64, i64, int_fast64_t, int_fast64_t, FPTU_DENIL_SINT64)
FPTU_GET_IMPL(uint64, u64, u64, uint_fast64_t, uint_fast64_t, FPTU_DENIL_UINT64)
FPTU_GET_IMPL(fp64, f64, f64, double_t, double_t, FPTU_DENIL_FP64)
FPTU_GET_IMPL(fp32, f32, f32, float_t, float_t, FPTU_DENIL_FP32)
FPTU_GET_IMPL(datetime, datetime, t64, fptu_datetime_C, fptu_datetime_C,
              FPTU_DENIL_DATETIME)

FPTU_GET_IMPL(opaque, varbinary, varbin, ::iovec, fptu::details::iovec_thunk,
              fptu::details::iovec_thunk())
FPTU_GET_IMPL(nested, nested, nested, fptu_ro, fptu::details::iovec_thunk,
              fptu::details::iovec_thunk())
#undef FPTU_GET_IMPL

#define FPTU_GET_IMPL(BITS)                                                    \
  const uint8_t *fptu_get_##BITS(fptu_ro ro, unsigned column,                  \
                                 int *error) noexcept {                        \
    error_guard raii(error);                                                   \
    try {                                                                      \
      const fptu::token id(fptu::genus::b##BITS, column, false);               \
      return erthink::constexpr_pointer_cast<const uint8_t *>(                 \
          &impl(ro)->get_b##BITS(id));                                         \
    } catch (const std::exception &e) {                                        \
      raii.feed(e);                                                            \
      return nullptr;                                                          \
    }                                                                          \
  }

FPTU_GET_IMPL(96)
FPTU_GET_IMPL(128)
FPTU_GET_IMPL(160)
FPTU_GET_IMPL(256)
#undef FPTU_GET_IMPL

const char *fptu_get_cstr(fptu_ro ro, unsigned column, int *error) noexcept {
  error_guard raii(error);
  try {
    const fptu::token id(fptu::genus::text, column, false);
    const fptu::string_view value(impl(ro)->get_string(id));
    if (value.empty())
      return "";

    const char *const cstr_end = value.cend();
    const char *const tuple_end =
        erthink::constexpr_pointer_cast<const char *>(ro.sys.iov_base) +
        ro.sys.iov_len;
    if (cstr_end < tuple_end && *cstr_end == '\0')
      return value.data();

    /* LY: UNSAFE(!) crutch for returning legacy c-string */
    static thread_local std::string holder = value;
    return holder.c_str();
  } catch (const std::exception &e) {
    raii.feed(e);
    return "";
  }
}

//------------------------------------------------------------------------------

#define FPTU_GET_IMPL(BITS)                                                    \
  fptu_lge fptu_cmp_##BITS(fptu_ro ro, unsigned column,                        \
                           const uint8_t *value) noexcept {                    \
    if (unlikely(value == nullptr))                                            \
      return fptu_ic;                                                          \
    const fptu_field *pf = fptu::lookup(ro, column, fptu_##BITS);              \
    if (unlikely(pf == nullptr))                                               \
      return fptu_ic;                                                          \
    return fptu::cmpbin(pf->relative.payload()->flat, value, BITS / 8);        \
  }

FPTU_GET_IMPL(96)
FPTU_GET_IMPL(128)
FPTU_GET_IMPL(160)
FPTU_GET_IMPL(256)
#undef FPTU_GET_IMPL

fptu_lge fptu_cmp_opaque(fptu_ro ro, unsigned column, const void *value,
                         std::size_t bytes) noexcept {
  const fptu_field *pf = fptu::lookup(ro, column, fptu_opaque);
  if (pf == nullptr)
    return bytes ? fptu_ic : fptu_eq;

  const struct iovec iov = fptu_field_opaque(pf);
  return fptu_cmp_binary(iov.iov_base, iov.iov_len, value, bytes);
}

fptu_lge fptu_cmp_opaque_iov(fptu_ro ro, unsigned column,
                             const struct iovec value) noexcept {
  return fptu_cmp_opaque(ro, column, value.iov_base, value.iov_len);
}
