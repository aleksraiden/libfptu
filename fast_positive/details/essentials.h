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

#pragma once
#include "fast_positive/details/api.h"

#include "fast_positive/details/utils.h"
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace fptu {

/* FIELD's TOKEN & TAG ---------------------------------------------------------

        1              1 0              0
   MSB> FEDCBA9876543210 FEDCBA9876543210 <LSB
        0oooooooooooooSD sssssssssssGGGGG <<== preplaced
        1~~~~~~~~~~~~CSQ IiiiiiiiiiiGGGGG <<== loose/inlay
                         sssssssssss11111 <<== hole

        o - offset of preplaced fields
        i - id for loose fields
        I - inlay flag
        G - genus
        s - the size in bytes ONLY for holes and preplaced fields

        D - designated NIL (for preplaced)
        Q - quiet absence (for loose)
        S - saturation instead of range checking
        C - collection/repeated (for loose)

   LY: Передвинуть type/genus к верхней границе младщих 16 бит. Это позволит
       сортировать дыры в rw-кортежах, а также искать с использованием SIMD.

------------------------------------------------------------------------------*/

enum fundamentals {
  ident_bitness = 11,
  genus_bitness = 5,
  unit_size = 4,
  unit_shift = 2,
  tuple_flags_bits = 2,
  max_tuple_units_brutto = 65535
  /* С размером кортежа связаны такие ограничения и компромиссы:
   *  1) Хотим хранить все смещения внутри кортежа как 16-битные числа,
   *     т.е. между заголовком поля и его данными может быть до 65535
   *     4-х байтовых юнитов. Это позволяет создать кортеж размером почти
   *     до 64K + 256К * 2, например если последнее поле в индексе будет
   *     максимального размера (16-битного смещение будет достаточно для
   *     ссылки на его начало из конца индекса). Однако, возникает масса
   *     сопутствующих проблем и особых случаев.
   *  2) В заголовке кортежа необходимо хранить кол-во loose-полей и полный
   *     размер кортежа. Если оставлять заголовок 4-байтным, а не увеличивать
   *     вдвое, то с учетом места под флажки/признаки для кодирования длины
   *     остается примерно 16 бит. Можно выкроить 17, но тогда возникают
   *     трудности при проверке и компактификации.
   *  3) Проверка кортежа и компактификация требует создания "карты полей".
   *     Если ограничить полный размер кортежа 65535 юнитами, то карта может
   *     быть построена в 16-ти битных координатах. Иначе требуется использовать
   *     32-е числа, что удваивает размер и трафик по памяти.
   *  4) Отказ от лимита в 65535 юнитов на полный размер кортежа также требует
   *     дополнительных проверок при управлениями данными полей и смещениями
   *     "дырок". Кроме проверок, при этом неизбежно могут возникать ситуации
   *     когда смещение не может быть сгенерировано/сохранено без перемещения
   *     полей внутри кортежа.
   *
   * По совокупности решено НЕ "натягивать сову на глобус" и принять ограничение
   * максимального полного/brutto размера кортежа в 65535 4-байтных юнитов:
   *  - максимальный брутто-размер кортежа 262140 байт = 256K - 4 байта.
   *  - максимальный нетто-размер кортежа 2621436 байт = 256K - 8 байт.
   *  - максимальный размер данных одного поля 2621432 байта = 256K - 12 байт,
   *    т.е. если имеется кортеж максимального размера с одним полем, то:
   *      - полный размер сериализованной формы кортежа = 262140 байт;
   *      - в эти 262140 байт входит 4 байта заголовка кортежа и 4 байта
   *        дескриптора/заголовка поля;
   *      - размер непосредстенно полезных данных самого поля = 262132 байта.
   */
  ,
  max_tuple_bytes_brutto = unit_size * max_tuple_units_brutto,
  max_tuple_units_netto = max_tuple_units_brutto - 1,
  max_tuple_bytes_netto = unit_size * max_tuple_units_netto,
  max_field_units = max_tuple_units_netto - 1,
  max_field_bytes = unit_size * max_field_units,
  max_fields = (1 << (16 - tuple_flags_bits)) - 1,
  buffer_enough =
      sizeof(size_t) * 16 + max_tuple_bytes_netto + max_fields * unit_size,
  buffer_limit = max_tuple_bytes_netto * 2,
};

enum configure {
  onstask_allocation_threshold = 2048,
  sort_index_threshold = 256
};

enum genus : unsigned /* тип данных, 5 бит */ {
  // variable length type
  text = 0,
  varbin = 1 /* variable length binary (up to 256K) & arrays */,
  nested = 2 /* nested tuple */,
  property = 3 /* { id:8, len:8, data[0 <= len <= 253] } */,

  i8 = 4,
  boolean = i8,
  u8 = 5,
  b8 = u8,

  i16 = 6,
  enumeration = i16,
  u16 = 7,
  b16 = u16,

  i32 = 8,
  u32 = 9,
  b32 = u32,
  f32 = 10 /* Single precision IEEE-754 */,
  t32 = 11 /* 32-bit UTC time_t */,

  i64 = 12,
  u64 = 13,
  b64 = u64,
  f64 = 14 /* Double precision IEEE-754 */,
  d64 = 15 /* https://en.wikipedia.org/wiki/Decimal_floating_point */,
  t64 = 16 /* 64-bit fixed-point UTC datetime & timestamp */,

  // fixbin
  b96 = 17,
  b128 = 18,
  b160 = 19,
  b192 = 20,
  b224 = 21,
  b256 = 22,
  b320 = 23,
  b384 = 24,
  b512 = 25,

  // application-specific with predefined size and DENIL=0
  app_reserved_64 = 26,
  app_reserved_128 = 27,
  mac = 28,
  ip = 29,
  ipnet = 30,

  // auxiliary internal
  hole = 31
};

namespace details {

using tag_t = uint32_t;
using genus_mask_t = uint32_t;
using unit_t = uint32_t;

constexpr std::size_t bytes2units(const std::size_t bytes) noexcept {
  return (bytes + unit_size - 1) >> unit_shift;
}
constexpr std::size_t units2bytes(const std::size_t units) noexcept {
  return units << unit_shift;
}

static constexpr bool is_fixed_size(const genus type) noexcept {
  constexpr_assert(type != hole);
  return type > property;
}

static constexpr bool is_fixed_size(const tag_t tag) noexcept {
  return (tag & 0b11100) != 0;
}

static constexpr bool is_inplaced(const genus type) noexcept {
  return utils::test_bit(
      utils::bitset_mask<i16, u16, i8, u8, boolean, enumeration>::value, type);
}

enum tag_bits : uint32_t {
  genus_mask = (1 << genus_bitness) - 1,
  inlay_flag = 1u << 15,
  denil_flag = 1u << 16,
  quietabsence_flag = 1u << 16,
  saturation_flag = 1u << 17,
  collection_flag = 1u << 18,
  loose_flag = 1u << 31,
  offset_shift = 18,
  id_shift = genus_bitness,
  max_preplaced_offset = (1u << (32 - offset_shift - 1)) - 1,
  max_ident = (1u << ident_bitness) - 1,

  loose_begin = 0,
  loose_end = 1024 /* (1u << fundamentals::ident_bitness) / 2 */,
  inlay_begin = loose_end,
  inlay_end = 2048 /* 1u << fundamentals::ident_bitness */,

  loose_first = loose_begin,
  loose_last = loose_end - 1,
  inlay_first = inlay_begin,
  inlay_last = inlay_end - 1
};

static inline constexpr genus tag2genus(const tag_t tag) noexcept {
  return genus(tag & genus_mask);
}

static inline constexpr bool is_inplaced(const tag_t tag) noexcept {
  return is_inplaced(tag2genus(tag));
}

static inline constexpr bool is_loose(const tag_t tag) noexcept {
  return static_cast<int32_t>(tag) < 0;
}

static constexpr bool is_preplaced(const tag_t tag) noexcept {
  return static_cast<int32_t>(tag) >= 0;
}

static constexpr bool is_saturated(const tag_t tag) noexcept {
  return (tag & tag_bits::saturation_flag) != 0;
}

static constexpr bool is_rangechecking(const tag_t tag) noexcept {
  return (tag & tag_bits::saturation_flag) == 0;
}

static constexpr bool is_inlay(const tag_t tag) noexcept {
  constexpr_assert(is_loose(tag));
  return (tag & tag_bits::inlay_flag) != 0;
}

static constexpr bool is_loose_inlay(const tag_t tag) noexcept {
  return (tag & (tag_bits::inlay_flag | tag_bits::loose_flag)) ==
         (tag_bits::inlay_flag | tag_bits::loose_flag);
}

static constexpr bool is_collection(const tag_t tag) noexcept {
  constexpr_assert(is_loose(tag));
  return (tag & tag_bits::collection_flag) != 0;
}

static constexpr bool is_loose_collection(const tag_t tag) noexcept {
  return (tag & (tag_bits::collection_flag | tag_bits::loose_flag)) ==
         (tag_bits::collection_flag | tag_bits::loose_flag);
}

static constexpr bool distinct_null(const tag_t tag) noexcept {
  constexpr_assert(is_preplaced(tag));
  return (tag & tag_bits::denil_flag) != 0;
}

static constexpr bool is_quietabsence(const tag_t tag) noexcept {
  constexpr_assert(is_loose(tag));
  return (tag & tag_bits::quietabsence_flag) != 0;
}

static constexpr std::size_t tag2offset(const tag_t tag) noexcept {
  constexpr_assert(is_preplaced(tag));
  return tag >> offset_shift;
}

static constexpr std::size_t tag2indysize(const tag_t tag) noexcept {
  constexpr_assert(is_preplaced(tag));
  return static_cast<uint16_t>(tag) >> id_shift;
}

static constexpr unsigned tag2id(const tag_t tag) noexcept {
  constexpr_assert(is_loose(tag));
  return static_cast<uint16_t>(tag) >> id_shift;
}

static constexpr unsigned tag2id(const uint16_t genius_and_id) noexcept {
  return genius_and_id >> id_shift;
}

static constexpr tag_t make_tag(const genus type, const unsigned id,
                                const bool collection,
                                const bool quietabsence = false,
                                const bool saturated = false) noexcept {
  constexpr_assert(type < hole && id <= tag_bits::max_ident);
  return type + (id << id_shift) +
         (collection ? tag_bits::collection_flag | tag_bits::loose_flag
                     : tag_bits::loose_flag) +
         (quietabsence ? tag_bits::quietabsence_flag : 0u) +
         (saturated ? tag_bits::saturation_flag : 0u);
}

static constexpr tag_t make_hole(const std::size_t units) noexcept {
  constexpr_assert(units <= tag_bits::max_ident);
  return tag_t(genus::hole | tag_bits::collection_flag | tag_bits::loose_flag) +
         tag_t(units << genus_bitness);
}

static constexpr tag_t tag_from_offset(const std::size_t offset,
                                       const genus type,
                                       const std::size_t indysize,
                                       const bool deniled,
                                       const bool saturated) noexcept {
  constexpr_assert(type <= hole && offset <= max_preplaced_offset);
  constexpr_assert(indysize > 0 && indysize < tag_bits::max_ident);
  return tag_t(type + (offset << offset_shift) + (indysize << id_shift) +
               (deniled ? tag_bits::denil_flag : 0u) +
               (saturated ? tag_bits::saturation_flag : 0u));
}

//------------------------------------------------------------------------------

static constexpr genus_mask_t mask_all_types =
    UINT32_MAX - utils::bitset_mask<hole>::value;

static constexpr genus_mask_t mask_integer =
    utils::bitset_mask<i8, u8, i16, u16, i32, u32, i64, u64>::value;

static constexpr genus_mask_t mask_float =
    utils::bitset_mask<f32, f64, d64>::value;

static constexpr genus_mask_t mask_signed =
    utils::bitset_mask<i8, i16, i32, i64>::value | mask_float;

static constexpr genus_mask_t mask_unsigned = mask_integer & ~mask_signed;

static constexpr genus_mask_t mask_number = mask_integer | mask_float;

} // namespace details
} // namespace fptu
