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

#include "fptu_test.h"

TEST(Fetch, Invalid) {
  fptu_ro ro;
  ro.total_bytes = 0;
  ro.units = nullptr;

  EXPECT_EQ(nullptr, fptu_fetch(ro, nullptr, 0, 0));
  EXPECT_EQ(nullptr, fptu_fetch(ro, nullptr, fptu_max_tuple_bytes / 2,
                                fptu::max_fields / 2));
  EXPECT_EQ(nullptr,
            fptu_fetch(ro, nullptr, fptu_max_tuple_bytes, fptu::max_fields));
  EXPECT_EQ(nullptr, fptu_fetch(ro, nullptr, ~0u, ~0u));

  char space_exactly_noitems[fptu_rw::pure_tuple_size()];
  EXPECT_EQ(nullptr, fptu_fetch(ro, space_exactly_noitems,
                                sizeof(space_exactly_noitems), 1));
  EXPECT_EQ(nullptr,
            fptu_fetch(ro, space_exactly_noitems, sizeof(space_exactly_noitems),
                       fptu::max_fields));
  EXPECT_EQ(nullptr, fptu_fetch(ro, nullptr, sizeof(space_exactly_noitems), 0));
  EXPECT_EQ(nullptr, fptu_fetch(ro, space_exactly_noitems,
                                sizeof(space_exactly_noitems) - 1, 0));
  EXPECT_EQ(nullptr, fptu_fetch(ro, space_exactly_noitems, 0, 0));
  EXPECT_EQ(nullptr, fptu_fetch(ro, space_exactly_noitems, 0, 1));
  EXPECT_EQ(nullptr,
            fptu_fetch(ro, space_exactly_noitems, 0, fptu::max_fields));
  EXPECT_EQ(nullptr,
            fptu_fetch(ro, space_exactly_noitems, 0, fptu::max_fields * 2));
  EXPECT_EQ(nullptr, fptu_fetch(ro, space_exactly_noitems, 0, ~0u));

  char space_maximum[fptu_rw::pure_tuple_size() + fptu_max_tuple_bytes];
  EXPECT_EQ(nullptr, fptu_fetch(ro, space_maximum, sizeof(space_maximum),
                                fptu::max_fields + 1));
  EXPECT_EQ(nullptr, fptu_fetch(ro, nullptr, sizeof(space_maximum), 0));
  EXPECT_EQ(nullptr, fptu_fetch(ro, space_exactly_noitems, ~0u, 1));
  ASSERT_EQ(nullptr, fptu_fetch(ro, space_exactly_noitems,
                                fptu_buffer_limit + 1, fptu::max_fields));

  fptu_rw *pt;
  pt = fptu_fetch(ro, space_exactly_noitems, sizeof(space_exactly_noitems), 0);
  ASSERT_NE(nullptr, pt);
  EXPECT_STREQ(nullptr, fptu::check(pt));
  pt = fptu_fetch(ro, space_maximum, sizeof(space_maximum), 0);
  ASSERT_NE(nullptr, pt);
  EXPECT_STREQ(nullptr, fptu::check(pt));
  pt = fptu_fetch(ro, space_maximum, sizeof(space_maximum), 1);
  ASSERT_NE(nullptr, pt);
  EXPECT_STREQ(nullptr, fptu::check(pt));
  pt = fptu_fetch(ro, space_maximum, sizeof(space_maximum),
                  fptu::max_fields / 2);
  ASSERT_NE(nullptr, pt);
  EXPECT_STREQ(nullptr, fptu::check(pt));
  pt = fptu_fetch(ro, space_maximum, sizeof(space_maximum), fptu::max_fields);
  ASSERT_NE(nullptr, pt);
  EXPECT_STREQ(nullptr, fptu::check(pt));
}

TEST(Fetch, Base) {
  char origin_space[fptu::buffer_enough];
  char fetched_space[fptu::buffer_enough];
  fptu_ro origin_ro, fetched_ro;
  fptu_rw *origin_pt, *fetched_pt;

  origin_pt = fptu_init(origin_space, sizeof(origin_space), fptu::max_fields);
  ASSERT_NE(nullptr, origin_pt);
  EXPECT_STREQ(nullptr, fptu::check(origin_pt));
  origin_ro = fptu_take_noshrink(origin_pt);
  ASSERT_STREQ(nullptr, fptu::check(origin_ro));
  EXPECT_EQ(fptu::unit_size, origin_ro.total_bytes);

  // check empty without more-items
  fetched_pt = fptu_fetch(origin_ro, fetched_space, sizeof(fetched_space), 0);
  ASSERT_NE(nullptr, fetched_pt);
  EXPECT_STREQ(nullptr, fptu::check(fetched_pt));

  fetched_ro = fptu_take_noshrink(fetched_pt);
  ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
  ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
  EXPECT_EQ(0,
            memcmp(origin_ro.units, fetched_ro.units, origin_ro.total_bytes));

  // check empty with max-more-items
  fetched_pt = fptu_fetch(origin_ro, fetched_space, sizeof(fetched_space),
                          fptu::max_fields);
  ASSERT_NE(nullptr, fetched_pt);
  EXPECT_STREQ(nullptr, fptu::check(fetched_pt));

  fetched_ro = fptu_take_noshrink(fetched_pt);
  ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
  ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
  EXPECT_EQ(0,
            memcmp(origin_ro.units, fetched_ro.units, origin_ro.total_bytes));

  EXPECT_EQ(origin_pt->end_, fetched_pt->end_);
  EXPECT_EQ(origin_pt->pivot_, fetched_pt->pivot_);
  EXPECT_EQ(origin_pt->tail_, fetched_pt->tail_);
  EXPECT_EQ(origin_pt->head_, fetched_pt->head_);
  EXPECT_EQ(origin_pt->junk_.count, fetched_pt->junk_.count);
  EXPECT_EQ(origin_pt->junk_.volume, fetched_pt->junk_.volume);

  // adds header-only fields and check
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(origin_pt, fptu_max_cols, 42));
  ASSERT_STREQ(nullptr, fptu::check(origin_pt));
  origin_ro = fptu_take_noshrink(origin_pt);
  ASSERT_STREQ(nullptr, fptu::check(origin_ro));
  EXPECT_EQ(fptu::unit_size * 2u, origin_ro.total_bytes);

  // check with max-more-items
  fetched_pt = fptu_fetch(origin_ro, fetched_space, sizeof(fetched_space),
                          fptu::max_fields);
  ASSERT_NE(nullptr, fetched_pt);
  EXPECT_STREQ(nullptr, fptu::check(fetched_pt));

  fetched_ro = fptu_take_noshrink(fetched_pt);
  ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
  ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
  EXPECT_EQ(0,
            memcmp(origin_ro.units, fetched_ro.units, origin_ro.total_bytes));

  EXPECT_EQ(origin_pt->end_, fetched_pt->end_);
  EXPECT_EQ(origin_pt->pivot_, fetched_pt->pivot_);
  EXPECT_EQ(origin_pt->tail_, fetched_pt->tail_);
  EXPECT_EQ(origin_pt->head_, fetched_pt->head_);
  EXPECT_EQ(origin_pt->junk_.count, fetched_pt->junk_.count);
  EXPECT_EQ(origin_pt->junk_.volume, fetched_pt->junk_.volume);

  // check without more-items
  fetched_pt = fptu_fetch(origin_ro, fetched_space, sizeof(fetched_space), 0);
  ASSERT_NE(nullptr, fetched_pt);
  EXPECT_STREQ(nullptr, fptu::check(fetched_pt));

  fetched_ro = fptu_take_noshrink(fetched_pt);
  ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
  ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
  EXPECT_EQ(0,
            memcmp(origin_ro.units, fetched_ro.units, origin_ro.total_bytes));

  // re-create, adds fileds with payload and check
  origin_pt = fptu_init(origin_space, sizeof(origin_space), fptu::max_fields);
  ASSERT_NE(nullptr, origin_pt);
  EXPECT_STREQ(nullptr, fptu::check(origin_pt));

  EXPECT_EQ(FPTU_OK, fptu_insert_uint32(origin_pt, fptu_max_cols, 42));
  ASSERT_STREQ(nullptr, fptu::check(origin_pt));
  origin_ro = fptu_take_noshrink(origin_pt);
  ASSERT_STREQ(nullptr, fptu::check(origin_ro));
  EXPECT_EQ(fptu::unit_size * 3u, origin_ro.total_bytes);

  // check with max-more-items
  fetched_pt = fptu_fetch(origin_ro, fetched_space, sizeof(fetched_space),
                          fptu::max_fields);
  ASSERT_NE(nullptr, fetched_pt);
  EXPECT_STREQ(nullptr, fptu::check(fetched_pt));

  fetched_ro = fptu_take_noshrink(fetched_pt);
  ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
  ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
  EXPECT_EQ(0,
            memcmp(origin_ro.units, fetched_ro.units, origin_ro.total_bytes));

  EXPECT_EQ(origin_pt->end_, fetched_pt->end_);
  EXPECT_EQ(origin_pt->pivot_, fetched_pt->pivot_);
  EXPECT_EQ(origin_pt->tail_, fetched_pt->tail_);
  EXPECT_EQ(origin_pt->head_, fetched_pt->head_);
  EXPECT_EQ(origin_pt->junk_.count, fetched_pt->junk_.count);
  EXPECT_EQ(origin_pt->junk_.volume, fetched_pt->junk_.volume);

  // check without more-items
  fetched_pt = fptu_fetch(origin_ro, fetched_space, sizeof(fetched_space), 0);
  ASSERT_NE(nullptr, fetched_pt);
  EXPECT_STREQ(nullptr, fptu::check(fetched_pt));

  fetched_ro = fptu_take_noshrink(fetched_pt);
  ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
  ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
  EXPECT_EQ(0,
            memcmp(origin_ro.units, fetched_ro.units, origin_ro.total_bytes));
}

TEST(Fetch, Variate) {
  char origin_space[fptu::buffer_enough];
  char fetched_space[fptu::buffer_enough];
  fptu_ro origin_ro, fetched_ro;
  fptu_rw *origin_pt, *fetched_pt;

  static const size_t space_cases[] = {
      /* clang-format off */
        4, 5, 6, 7, 8, 9, 42, fptu_rw::pure_tuple_size(),
        fptu_max_tuple_bytes / 3, fptu_max_tuple_bytes / 2,
        fptu_max_tuple_bytes
      /* clang-format on */
  };

  static const unsigned items_cases[] = {
      /* clang-format off */
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 42, ~0u, fptu::max_fields / 3,
        fptu::max_fields / 2, fptu::max_fields, fptu::max_fields + 1,
        fptu::max_fields * 2
      /* clang-format on */
  };

  for (auto fetch_buffer_space : space_cases) {
    const size_t bytes = fptu_rw::pure_tuple_size() + fetch_buffer_space;
    ASSERT_LE(bytes, sizeof(fetched_space));

    for (auto more_items : items_cases) {

      origin_pt =
          fptu_init(origin_space, sizeof(origin_space), fptu::max_fields);
      ASSERT_NE(nullptr, origin_pt);
      EXPECT_STREQ(nullptr, fptu::check(origin_pt));
      origin_ro = fptu_take_noshrink(origin_pt);
      ASSERT_STREQ(nullptr, fptu::check(origin_ro));
      EXPECT_EQ(fptu::unit_size, origin_ro.total_bytes);

      // check empty
      size_t origin_items =
          (size_t)(fptu_end_ro(origin_ro) - fptu_begin_ro(origin_ro));
      size_t origin_payload_bytes = origin_ro.total_bytes -
                                    fptu::details::units2bytes(origin_items) -
                                    fptu::unit_size;
      SCOPED_TRACE(
          "origin.items " + std::to_string(origin_items) +
          ", origin.payload_bytes " + std::to_string(origin_payload_bytes) +
          ", fetch.buffer_space " + std::to_string(fetch_buffer_space) +
          ", fetch.more_items " + std::to_string(more_items));
      fetched_pt = fptu_fetch(origin_ro, fetched_space, bytes, more_items);
      if (more_items > fptu::max_fields ||
          bytes < fptu_space(origin_items + more_items, origin_payload_bytes)) {
        EXPECT_EQ(nullptr, fetched_pt);
      } else {
        EXPECT_NE(nullptr, fetched_pt);
      }
      if (!fetched_pt)
        continue;

      EXPECT_GE(0, (int)fptu::check_and_get_buffer_size(origin_ro, more_items,
                                                        0, nullptr));
      const char *error = "clean me";
      EXPECT_GE(bytes, fptu::check_and_get_buffer_size(origin_ro, more_items, 0,
                                                       &error));
      EXPECT_STREQ(nullptr, error);
      EXPECT_STREQ(nullptr, fptu::check(fetched_pt));
      fetched_ro = fptu_take_noshrink(fetched_pt);
      ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
      ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
      EXPECT_EQ(
          0, memcmp(origin_ro.units, fetched_ro.units, origin_ro.total_bytes));

      if (more_items + origin_items >= fptu::max_fields) {
        EXPECT_EQ(origin_pt->pivot_, fetched_pt->pivot_);
        EXPECT_EQ(origin_pt->tail_, fetched_pt->tail_);
        EXPECT_EQ(origin_pt->head_, fetched_pt->head_);
        EXPECT_EQ(origin_pt->junk_.count, fetched_pt->junk_.count);
        EXPECT_EQ(origin_pt->junk_.volume, fetched_pt->junk_.volume);
      }
      if (bytes == fptu::buffer_enough) {
        EXPECT_EQ(origin_pt->end_, fetched_pt->end_);
      }

      // adds header-only fields and check
      for (unsigned n = 1; n < 11; ++n) {
        SCOPED_TRACE("header-only, n = " + std::to_string(n));

        EXPECT_EQ(FPTU_OK,
                  fptu_insert_uint16(origin_pt, fptu_max_cols, uint16_t(n)));
        ASSERT_STREQ(nullptr, fptu::check(origin_pt));
        origin_ro = fptu_take_noshrink(origin_pt);
        ASSERT_STREQ(nullptr, fptu::check(origin_ro));
        EXPECT_EQ(fptu::unit_size * (n + 1), origin_ro.total_bytes);

        origin_items =
            (size_t)(fptu_end_ro(origin_ro) - fptu_begin_ro(origin_ro));
        origin_payload_bytes = origin_ro.total_bytes -
                               fptu::details::units2bytes(origin_items) -
                               fptu::unit_size;
        SCOPED_TRACE("origin.items " + std::to_string(origin_items) +
                     ", origin.payload_bytes " +
                     std::to_string(origin_payload_bytes) + ", fetch.space " +
                     std::to_string(fetch_buffer_space) + ", more_items " +
                     std::to_string(more_items));
        fetched_pt = fptu_fetch(origin_ro, fetched_space, bytes, more_items);
        if (more_items > fptu::max_fields ||
            bytes <
                fptu_space(origin_items + more_items, origin_payload_bytes)) {
          EXPECT_EQ(nullptr, fetched_pt);
        } else {
          EXPECT_NE(nullptr, fetched_pt);
        }
        if (!fetched_pt)
          continue;

        EXPECT_GE(bytes, fptu::check_and_get_buffer_size(origin_ro, more_items,
                                                         0, &error));
        EXPECT_STREQ(nullptr, error);
        EXPECT_STREQ(nullptr, fptu::check(fetched_pt));
        fetched_ro = fptu_take_noshrink(fetched_pt);
        ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
        ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
        EXPECT_EQ(0, memcmp(origin_ro.units, fetched_ro.units,
                            origin_ro.total_bytes));

        if (more_items + origin_items >= fptu::max_fields) {
          EXPECT_EQ(origin_pt->pivot_, fetched_pt->pivot_);
          EXPECT_EQ(origin_pt->tail_, fetched_pt->tail_);
          EXPECT_EQ(origin_pt->head_, fetched_pt->head_);
          EXPECT_EQ(origin_pt->junk_.count, fetched_pt->junk_.count);
          EXPECT_EQ(origin_pt->junk_.volume, fetched_pt->junk_.volume);
        }
        if (bytes == fptu::buffer_enough) {
          EXPECT_EQ(origin_pt->end_, fetched_pt->end_);
        }
      }

      origin_pt =
          fptu_init(origin_space, sizeof(origin_space), fptu::max_fields);
      ASSERT_NE(nullptr, origin_pt);
      EXPECT_STREQ(nullptr, fptu::check(origin_pt));

      // adds fileds with payload and check
      for (unsigned n = 1; n < 11; ++n) {
        SCOPED_TRACE("with-payload, n = " + std::to_string(n));

        EXPECT_EQ(FPTU_OK, fptu_insert_uint32(origin_pt, fptu_max_cols, n));
        ASSERT_STREQ(nullptr, fptu::check(origin_pt));
        origin_ro = fptu_take_noshrink(origin_pt);
        ASSERT_STREQ(nullptr, fptu::check(origin_ro));
        EXPECT_EQ(fptu::unit_size * (n + n + 1), origin_ro.total_bytes);

        origin_items =
            (size_t)(fptu_end_ro(origin_ro) - fptu_begin_ro(origin_ro));
        origin_payload_bytes = origin_ro.total_bytes -
                               fptu::details::units2bytes(origin_items) -
                               fptu::unit_size;
        SCOPED_TRACE("origin.items " + std::to_string(origin_items) +
                     ", origin.payload_bytes " +
                     std::to_string(origin_payload_bytes) + ", fetch.space " +
                     std::to_string(fetch_buffer_space) + ", more_items " +
                     std::to_string(more_items));
        fetched_pt = fptu_fetch(origin_ro, fetched_space, bytes, more_items);
        if (more_items > fptu::max_fields ||
            bytes <
                fptu_space(origin_items + more_items, origin_payload_bytes)) {
          EXPECT_EQ(nullptr, fetched_pt);
        } else {
          EXPECT_NE(nullptr, fetched_pt);
        }
        if (!fetched_pt)
          continue;

        EXPECT_GE(bytes, fptu::check_and_get_buffer_size(origin_ro, more_items,
                                                         0, &error));
        EXPECT_STREQ(nullptr, error);
        EXPECT_STREQ(nullptr, fptu::check(fetched_pt));
        fetched_ro = fptu_take_noshrink(fetched_pt);
        ASSERT_STREQ(nullptr, fptu::check(fetched_ro));
        ASSERT_EQ(origin_ro.total_bytes, fetched_ro.total_bytes);
        EXPECT_EQ(0, memcmp(origin_ro.units, fetched_ro.units,
                            origin_ro.total_bytes));

        if (more_items + origin_items >= fptu::max_fields) {
          EXPECT_EQ(origin_pt->pivot_, fetched_pt->pivot_);
          EXPECT_EQ(origin_pt->tail_, fetched_pt->tail_);
          EXPECT_EQ(origin_pt->head_, fetched_pt->head_);
          EXPECT_EQ(origin_pt->junk_.count, fetched_pt->junk_.count);
          EXPECT_EQ(origin_pt->junk_.volume, fetched_pt->junk_.volume);
        }
        if (bytes == fptu::buffer_enough) {
          EXPECT_EQ(origin_pt->end_, fetched_pt->end_);
        }
      }
    }
  }
}

TEST(Fetch, DeNils) {
  EXPECT_EQ(-1, fptu_field_column(nullptr));

  EXPECT_EQ(0, fptu_field_uint16(nullptr));
  EXPECT_EQ(0, fptu_field_int32(nullptr));
  EXPECT_EQ(0, fptu_field_uint32(nullptr));
  EXPECT_EQ(0, fptu_field_int64(nullptr));
  EXPECT_EQ(0, fptu_field_uint64(nullptr));
  if (!__FINITE_MATH_ONLY__) {
    EXPECT_TRUE(std::isnan(/*FPTU_DENIL_FP32*/ fptu_field_fp32(nullptr)));
    EXPECT_TRUE(std::isnan(/*FPTU_DENIL_FP64*/ fptu_field_fp64(nullptr)));
  }
  const auto expected_fp32 = FPTU_DENIL_FP32;
  const auto got_fp32 = fptu_field_fp32(nullptr);
  EXPECT_EQ(0, std::memcmp(&expected_fp32, &got_fp32, sizeof(expected_fp32)));
  const auto expected_fp64 = FPTU_DENIL_FP64;
  const auto got_fp64 = fptu_field_fp64(nullptr);
  EXPECT_EQ(0, std::memcmp(&expected_fp64, &got_fp64, sizeof(expected_fp64)));
  EXPECT_EQ(FPTU_DENIL_DATETIME, fptu_field_datetime(nullptr));
  EXPECT_EQ(FPTU_DENIL_CSTR, fptu_field_cstr(nullptr));

  EXPECT_EQ(FPTU_DENIL_FIXBIN, fptu_field_96(nullptr));
  EXPECT_EQ(FPTU_DENIL_FIXBIN, fptu_field_128(nullptr));
  EXPECT_EQ(FPTU_DENIL_FIXBIN, fptu_field_160(nullptr));
  EXPECT_EQ(nullptr, fptu_field_opaque(nullptr).iov_base);
  EXPECT_EQ(0u, fptu_field_opaque(nullptr).iov_len);
}

//------------------------------------------------------------------------------

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
