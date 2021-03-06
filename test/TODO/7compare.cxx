/*
 *  Fast Positive Tuples (libfptu)
 *  Copyright 2016-2019 Leonid Yuriev, https://github.com/erthink/libfptu
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
 *
 * ***************************************************************************
 *
 * Imported (with simplification) from 1Hippeus project.
 * Copyright (c) 2006-2013 Leonid Yuriev <leo@yuriev.ru>.
 */

#include "fptu_test.h"
#include "shuffle6.hpp"

#ifdef _MSC_VER
#pragma warning(push, 1)
#if _MSC_VER < 1900
/* LY: workaround for dead code:
       microsoft visual studio 12.0\vc\include\xtree(1826) */
#pragma warning(disable : 4702) /* unreachable code */
#endif
#endif /* _MSC_VER */

#include <algorithm>
#include <set>
#include <string>

#ifdef _MSC_VER
#pragma warning(pop)
#endif

TEST(Compare, FetchTags) {
  char space[fptu::buffer_enough];
  ASSERT_TRUE(shuffle6::selftest());

  for (unsigned create_iter = 0; create_iter < (1 << 6); ++create_iter) {
    const unsigned create_mask = gray_code(create_iter);
    for (unsigned n = 0; n < shuffle6::factorial; ++n) {
      SCOPED_TRACE("shuffle #" + std::to_string(n) + ", create-mask " +
                   std::to_string(create_mask));

      shuffle6 shuffle(n);
      fptu_rw *pt = fptu_init(space, sizeof(space), fptu::max_fields);
      ASSERT_NE(nullptr, pt);

      std::set<unsigned> checker;
      std::string pattern;
      pattern.reserve(32);

      while (!shuffle.empty()) {
        unsigned i = shuffle.next();
        if (create_mask & (1 << i)) {
          switch (i) {
          default:
            ASSERT_TRUE(false);
            break;
          case 0:
          case 1:
            EXPECT_EQ(FPTU_OK, fptu_insert_uint16(pt, 41, 0));
            checker.insert(fptu::make_tag(41, fptu_uint16));
            pattern += " x";
            break;
          case 2:
          case 3:
            EXPECT_EQ(FPTU_OK, fptu_insert_int32(pt, 42, 0));
            checker.insert(fptu::make_tag(42, fptu_int32));
            pattern += " y";
            break;
          case 4:
          case 5:
            EXPECT_EQ(FPTU_OK, fptu_insert_uint64(pt, 43, 0));
            checker.insert(fptu::make_tag(43, fptu_uint64));
            pattern += " z";
            break;
          }
        }
      }

      ASSERT_STREQ(nullptr, fptu_check_rw(pt));
      fptu_ro ro = fptu_take_noshrink(pt);
      ASSERT_STREQ(nullptr, fptu_check_ro(ro));

      SCOPED_TRACE("pattern" + pattern);
      uint16_t tags[6 + 1];
      const auto end = fptu_tags(tags, fptu_begin_ro(ro), fptu_end_ro(ro));
      size_t count = (size_t)(end - tags);

      ASSERT_GE(6u, count);
      ASSERT_EQ(checker.size(), count);
      ASSERT_TRUE(std::equal(checker.begin(), checker.end(), tags));
    }
  }
}

static void probe(const fptu_rw *major_rw, const fptu_rw *minor_rw) {
  ASSERT_STREQ(nullptr, fptu_check_rw(major_rw));
  ASSERT_STREQ(nullptr, fptu_check_rw(minor_rw));

  const auto major = fptu_take_noshrink(major_rw);
  const auto minor = fptu_take_noshrink(minor_rw);
  ASSERT_STREQ(nullptr, fptu_check_ro(major));
  ASSERT_STREQ(nullptr, fptu_check_ro(minor));

  fptu_lge eq1, eq2, gt, lt;
  EXPECT_EQ(fptu_eq, eq1 = fptu_cmp_tuples(major, major));
  EXPECT_EQ(fptu_eq, eq2 = fptu_cmp_tuples(minor, minor));
  EXPECT_EQ(fptu_gt, gt = fptu_cmp_tuples(major, minor));
  EXPECT_EQ(fptu_lt, lt = fptu_cmp_tuples(minor, major));

  bool all_ok = (fptu_eq == eq1) && (fptu_eq == eq2) && (fptu_gt == gt) &&
                (fptu_lt == lt);

  ASSERT_TRUE(all_ok);
}

TEST(Compare, EmptyNull) {
  fptu_ro null;
  null.sys.iov_base = NULL;
  null.sys.iov_len = 0;
  ASSERT_STREQ(nullptr, fptu_check_ro(null));

  char space_exactly_noitems[fptu_rw::pure_tuple_size()];
  fptu_rw *empty_rw =
      fptu_init(space_exactly_noitems, sizeof(space_exactly_noitems), 0);
  ASSERT_NE(nullptr, empty_rw);
  ASSERT_STREQ(nullptr, fptu_check_rw(empty_rw));
  const auto empty_ro = fptu_take_noshrink(empty_rw);
  ASSERT_STREQ(nullptr, fptu_check_ro(empty_ro));

  EXPECT_EQ(fptu_eq, fptu_cmp_tuples(null, null));
  EXPECT_EQ(fptu_eq, fptu_cmp_tuples(null, empty_ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_tuples(empty_ro, null));
  EXPECT_EQ(fptu_eq, fptu_cmp_tuples(empty_ro, empty_ro));
}

TEST(Compare, Base) {
  char space4major[fptu::buffer_enough];
  fptu_rw *major =
      fptu_init(space4major, sizeof(space4major), fptu::max_fields);
  ASSERT_NE(nullptr, major);
  ASSERT_STREQ(nullptr, fptu_check_rw(major));

  char space4minor[fptu::buffer_enough];
  fptu_rw *minor =
      fptu_init(space4minor, sizeof(space4minor), fptu::max_fields);
  ASSERT_NE(nullptr, minor);
  ASSERT_STREQ(nullptr, fptu_check_rw(minor));

  // ???????????? ??????-???? ???????????????????? ??????????
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(major, 0, 0));
  probe(major, minor);
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(major, 0, 0));
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(minor, 0, 0));
  probe(major, minor);
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(major, 0, 0));
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(minor, 0, 0));
  probe(major, minor);
  ASSERT_EQ(FPTU_OK, fptu_clear(major));
  ASSERT_EQ(FPTU_OK, fptu_clear(minor));

  // ???????????? ???????????????? ???????????????????? ??????????
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(major, 0, 2));
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(minor, 0, 1));
  probe(major, minor);
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(minor, 0, INT16_MAX));
  probe(major, minor);
  ASSERT_EQ(FPTU_OK, fptu_clear(major));
  ASSERT_EQ(FPTU_OK, fptu_clear(minor));

  // ???????????? ?????????? ?? ???????????????? ??????????
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(minor, 1, 2));
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(major, 1, 3));
  EXPECT_EQ(FPTU_OK, fptu_insert_int32(major, 0, 1));
  probe(major, minor);
  ASSERT_EQ(FPTU_OK, fptu_clear(major));
  ASSERT_EQ(FPTU_OK, fptu_clear(minor));
}

#ifdef __OPTIMIZE__
TEST(Compare, Shuffle)
#else
/* LY: ?????? ?????????????????????? ?????????????????????? ???? 3 ?????????? */
TEST(Compare, DISABLED_Shuffle)
#endif
{
  /* ???????????????? ?????????????????? ?????? ?????????????????? ???????????????????? ?????????????????? ???????????????????? ????????????????.
   *
   * ????????????????:
   *  1. ?????? major ?? minor ???????????????????? ?????? ???????????????? ???????????????????? ????????????.
   *      - ???????? 6 ???????????????? ??????????????????: 2 ???????? ???????? 2 ???????? ?? ????????????????????
   *        ????????????/???????????? (?????????? 4 ???????????????? ??????????);
   *      - ?????? ???????? 6 ?????????????????? ???????????????????????? ?????? ???????????????????? ??????????????????????
   *        ?????????????? ???????????????? (64 ????????????????) ?? ?????? ???????????????? ??????????????
   *        ???????????????????? (720 ????????????????????).
   *  2. ?????? ???????????????? ???????????????????? ???????????? ???????????????????????????? ?????? major > minor,
   *      ?? ???????????? ?? major ???????????? ???????? ???????? ???????????????????????????? ????????, ???????? ????????
   *      ???????????????? ?????????????? ???????????? ?????????????????????? ?? minor.
   *  3. ?????? ???????????? ???????????????????? ?????????????????????? ???????????????????????? ??????????????????.
   *     ?? ???????????????????? ?????????????????????? ?????? fast/slow-path ???????? ?????????????????? ????????????????
   */
  ASSERT_TRUE(shuffle6::selftest());

  char space4minor[fptu::buffer_enough];
  fptu_rw *minor =
      fptu_init(space4minor, sizeof(space4minor), fptu::max_fields);
  ASSERT_NE(nullptr, minor);
  ASSERT_STREQ(nullptr, fptu_check_rw(minor));

  char space4major[fptu::buffer_enough];
  fptu_rw *major =
      fptu_init(space4major, sizeof(space4major), fptu::max_fields);
  ASSERT_NE(nullptr, major);
  ASSERT_STREQ(nullptr, fptu_check_rw(major));

  std::string minor_pattern, major_pattern;
  time_t start_timestamp = time(nullptr);
  // 64 * 64/2 * 720 * 720 = ?????????????? 1,061,683,200 ????????????????????
  for (unsigned minor_mask = 0; minor_mask < 64; ++minor_mask) {
    for (unsigned minor_order = 0; minor_order < shuffle6::factorial;
         ++minor_order) {
      ASSERT_EQ(FPTU_OK, fptu_clear(minor));

      minor_pattern.clear();
      shuffle6 minor_shuffle(minor_order);
      auto pending_mask = minor_mask;
      while (pending_mask && !minor_shuffle.empty()) {
        auto i = minor_shuffle.next();
        if (pending_mask & (1 << i)) {
          pending_mask -= 1 << i;
          switch (i) {
          default:
            ASSERT_TRUE(false);
            break;
          case 4:
            ASSERT_EQ(FPTU_OK, fptu_insert_uint32(minor, 1, 0));
            minor_pattern += " A0";
            break;
          case 5:
            ASSERT_EQ(FPTU_OK, fptu_insert_uint32(minor, 1, 1));
            minor_pattern += " A1";
            break;
          case 2:
            ASSERT_EQ(FPTU_OK, fptu_insert_int64(minor, 2, 2));
            minor_pattern += " B2";
            break;
          case 3:
            ASSERT_EQ(FPTU_OK, fptu_insert_int64(minor, 2, 3));
            minor_pattern += " B3";
            break;
          case 1:
            ASSERT_EQ(FPTU_OK, fptu_insert_cstr(minor, 3, "4"));
            minor_pattern += " C4";
            break;
          case 0:
            ASSERT_EQ(FPTU_OK, fptu_insert_fp32(minor, 4, 5));
            minor_pattern += " D5";
            break;
          }
        } else {
          /* ???????????????????? ????????????????????????, ?? ?????????????? ??????????????????????????
           * ?? ?????????????? present-mask ???????????????? ???????? ???????????? ???????????????????????????? */
          break;
        }
      }

      if (pending_mask == 0) {
        SCOPED_TRACE(fptu::format("minor: present-mask %u, shuffle # %u, [%s ]",
                                  minor_mask, minor_order,
                                  minor_pattern.c_str()));
        for (unsigned major_mask = minor_mask + 1; major_mask < 64;
             ++major_mask) {

          if (((minor_mask >> 2) & 3) == 3 && ((major_mask >> 2) & 3) < 3)
            /* ???????????????????? ???????????????? major < minor ???? ???????????? ???????? ?????????????????? */
            continue;

          if (((minor_mask >> 4) & 3) == 3 && ((major_mask >> 4) & 3) < 3)
            /* ???????????????????? ???????????????? major < minor ???? ???????????? ???????? ?????????????????? */
            continue;

          for (unsigned major_order = 0; major_order < shuffle6::factorial;
               ++major_order) {
            ASSERT_EQ(FPTU_OK, fptu_clear(major));

            major_pattern.clear();
            shuffle6 major_shuffle(major_order);
            pending_mask = major_mask;
            while (pending_mask && !major_shuffle.empty()) {
              auto i = major_shuffle.next();
              if (pending_mask & (1 << i)) {
                pending_mask -= 1 << i;
                switch (i) {
                default:
                  ASSERT_TRUE(false);
                  break;
                case 4:
                  ASSERT_EQ(FPTU_OK, fptu_insert_uint32(major, 1, 1));
                  major_pattern += " A1";
                  break;
                case 5:
                  ASSERT_EQ(FPTU_OK, fptu_insert_uint32(major, 1, 2));
                  major_pattern += " A2";
                  break;
                case 2:
                  ASSERT_EQ(FPTU_OK, fptu_insert_int64(major, 2, 3));
                  major_pattern += " B3";
                  break;
                case 3:
                  ASSERT_EQ(FPTU_OK, fptu_insert_int64(major, 2, 4));
                  major_pattern += " B4";
                  break;
                case 1:
                  ASSERT_EQ(FPTU_OK, fptu_insert_cstr(major, 3, "5"));
                  major_pattern += " C5";
                  break;
                case 0:
                  ASSERT_EQ(FPTU_OK, fptu_insert_fp32(major, 4, 6));
                  major_pattern += " D6";
                  break;
                }
              } else {
                /* ???????????????????? ????????????????????????, ?? ?????????????? ??????????????????????????
                 * ?? ?????????????? present-mask ???????????????? ???????? ???????????? ???????????????????????????? */
                break;
              }
            }

            if (pending_mask == 0) {
              SCOPED_TRACE(
                  fptu::format("major: present-mask %u, shuffle # %u, [%s ]",
                               major_mask, major_order, major_pattern.c_str()));
              ASSERT_NO_FATAL_FAILURE(probe(major, minor));

              time_t now_timestamp = time(nullptr);
              if (now_timestamp - start_timestamp > 42 &&
                  fptu_is_under_valgrind())
                /* ?????? Valgrind ???????? ?????????? ???????????????? ?????????? ??????????, ???? ????????????
                 * ???????????????????????? ?????? ?????????? ?????????????? ??????????-???????? ???????????????? ??????????
                 * ???????????????????? ????????????????.
                 * ?????????????? ?????????? 42 ?????????????? ???????????????????? ???????????? ????????????. */
                return;
            }
          }
        }
      }
    }
  }
}

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
