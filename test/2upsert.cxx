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

#include "fptu_test.h"

#include <stdlib.h>

#ifdef _MSC_VER
#pragma warning(disable : 4738) /* storing 32-bit float result in memory... */
#pragma warning(disable : 4640) /* construction of local static object is not  \
                                   thread-safe */
#endif                          /* _MSC_VER (warnings) */

static const uint8_t pattern[256] = {
    /* clang-format off */
    177, 85,  188, 146, 222, 148, 10,  7,   241, 57,  199, 43,  106, 240, 124,
    237, 220, 230, 197, 76,  116, 153, 205, 221, 28,  2,   31,  233, 58,  60,
    159, 228, 109, 20,  66,  214, 111, 15,  18,  44,  208, 72,  249, 210, 113,
    212, 165, 1,   225, 174, 164, 204, 45,  130, 82,  80,  99,  138, 48,  167,
    78,  14,  149, 207, 103, 178, 223, 25,  163, 118, 139, 122, 37,  119, 182,
    26,  4,   236, 96,  64,  196, 75,  29,  95,  252, 33,  185, 87,  110, 202,
    200, 125, 93,  55,  84,  105, 89,  215, 161, 211, 154, 86,  39,  145, 77,
    190, 147, 136, 108, 132, 107, 172, 229, 83,  187, 226, 160, 155, 242, 133,
    23,  8,   6,   151, 184, 195, 17,  16,  140, 191, 131, 156, 61,  239, 127,
    181, 94,  176, 27,  81,  235, 141, 69,  47,  170, 74,  168, 88,  56,  193,
    68,  209, 104, 143, 52,  53,  46,  115, 158, 100, 243, 213, 247, 34,  62,
    238, 203, 232, 92,  49,  54,  42,  245, 171, 227, 123, 24,  186, 63,  112,
    135, 183, 254, 5,   198, 13,  216, 73,  219, 173, 255, 121, 79,  137, 150,
    12,  162, 41,  206, 217, 231, 120, 59,  128, 101, 51,  201, 253, 35,  194,
    166, 70,  71,  11,  189, 50,  234, 218, 30,  0,   134, 32,  152, 90,  19,
    224, 3,   250, 98,  169, 102, 38,  142, 91,  117, 180, 175, 246, 9,   129,
    114, 244, 67,  157, 21,  144, 126, 40,  179, 36,  192, 248, 22,  65,  251,
    97
    /* clang-format on */
};

struct iovec opaque_iov(unsigned col, unsigned n, unsigned salt) {
  struct iovec value;
  value.iov_base = (void *)(pattern + (n * salt) % 223);
  value.iov_len = 4 + ((n + col) & 3) * 4;
  return value;
}

TEST(Upsert, InvalidColumn) {
  char space_exactly_noitems[fptu_rw::pure_tuple_size()];
  fptu_rw *pt =
      fptu_init(space_exactly_noitems, sizeof(space_exactly_noitems), 0);
  ASSERT_NE(nullptr, pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  ASSERT_EQ(0u, fptu_space4items(pt));
  ASSERT_EQ(0u, fptu_space4data(pt));
  ASSERT_EQ(0u, fptu_junkspace(pt));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));

  // column is a stub, expect EINVAL
  const unsigned inval_col = fptu_max_cols + 1;
  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_null(pt, inval_col));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_uint16(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_uint16(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_uint16(pt, inval_col, 0));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_bool(pt, inval_col, true));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_bool(pt, inval_col, false));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_bool(pt, inval_col, true));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_uint32(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_uint32(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_uint32(pt, inval_col, 0));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_int32(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_int32(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_int32(pt, inval_col, 0));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_uint64(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_uint64(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_uint64(pt, inval_col, 0));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_int64(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_int64(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_int64(pt, inval_col, 0));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_fp64(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_fp64(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_fp64(pt, inval_col, 0));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_fp32(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_fp32(pt, inval_col, 0));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_fp32(pt, inval_col, 0));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_96(pt, inval_col, "96__________"));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_96(pt, inval_col, "96__________"));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_96(pt, inval_col, "96__________"));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_128(pt, inval_col, "128_____________"));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_128(pt, inval_col, "128_____________"));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_128(pt, inval_col, "128_____________"));

  EXPECT_EQ(FPTU_EINVAL,
            fptu_upsert_160(pt, inval_col, "160_________________"));
  EXPECT_EQ(FPTU_EINVAL,
            fptu_insert_160(pt, inval_col, "160_________________"));
  EXPECT_EQ(FPTU_EINVAL,
            fptu_update_160(pt, inval_col, "160_________________"));

  fptu_datetime_t dt = fptu_now_coarse();
  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_datetime(pt, inval_col, dt));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_datetime(pt, inval_col, dt));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_datetime(pt, inval_col, dt));

  EXPECT_EQ(FPTU_EINVAL,
            fptu_upsert_256(pt, inval_col, "256_____________________________"));
  EXPECT_EQ(FPTU_EINVAL,
            fptu_insert_256(pt, inval_col, "256_____________________________"));
  EXPECT_EQ(FPTU_EINVAL,
            fptu_update_256(pt, inval_col, "256_____________________________"));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_cstr(pt, inval_col, "cstr"));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_cstr(pt, inval_col, "cstr"));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_cstr(pt, inval_col, "cstr"));

  EXPECT_EQ(FPTU_EINVAL, fptu_upsert_opaque(pt, inval_col, "data", 4));
  EXPECT_EQ(FPTU_EINVAL, fptu_insert_opaque(pt, inval_col, "data", 4));
  EXPECT_EQ(FPTU_EINVAL, fptu_update_opaque(pt, inval_col, "data", 4));

  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  EXPECT_STREQ(nullptr, fptu_legacy::check(pt));
}

TEST(Upsert, ZeroSpace) {
  char space_exactly_noitems[fptu_rw::pure_tuple_size()];
  fptu_rw *pt =
      fptu_init(space_exactly_noitems, sizeof(space_exactly_noitems), 0);
  ASSERT_NE(nullptr, pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // upsert_xyz() expect no-space
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_null(pt, fptu_max_cols));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_uint16(pt, 1, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_bool(pt, 1, true));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_uint32(pt, 1, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_int32(pt, 42, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_uint64(pt, 111, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_int64(pt, fptu_max_cols / 3, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_fp64(pt, fptu_max_cols - 3, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_fp32(pt, fptu_max_cols - 4, 1));
  EXPECT_EQ(FPTU_ENOSPACE,
            fptu_upsert_96(pt, fptu_max_cols / 2, "96__________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_128(pt, 257, "128_____________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_160(pt, 7, "160_________________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_datetime(pt, 8, fptu_now_coarse()));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_256(pt, fptu_max_cols - 2,
                                           "256_____________________________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_cstr(pt, fptu_max_cols - 1, "cstr"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_opaque(pt, fptu_max_cols, "data", 4));
  EXPECT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // discernible_null == FALSE leads to remove empty-zeros
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_bool(pt, 1, false));
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_uint16(pt, 1, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(pt, 0, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_insert_uint32(pt, 1, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_insert_int32(pt, 42, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_insert_uint64(pt, 111, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_uint32(pt, 1, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_int32(pt, 42, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_uint64(pt, 111, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_int64(pt, fptu_max_cols / 3, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_fp64(pt, fptu_max_cols - 3, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_upsert_fp32(pt, fptu_max_cols - 4, 0));
  //  EXPECT_EQ(FPTU_OK, fptu_insert_cstr(pt, fptu_max_cols - 1, ""));
  EXPECT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert_xyz() expect no-space
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_uint16(pt, 0, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_bool(pt, 0, true));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_uint32(pt, 1, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_int32(pt, 42, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_uint64(pt, 111, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_int64(pt, fptu_max_cols / 3, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_fp64(pt, fptu_max_cols - 3, 1));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_fp32(pt, fptu_max_cols - 4, 1));
  EXPECT_EQ(FPTU_ENOSPACE,
            fptu_insert_96(pt, fptu_max_cols / 2, "96__________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_128(pt, 257, "128_____________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_160(pt, 7, "160_________________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_datetime(pt, 8, fptu_now_coarse()));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_256(pt, fptu_max_cols - 2,
                                           "256_____________________________"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_cstr(pt, fptu_max_cols - 1, "cstr"));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_insert_opaque(pt, fptu_max_cols, "data", 4));

  EXPECT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // update_xyz() expect no-entry
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint16(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_bool(pt, 0, false));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint32(pt, 1, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_int32(pt, 42, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint64(pt, 111, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_int64(pt, fptu_max_cols / 3, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_fp64(pt, fptu_max_cols - 3, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_fp32(pt, fptu_max_cols - 4, 0));
  EXPECT_EQ(FPTU_ENOFIELD,
            fptu_update_96(pt, fptu_max_cols / 2, "96__________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_128(pt, 257, "128_____________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_160(pt, 7, "160_________________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_datetime(pt, 8, fptu_now_coarse()));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_256(pt, fptu_max_cols - 2,
                                           "256_____________________________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_cstr(pt, fptu_max_cols - 1, "cstr"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_opaque(pt, fptu_max_cols, "data", 4));

  EXPECT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
}

TEST(Upsert, Base) {
  const unsigned data = 34 * 4;
  fptu_rw *pt = fptu_alloc(15, data);
  ASSERT_NE(nullptr, pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(15u, fptu_space4items(pt));
  EXPECT_EQ(data, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  unsigned used = 0;

  EXPECT_EQ(FPTU_OK, fptu_upsert_null(pt, fptu_max_cols));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(14u, fptu_space4items(pt));
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_uint16(pt, 0, 0x8001));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(13u, fptu_space4items(pt));
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_uint32(pt, 1, 1354824703));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(12u, fptu_space4items(pt));
  used += 4;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_int32(pt, 42, -8782211));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(11u, fptu_space4items(pt));
  used += 4;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_uint64(pt, 111, 15047220096467327));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(10u, fptu_space4items(pt));
  used += 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK,
            fptu_upsert_int64(pt, fptu_max_cols / 3, -60585001468255361));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(9u, fptu_space4items(pt));
  used += 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK,
            fptu_upsert_fp64(pt, fptu_max_cols - 3, 3.14159265358979323846));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(8u, fptu_space4items(pt));
  used += 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_fp32(pt, fptu_max_cols - 4,
                                      (float)2.7182818284590452354));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(7u, fptu_space4items(pt));
  used += 4;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  static const uint8_t *const _96 = pattern;
  static const uint8_t *const _128 = _96 + 12;
  static const uint8_t *const _160 = _128 + 16;
  static const uint8_t *const _256 = _160 + 24;
  ASSERT_LT(32, pattern + sizeof(pattern) - _256);

  EXPECT_EQ(FPTU_OK, fptu_upsert_96(pt, fptu_max_cols / 2, _96));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(6u, fptu_space4items(pt));
  used += 96 / 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_128(pt, 257, _128));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(5u, fptu_space4items(pt));
  used += 128 / 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_160(pt, 7, _160));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(4u, fptu_space4items(pt));
  used += 160 / 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  const fptu_datetime_t now = fptu_now_coarse();
  EXPECT_EQ(FPTU_OK, fptu_upsert_datetime(pt, 8, now));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(3u, fptu_space4items(pt));
  used += 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_256(pt, fptu_max_cols - 2, _256));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(2u, fptu_space4items(pt));
  used += 256 / 8;
  EXPECT_EQ(data - used, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_cstr(pt, fptu_max_cols - 1, "abc"));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(1u, fptu_space4items(pt));
  EXPECT_EQ(8u, fptu_space4data(pt));

  EXPECT_EQ(FPTU_OK, fptu_upsert_cstr(pt, 42, "cstr"));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // update present column, expect no error
  EXPECT_EQ(FPTU_OK, fptu_upsert_null(pt, fptu_max_cols));

  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_null(pt, 33));

  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));

  fptu_ro ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ((1 + 15 + 34) * 4u, ro.total_bytes);

  EXPECT_EQ(0x8001u, fptu_get_uint16(ro, 0, nullptr));
  EXPECT_EQ(UINT32_C(1354824703), fptu_get_uint32(ro, 1, nullptr));
  EXPECT_EQ(-8782211, fptu_get_int32(ro, 42, nullptr));
  EXPECT_EQ(UINT64_C(15047220096467327), fptu_get_uint64(ro, 111, nullptr));
  EXPECT_EQ(INT64_C(-60585001468255361),
            fptu_get_int64(ro, fptu_max_cols / 3, nullptr));
  EXPECT_EQ(3.1415926535897932, fptu_get_fp64(ro, fptu_max_cols - 3, nullptr));
  EXPECT_EQ((float)2.7182818284590452354,
            fptu_get_fp32(ro, fptu_max_cols - 4, nullptr));
  EXPECT_STREQ("abc", fptu_get_cstr(ro, fptu_max_cols - 1, nullptr));
  EXPECT_STREQ("cstr", fptu_get_cstr(ro, 42, nullptr));
  EXPECT_EQ(fptu_eq, fptu_cmp_96(ro, fptu_max_cols / 2, _96));
  EXPECT_EQ(fptu_eq, fptu_cmp_128(ro, 257, _128));
  EXPECT_EQ(fptu_eq, fptu_cmp_160(ro, 7, _160));
  EXPECT_EQ(now.fixedpoint_32dot32(),
            fptu_get_datetime(ro, 8, nullptr).fixedpoint);
  EXPECT_EQ(fptu_eq, fptu_cmp_256(ro, fptu_max_cols - 2, _256));

  EXPECT_EQ(FPTU_OK, fptu_upsert_uint16(pt, 0, 42));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(42u, fptu_get_uint16(ro, 0, nullptr));

  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  fptu_destroy(pt);
}

TEST(Upsert, Overwrite) {
  fptu_rw *pt = fptu_alloc(3 + 3 * 2, (1 + 2 + (2 + 3 + 4) * 2 + 5) * 4);
  ASSERT_NE(nullptr, pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(9u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  const unsigned m = 46091;
  unsigned n = 1;
  EXPECT_EQ(FPTU_OK, fptu_upsert_opaque_iov(pt, 0, opaque_iov(0, n, 23)));
  EXPECT_EQ(FPTU_OK, fptu_upsert_opaque_iov(pt, 1, opaque_iov(1, n, 37)));
  EXPECT_EQ(FPTU_OK, fptu_upsert_opaque_iov(pt, 2, opaque_iov(2, n, 41)));
  EXPECT_EQ(FPTU_OK, fptu_upsert_uint16(pt, 3, uint16_t(n * m)));
  EXPECT_EQ(FPTU_OK, fptu_upsert_uint32(pt, 4, n * m * m));
  EXPECT_EQ(FPTU_OK, fptu_upsert_uint64(pt, 5, n * m * m * m));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(3u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  fptu_ro ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 0, opaque_iov(0, n, 23)));
  EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 1, opaque_iov(1, n, 37)));
  EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 2, opaque_iov(2, n, 41)));
  EXPECT_EQ((uint16_t)(n * m), fptu_get_uint16(ro, 3, nullptr));
  EXPECT_EQ((uint32_t)(n * m * m), fptu_get_uint32(ro, 4, nullptr));
  EXPECT_EQ((uint64_t)(n * m * m * m), fptu_get_uint64(ro, 5, nullptr));

  while (n < 1001) {
    unsigned p = n++;

    // overwrite field#3 and check all
    SCOPED_TRACE("touch field #3, uint16_t, n " + std::to_string(n));
    ASSERT_EQ(FPTU_OK, fptu_upsert_uint16(pt, 3, (uint16_t)(n * m)));
    ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
    ro = fptu_take_noshrink(pt);
    ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 0, opaque_iov(0, p, 23)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 1, opaque_iov(1, p, 37)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 2, opaque_iov(2, p, 41)));
    EXPECT_EQ((uint16_t)(n * m), fptu_get_uint16(ro, 3, nullptr));
    EXPECT_EQ((uint32_t)(p * m * m), fptu_get_uint32(ro, 4, nullptr));
    EXPECT_EQ((uint64_t)(p * m * m * m), fptu_get_uint64(ro, 5, nullptr));

    // overwrite field#5 and check all
    SCOPED_TRACE("touch field #5, uint64_t, n " + std::to_string(n));
    ASSERT_EQ(FPTU_OK, fptu_upsert_uint64(pt, 5, n * m * m * m));
    ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
    ro = fptu_take_noshrink(pt);
    ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 0, opaque_iov(0, p, 23)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 1, opaque_iov(1, p, 37)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 2, opaque_iov(2, p, 41)));
    EXPECT_EQ((uint16_t)(n * m), fptu_get_uint16(ro, 3, nullptr));
    EXPECT_EQ((uint32_t)(p * m * m), fptu_get_uint32(ro, 4, nullptr));
    EXPECT_EQ((uint64_t)(n * m * m * m), fptu_get_uint64(ro, 5, nullptr));

    // overwrite field#0 and check all
    SCOPED_TRACE(
        "touch field #0, len " + std::to_string(opaque_iov(0, p, 23).iov_len) +
        "=>" + std::to_string(opaque_iov(0, n, 23).iov_len) + ", n " +
        std::to_string(n) + ", space " + std::to_string(fptu_space4items(pt)) +
        "/" + std::to_string(fptu_space4data(pt)) + ", junk " +
        std::to_string(fptu_junkspace(pt)));
    ASSERT_EQ(FPTU_OK, fptu_upsert_opaque_iov(pt, 0, opaque_iov(0, n, 23)));
    ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
    ro = fptu_take_noshrink(pt);
    ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 0, opaque_iov(0, n, 23)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 1, opaque_iov(1, p, 37)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 2, opaque_iov(2, p, 41)));
    EXPECT_EQ((uint16_t)(n * m), fptu_get_uint16(ro, 3, nullptr));
    EXPECT_EQ((uint32_t)(p * m * m), fptu_get_uint32(ro, 4, nullptr));
    EXPECT_EQ((uint64_t)(n * m * m * m), fptu_get_uint64(ro, 5, nullptr));

    // overwrite field#2 and check all
    SCOPED_TRACE("touch field #2, len " +
                 std::to_string(opaque_iov(2, p, 41).iov_len) + "=>" +
                 std::to_string(opaque_iov(2, n, 41).iov_len) + ", space " +
                 std::to_string(fptu_space4items(pt)) + "/" +
                 std::to_string(fptu_space4data(pt)) + ", junk " +
                 std::to_string(fptu_junkspace(pt)));
    ASSERT_EQ(FPTU_OK, fptu_upsert_opaque_iov(pt, 2, opaque_iov(2, n, 41)));
    ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
    ro = fptu_take_noshrink(pt);
    ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 0, opaque_iov(0, n, 23)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 1, opaque_iov(1, p, 37)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 2, opaque_iov(2, n, 41)));
    EXPECT_EQ((uint16_t)(n * m), fptu_get_uint16(ro, 3, nullptr));
    EXPECT_EQ((uint32_t)(p * m * m), fptu_get_uint32(ro, 4, nullptr));
    EXPECT_EQ((uint64_t)(n * m * m * m), fptu_get_uint64(ro, 5, nullptr));

    // overwrite field#4 and check all
    SCOPED_TRACE("touch field #4, uint32_t, n " + std::to_string(n));
    ASSERT_EQ(FPTU_OK, fptu_upsert_uint32(pt, 4, n * m * m));
    ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
    ro = fptu_take_noshrink(pt);
    ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 0, opaque_iov(0, n, 23)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 1, opaque_iov(1, p, 37)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 2, opaque_iov(2, n, 41)));
    EXPECT_EQ((uint16_t)(n * m), fptu_get_uint16(ro, 3, nullptr));
    EXPECT_EQ((uint32_t)(n * m * m), fptu_get_uint32(ro, 4, nullptr));
    EXPECT_EQ((uint64_t)(n * m * m * m), fptu_get_uint64(ro, 5, nullptr));

    // overwrite field#1 and check all
    SCOPED_TRACE("touch field #1, len " +
                 std::to_string(opaque_iov(1, p, 37).iov_len) + "=>" +
                 std::to_string(opaque_iov(1, n, 37).iov_len) + ", space " +
                 std::to_string(fptu_space4items(pt)) + "/" +
                 std::to_string(fptu_space4data(pt)) + ", junk " +
                 std::to_string(fptu_junkspace(pt)));
    ASSERT_EQ(FPTU_OK, fptu_upsert_opaque_iov(pt, 1, opaque_iov(1, n, 37)));
    ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
    ro = fptu_take_noshrink(pt);
    ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 0, opaque_iov(0, n, 23)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 1, opaque_iov(1, n, 37)));
    EXPECT_EQ(fptu_eq, fptu_cmp_opaque_iov(ro, 2, opaque_iov(2, n, 41)));
    EXPECT_EQ((uint16_t)(n * m), fptu_get_uint16(ro, 3, nullptr));
    EXPECT_EQ((uint32_t)(n * m * m), fptu_get_uint32(ro, 4, nullptr));
    EXPECT_EQ((uint64_t)(n * m * m * m), fptu_get_uint64(ro, 5, nullptr));
  }

  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  fptu_destroy(pt);
}

TEST(Upsert, InsertUpdate) {
  const unsigned items_limit = 29;
  const unsigned bytes_limit = 34 * 4 * 2 + 8;
  fptu_rw *pt = fptu_alloc(items_limit, bytes_limit);
  ASSERT_NE(nullptr, pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  unsigned bytes_used = 0;
  EXPECT_EQ(FPTU_OK, fptu_upsert_null(pt, 0));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 1, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // update_xyz() expect no-entry
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint16(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint32(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_int32(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint64(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_int64(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_fp64(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_fp32(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_96(pt, 0, "96__________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_128(pt, 0, "128_____________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_160(pt, 0, "160_________________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_datetime(pt, 0, fptu_now_coarse()));
  EXPECT_EQ(FPTU_ENOFIELD,
            fptu_update_256(pt, 0, "256_____________________________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_cstr(pt, 0, "cstr"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_opaque(pt, 0, "data", 4));

  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 1, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  //------------------------------------------------------------------
  fptu_ro ro;

  // insert the first copy of field(0, uint16)
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(pt, 0, 0x8001));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 2, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(0x8001u, fptu_get_uint16(ro, 0, nullptr));
  // insert the second copy of field(0, uint16)
  EXPECT_EQ(FPTU_OK, fptu_insert_uint16(pt, 0, 0x8001 + 43));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 3, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(0x8001u + 43, fptu_get_uint16(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, uint32)
  EXPECT_EQ(FPTU_OK, fptu_insert_uint32(pt, 0, 1354824703));
  bytes_used += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 4, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(UINT32_C(1354824703), fptu_get_uint32(ro, 0, nullptr));
  // insert the second copy of field(0, uint32)
  EXPECT_EQ(FPTU_OK, fptu_insert_uint32(pt, 0, 1354824703 + 43));
  bytes_used += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 5, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(UINT32_C(1354824703) + 43u, fptu_get_uint32(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, int32)
  EXPECT_EQ(FPTU_OK, fptu_insert_int32(pt, 0, -8782211));
  bytes_used += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 6, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(INT32_C(-8782211), fptu_get_int32(ro, 0, nullptr));
  // insert the second copy of field(0, int32)
  EXPECT_EQ(FPTU_OK, fptu_insert_int32(pt, 0, -8782211 + 43));
  bytes_used += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 7, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(INT32_C(-8782211) + 43, fptu_get_int32(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, uint64)
  EXPECT_EQ(FPTU_OK, fptu_insert_uint64(pt, 0, 15047220096467327));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 8, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(UINT64_C(15047220096467327), fptu_get_uint64(ro, 0, nullptr));
  // insert the second copy of field(0, uint64)
  EXPECT_EQ(FPTU_OK, fptu_insert_uint64(pt, 0, 15047220096467327 + 43));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 9, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(UINT64_C(15047220096467327) + 43, fptu_get_uint64(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, int64)
  EXPECT_EQ(FPTU_OK, fptu_insert_int64(pt, 0, -60585001468255361));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 10, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(INT64_C(-60585001468255361), fptu_get_int64(ro, 0, nullptr));
  // insert the second copy of field(0, int64)
  EXPECT_EQ(FPTU_OK, fptu_insert_int64(pt, 0, -60585001468255361 + 43));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 11, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(INT64_C(-60585001468255361) + 43, fptu_get_int64(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, fp64)
  EXPECT_EQ(FPTU_OK, fptu_insert_fp64(pt, 0, 3.14159265358979323846));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 12, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(3.1415926535897932, fptu_get_fp64(ro, 0, nullptr));
  // insert the second copy of field(0, fp64)
  EXPECT_EQ(FPTU_OK, fptu_insert_fp64(pt, 0, 3.14159265358979323846 + 43));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 13, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(3.1415926535897932 + 43, fptu_get_fp64(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, fp32)
  EXPECT_EQ(FPTU_OK, fptu_insert_fp32(pt, 0, (float)2.7182818284590452354));
  bytes_used += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 14, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ((float)2.7182818284590452354, fptu_get_fp32(ro, 0, nullptr));
  // insert the second copy of field(0, fp32)
  EXPECT_EQ(FPTU_OK,
            fptu_insert_fp32(pt, 0, (float)2.7182818284590452354 + 43));
  bytes_used += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 15, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ((float)2.7182818284590452354 + 43, fptu_get_fp32(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  static const uint8_t *const _96 = pattern + 42;
  static const uint8_t *const _128 = _96 + 12;
  static const uint8_t *const _160 = _128 + 16;
  static const uint8_t *const _256 = _160 + 24;
  ASSERT_LT(32, pattern + sizeof(pattern) - 43 - _256);
  const fptu_datetime_t now1 = fptu_now_coarse();

  // insert the first copy of field(0, _96)
  EXPECT_EQ(FPTU_OK, fptu_insert_96(pt, 0, _96));
  bytes_used += 96 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 16, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_96(ro, 0, _96));
  EXPECT_EQ(0, memcmp(_96, fptu_get_96(ro, 0, nullptr), 96 / 8));
  // insert the second copy of field(0, _96)
  EXPECT_EQ(FPTU_OK, fptu_insert_96(pt, 0, _96 + 43));
  bytes_used += 96 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 17, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_96(ro, 0, _96 + 43));
  EXPECT_EQ(0, memcmp(_96 + 43, fptu_get_96(ro, 0, nullptr), 96 / 8));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, _128)
  EXPECT_EQ(FPTU_OK, fptu_insert_128(pt, 0, _128));
  bytes_used += 128 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 18, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_128(ro, 0, _128));
  EXPECT_EQ(0, memcmp(_128, fptu_get_128(ro, 0, nullptr), 128 / 8));
  // insert the second copy of field(0, _128)
  EXPECT_EQ(FPTU_OK, fptu_insert_128(pt, 0, _128 + 43));
  bytes_used += 128 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 19, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_128(ro, 0, _128 + 43));
  EXPECT_EQ(0, memcmp(_128 + 43, fptu_get_128(ro, 0, nullptr), 128 / 8));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, _160)
  EXPECT_EQ(FPTU_OK, fptu_insert_160(pt, 0, _160));
  bytes_used += 160 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 20, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_160(ro, 0, _160));
  EXPECT_EQ(0, memcmp(_160, fptu_get_160(ro, 0, nullptr), 160 / 8));
  // insert the second copy of field(0, _160)
  EXPECT_EQ(FPTU_OK, fptu_insert_160(pt, 0, _160 + 43));
  bytes_used += 160 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 21, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_160(ro, 0, _160 + 43));
  EXPECT_EQ(0, memcmp(_160 + 43, fptu_get_160(ro, 0, nullptr), 160 / 8));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, datetime)
  const fptu_datetime_t now2 = fptu_now_fine();
  EXPECT_EQ(FPTU_OK, fptu_insert_datetime(pt, 0, now1));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 22, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(now1.fixedpoint_32dot32(),
            fptu_get_datetime(ro, 0, nullptr).fixedpoint);
  // insert the second copy of field(0, datetime)
  EXPECT_EQ(FPTU_OK, fptu_insert_datetime(pt, 0, now2));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 23, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(now2.fixedpoint_32dot32(),
            fptu_get_datetime(ro, 0, nullptr).fixedpoint);
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, _256)
  EXPECT_EQ(FPTU_OK, fptu_insert_256(pt, 0, _256));
  bytes_used += 256 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 24, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_256(ro, 0, _256));
  EXPECT_EQ(0, memcmp(_256, fptu_get_256(ro, 0, nullptr), 256 / 8));
  // insert the second copy of field(0, _256)
  EXPECT_EQ(FPTU_OK, fptu_insert_256(pt, 0, _256 + 43));
  bytes_used += 256 / 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 25, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(fptu_eq, fptu_cmp_256(ro, 0, _256 + 43));
  EXPECT_EQ(0, memcmp(_256 + 43, fptu_get_256(ro, 0, nullptr), 256 / 8));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, c-string)
  EXPECT_EQ(FPTU_OK, fptu_insert_cstr(pt, 0, "abc"));
  bytes_used += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 26, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_STREQ("abc", fptu_get_cstr(ro, 0, nullptr));
  // insert the second copy of field(0, c-string)
  EXPECT_EQ(FPTU_OK, fptu_insert_cstr(pt, 0, "cstr"));
  bytes_used += 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 27, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_STREQ("cstr", fptu_get_cstr(ro, 0, nullptr));
  EXPECT_EQ(0u, fptu_junkspace(pt));

  // insert the first copy of field(0, opaque)
  EXPECT_EQ(FPTU_OK, fptu_insert_opaque(pt, 0, "data", 4));
  bytes_used += 4 + 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 28, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  // insert the second copy of field(0, opaque)
  EXPECT_EQ(FPTU_OK, fptu_insert_opaque(pt, 0, "bananan", 8));
  bytes_used += 4 + 8;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(items_limit - 29, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));

  //------------------------------------------------------------------
  // update present column, expect no error
  EXPECT_EQ(bytes_limit, bytes_used);
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(FPTU_OK, fptu_upsert_null(pt, 0));

  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  EXPECT_EQ(FPTU_ENOSPACE, fptu_upsert_null(pt, 33));

  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));

  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ((items_limit + 1) * 4 + bytes_limit, ro.total_bytes);

  // one more check secondary values
  EXPECT_EQ(0x8001u + 43, fptu_get_uint16(ro, 0, nullptr));
  EXPECT_EQ(UINT32_C(1354824703) + 43, fptu_get_uint32(ro, 0, nullptr));
  EXPECT_EQ(INT32_C(-8782211) + 43, fptu_get_int32(ro, 0, nullptr));
  EXPECT_EQ(UINT64_C(15047220096467327) + 43, fptu_get_uint64(ro, 0, nullptr));
  EXPECT_EQ(INT64_C(-60585001468255361) + 43, fptu_get_int64(ro, 0, nullptr));
  EXPECT_EQ(3.1415926535897932 + 43, fptu_get_fp64(ro, 0, nullptr));
  EXPECT_EQ((float)2.7182818284590452354 + 43, fptu_get_fp32(ro, 0, nullptr));
  EXPECT_STREQ("cstr", fptu_get_cstr(ro, 0, nullptr));
  EXPECT_EQ(fptu_eq, fptu_cmp_96(ro, 0, _96 + 43));
  EXPECT_EQ(fptu_eq, fptu_cmp_128(ro, 0, _128 + 43));
  EXPECT_EQ(fptu_eq, fptu_cmp_160(ro, 0, _160 + 43));
  EXPECT_EQ(now2.fixedpoint_32dot32(),
            fptu_get_datetime(ro, 0, nullptr).fixedpoint);
  EXPECT_EQ(fptu_eq, fptu_cmp_256(ro, 0, _256 + 43));

  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));

  //------------------------------------------------------------------

  // update first copy of the field and check value
  unsigned junk = 0;
  EXPECT_EQ(FPTU_OK, fptu_update_uint16(pt, 0, 0x8001 - 42));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(junk, fptu_junkspace(pt));
  EXPECT_EQ(0x8001u - 42, fptu_get_uint16(ro, 0, nullptr));

  // remove the first copy of field and check value of the next copy
  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_uint16));
  junk += 4;
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(junk, fptu_junkspace(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(0x8001u, fptu_get_uint16(ro, 0, nullptr));

  // remove the second copy
  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_uint16));
  junk += 4;
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(junk, fptu_junkspace(pt));

  // try erase one more
  EXPECT_EQ(0, fptu_legacy::erase(pt, 0, fptu_uint16));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint16(pt, 0, 0));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(junk, fptu_junkspace(pt));
  EXPECT_EQ(nullptr, fptu_legacy::lookup(pt, 0, fptu_uint16));

  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(nullptr, fptu_legacy::lookup(ro, 0, fptu_uint16));

  //------------------------------------------------------------------

  // update secondary values and check ones
  EXPECT_EQ(FPTU_OK, fptu_update_uint32(pt, 0, 1354824703 - 42));
  EXPECT_EQ(FPTU_OK, fptu_update_int32(pt, 0, -8782211 - 42));
  EXPECT_EQ(FPTU_OK, fptu_update_uint64(pt, 0, 15047220096467327 - 42));
  EXPECT_EQ(FPTU_OK, fptu_update_int64(pt, 0, -64700360770547893));
  EXPECT_EQ(FPTU_OK, fptu_update_fp64(pt, 0, 3.14159265358979323846 - 42));
  EXPECT_EQ(FPTU_OK,
            fptu_update_fp32(pt, 0, (float)2.7182818284590452354 - 42));
  EXPECT_EQ(FPTU_OK, fptu_update_96(pt, 0, _96 - 42));
  EXPECT_EQ(FPTU_OK, fptu_update_128(pt, 0, _128 - 42));
  EXPECT_EQ(FPTU_OK, fptu_update_160(pt, 0, _160 - 42));
  const fptu_datetime_t now3 = fptu_now_fine();
  EXPECT_EQ(FPTU_OK, fptu_update_datetime(pt, 0, now3));
  EXPECT_EQ(FPTU_OK, fptu_update_256(pt, 0, _256 - 42));
  EXPECT_EQ(FPTU_OK, fptu_update_cstr(pt, 0, "xyz_"));
  EXPECT_EQ(FPTU_OK, fptu_update_opaque(pt, 0, "1234567", 8));

  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(0u, fptu_space4items(pt));
  EXPECT_EQ(0u, fptu_space4data(pt));
  EXPECT_EQ(junk, fptu_junkspace(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(nullptr, fptu_legacy::lookup(ro, 0, fptu_uint16));

  EXPECT_EQ(UINT32_C(1354824703) - 42, fptu_get_uint32(ro, 0, nullptr));
  EXPECT_EQ(INT32_C(-8782211) - 42, fptu_get_int32(ro, 0, nullptr));
  EXPECT_EQ(UINT64_C(15047220096467327) - 42, fptu_get_uint64(ro, 0, nullptr));
  EXPECT_EQ(INT64_C(-64700360770547893), fptu_get_int64(ro, 0, nullptr));
  EXPECT_EQ(3.1415926535897932 - 42, fptu_get_fp64(ro, 0, nullptr));
  EXPECT_EQ((float)2.7182818284590452354 - 42, fptu_get_fp32(ro, 0, nullptr));
  EXPECT_STREQ("xyz_", fptu_get_cstr(ro, 0, nullptr));
  iovec io = fptu_get_opaque(ro, 0, nullptr);
  EXPECT_EQ(8u, io.iov_len);
  ASSERT_NE(nullptr, io.iov_base);
  EXPECT_STREQ("1234567", (const char *)io.iov_base);
  EXPECT_EQ(fptu_eq, fptu_cmp_96(ro, 0, _96 - 42));
  EXPECT_EQ(fptu_eq, fptu_cmp_128(ro, 0, _128 - 42));
  EXPECT_EQ(fptu_eq, fptu_cmp_160(ro, 0, _160 - 42));
  EXPECT_EQ(now3.fixedpoint_32dot32(),
            fptu_get_datetime(ro, 0, nullptr).fixedpoint);
  EXPECT_EQ(fptu_eq, fptu_cmp_256(ro, 0, _256 - 42));

  //------------------------------------------------------------------

  // remove secondary values and check
  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_uint32));
  junk += 4 + 4;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_uint64));
  junk += 4 + 8;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_int64));
  junk += 4 + 8;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_int32));
  junk += 4 + 4;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_fp32));
  junk += 4 + 4;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_fp64));
  junk += 4 + 8;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_cstr));
  junk += 4 + 8;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_opaque));
  bytes_used -= 12;
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  EXPECT_EQ(1u, fptu_space4items(pt));
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_96));
  junk += 4 + 12;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_128));
  junk += 4 + 16;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_160));
  junk += 4 + 20;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_datetime));
  junk += 4 + 8;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  EXPECT_EQ(1, fptu_legacy::erase(pt, 0, fptu_256));
  junk += 4 + 32;
  EXPECT_EQ(junk, fptu_junkspace(pt));

  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  EXPECT_EQ(1u, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit - bytes_used, fptu_space4data(pt));
  EXPECT_EQ(junk, fptu_junkspace(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(nullptr, fptu_legacy::lookup(ro, 0, fptu_uint16));

  // secondary removed, expect first values
  EXPECT_EQ(UINT32_C(1354824703), fptu_get_uint32(ro, 0, nullptr));
  EXPECT_EQ(INT32_C(-8782211), fptu_get_int32(ro, 0, nullptr));
  EXPECT_EQ(UINT64_C(15047220096467327), fptu_get_uint64(ro, 0, nullptr));
  EXPECT_EQ(INT64_C(-60585001468255361), fptu_get_int64(ro, 0, nullptr));
  EXPECT_EQ(3.1415926535897932, fptu_get_fp64(ro, 0, nullptr));
  EXPECT_EQ((float)2.7182818284590452354, fptu_get_fp32(ro, 0, nullptr));
  EXPECT_STREQ("abc", fptu_get_cstr(ro, 0, nullptr));
  io = fptu_get_opaque(ro, 0, nullptr);
  EXPECT_EQ(4u, io.iov_len);
  ASSERT_NE(nullptr, io.iov_base);
  EXPECT_EQ(0, strncmp("data", (const char *)io.iov_base, 4));
  EXPECT_EQ(fptu_eq, fptu_cmp_96(ro, 0, _96));
  EXPECT_EQ(fptu_eq, fptu_cmp_128(ro, 0, _128));
  EXPECT_EQ(fptu_eq, fptu_cmp_160(ro, 0, _160));
  EXPECT_EQ(now1.fixedpoint_32dot32(),
            fptu_get_datetime(ro, 0, nullptr).fixedpoint);
  EXPECT_EQ(fptu_eq, fptu_cmp_256(ro, 0, _256));

  //------------------------------------------------------------------
  // remove all first values

  EXPECT_EQ(14, fptu_legacy::erase(pt, 0, fptu_any));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  ro = fptu_take_noshrink(pt);
  ASSERT_STREQ(nullptr, fptu_legacy::check(ro));
  EXPECT_EQ(nullptr, fptu_legacy::lookup(ro, 0, fptu_uint16));

  // update_xyz() expect no-entry
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint16(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint32(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_int32(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_uint64(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_int64(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_fp64(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_fp32(pt, 0, 0));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_96(pt, 0, "96__________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_128(pt, 0, "128_____________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_160(pt, 0, "160_________________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_datetime(pt, 0, now3));
  EXPECT_EQ(FPTU_ENOFIELD,
            fptu_update_256(pt, 0, "256_____________________________"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_cstr(pt, 0, "cstr"));
  EXPECT_EQ(FPTU_ENOFIELD, fptu_update_opaque(pt, 0, "data", 4));
  EXPECT_EQ(0, fptu_legacy::erase(pt, 0, fptu_any));

  //------------------------------------------------------------------

  EXPECT_EQ(items_limit, fptu_space4items(pt));
  EXPECT_EQ(bytes_limit, fptu_space4data(pt));
  EXPECT_EQ(0u, fptu_junkspace(pt));
  ASSERT_STREQ(nullptr, fptu_legacy::check(pt));
  fptu_destroy(pt);
}

//------------------------------------------------------------------------------

int main(int argc, char **argv) {
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
