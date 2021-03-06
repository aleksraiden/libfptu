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

#include "fast_positive/tuples/api.h"
#include "fast_positive/tuples/details/exceptions.h"
#include "fast_positive/tuples/details/field.h"
#include "fast_positive/tuples/details/getter.h"
#include "fast_positive/tuples/details/meta.h"
#include "fast_positive/tuples/details/ro.h"
#include "fast_positive/tuples/details/scan.h"
#include "fast_positive/tuples/essentials.h"
#include "fast_positive/tuples/schema.h"
#include "fast_positive/tuples/token.h"

#include "fast_positive/tuples/internal.h"

#include "fast_positive/erthink/erthink_optimize4size.h"

namespace std {

__cold string to_string(const fptu_error error) {
  switch (error) {
  case FPTU_SUCCESS:
    return "FPTU: Success";
  case FPTU_ENOFIELD:
    return "FPTU: No such field (ENOENT)";
  case FPTU_EINVAL:
    return "FPTU: Invalid argument (EINVAL)";
  case FPTU_ENOSPACE:
    return "FPTU: No space left in tuple (ENOSPC)";
  default:
    return fptu::format("invalid(fptu_error)%i", (int)error);
  }
}

#if 0
template <typename native>
static inline std::string
array2str_native(fptu_tag_t tag, const fptu_payload *payload,
                 const char *name, const char *comma_fmt) {
  std::string result =
      fptu::format("{%u.%s[%u(%" PRIuPTR ")]=", fptu_get_colnum(tag), name,
                   payload->other.varlen.array_length,
                   units2bytes(payload->other.varlen.brutto));

  const native *array = (const native *)&payload->other.data[1];
  for (unsigned i = 0; i < payload->other.varlen.array_length; ++i)
    result += fptu::format(&comma_fmt[i == 0], array[i]);

  return result + "}";
}

static std::string array2str_fixbin(fptu_tag_t tag,
                                    const fptu_payload *payload,
                                    const char *name, unsigned itemsize) {
  std::string result =
      fptu::format("{%u.%s[%u(%" PRIuPTR ")]=", fptu_get_colnum(tag), name,
                   payload->other.varlen.array_length,
                   units2bytes(payload->other.varlen.brutto));

  const uint8_t *array = (const uint8_t *)&payload->other.data[1];
  for (unsigned i = 0; i < payload->other.varlen.array_length; ++i) {
    if (i)
      result += ",";
    result += fptu::hexadecimal(array, itemsize);
    array += itemsize;
  }

  return result + "}";
}

__cold string to_string(const fptu_field &field) {
  const auto type = field.type();
  auto payload = field.payload();
  switch ((int /* hush 'not in enumerated' */)type) {
  default:
  case fptu_null:
    return fptu::format("{%u.%s}", field.colnum(), fptu_type_name(type));
  case fptu_uint16:
    return fptu::format("{%u.%s=%u}", field.colnum(), fptu_type_name(type),
                        (unsigned)field.get_payload_uint16());
  case fptu_int32:
    return fptu::format("{%u.%s=%" PRId32 "}", field.colnum(),
                        fptu_type_name(type), payload->i32);
  case fptu_uint32:
    return fptu::format("{%u.%s=%" PRIu32 "}", field.colnum(),
                        fptu_type_name(type), payload->u32);
  case fptu_fp32:
    return fptu::format("{%u.%s=%g}", field.colnum(), fptu_type_name(type),
                        payload->fp32);
  case fptu_int64:
    return fptu::format("{%u.%s=%" PRId64 "}", field.colnum(),
                        fptu_type_name(type), payload->i64);
  case fptu_uint64:
    return fptu::format("{%u.%s=%" PRIu64 "}", field.colnum(),
                        fptu_type_name(type), payload->u64);
  case fptu_fp64:
    return fptu::format("{%u.%s=%.12g}", field.colnum(), fptu_type_name(type),
                        payload->fp64);

  case fptu_datetime:
    return fptu::format("{%u.%s=", field.colnum(), fptu_type_name(type)) +
           to_string(payload->dt) + '}';

//  case fptu_96:
//    return fptu::format("{%u.%s=", field.colnum(), fptu_type_name(type)) +
//           fptu::hexadecimal(payload->fixbin, 96 / 8) + '}';

  case fptu_128:
    return fptu::format("{%u.%s=", field.colnum(), fptu_type_name(type)) +
           fptu::hexadecimal(payload->fixbin, 128 / 8) + '}';

  case fptu_160:
    return fptu::format("{%u.%s=", field.colnum(), fptu_type_name(type)) +
           fptu::hexadecimal(payload->fixbin, 160 / 8) + '}';

  case fptu_256:
    return fptu::format("{%u.%s=", field.colnum(), fptu_type_name(type)) +
           fptu::hexadecimal(payload->fixbin, 256 / 8) + '}';

  case fptu_cstr:
    return fptu::format("{%u.%s=%s}", field.colnum(), fptu_type_name(type),
                        payload->cstr);

  case fptu_opaque:
    return fptu::format("{%u.%s=", field.colnum(), fptu_type_name(type)) +
           fptu::hexadecimal(payload->other.data,
                             payload->other.varlen.opaque_bytes) +
           '}';
    break;

  case fptu_nested:
    return fptu::format("{%u.%s=", field.colnum(), fptu_type_name(type)) +
           std::to_string(fptu_field_nested(&field)) + "}";

  case fptu_null | fptu_farray:
    return fptu::format("{%u.invalid-null[%u(%" PRIuPTR ")]}", field.colnum(),
                        payload->other.varlen.array_length,
                        units2bytes(payload->other.varlen.brutto));

  case fptu_uint16 | fptu_farray:
    return array2str_native<uint16_t>(field.tag, payload, "uint16", ",%u");
  case fptu_int32 | fptu_farray:
    return array2str_native<int32_t>(field.tag, payload, "int32", ",%" PRId32);
  case fptu_uint32 | fptu_farray:
    return array2str_native<uint32_t>(field.tag, payload, "uint32",
                                      ",%" PRIu32);
  case fptu_fp32 | fptu_farray:
    return array2str_native<float>(field.tag, payload, "fp32", ",%g");
  case fptu_int64 | fptu_farray:
    return array2str_native<int64_t>(field.tag, payload, "int64", ",%" PRId64);
  case fptu_uint64 | fptu_farray:
    return array2str_native<uint64_t>(field.tag, payload, "uint64",
                                      ",%" PRIu64);
  case fptu_fp64 | fptu_farray:
    return array2str_native<double>(field.tag, payload, "fp64", ",%.12g");

  case fptu_datetime | fptu_farray: {
    std::string result =
        fptu::format("{%u.%s[%u(%" PRIuPTR ")]=", field.colnum(), "datetime",
                     payload->other.varlen.array_length,
                     units2bytes(payload->other.varlen.brutto));

    const fptu_datetime_t *array =
        (const fptu_datetime_t *)&payload->other.data[1];
    for (unsigned i = 0; i < payload->other.varlen.array_length; ++i) {
      if (i)
        result += ",";
      result += to_string(array[i]);
    }
    return result + "}";
  }

  case fptu_96 | fptu_farray:
    return array2str_fixbin(field.tag, payload, "b96", 96 / 8);
  case fptu_128 | fptu_farray:
    return array2str_fixbin(field.tag, payload, "b128", 128 / 8);
  case fptu_160 | fptu_farray:
    return array2str_fixbin(field.tag, payload, "b160", 160 / 8);
  case fptu_256 | fptu_farray:
    return array2str_fixbin(field.tag, payload, "b256", 256 / 8);

  case fptu_cstr | fptu_farray: {
    std::string result =
        fptu::format("{%u.%s[%u(%" PRIuPTR ")]=", field.colnum(), "cstr",
                     payload->other.varlen.array_length,
                     units2bytes(payload->other.varlen.brutto));

    const char *array = (const char *)&payload->other.data[1];
    for (unsigned i = 0; i < payload->other.varlen.array_length; ++i) {
      result += fptu::format(&",%s"[i == 0], array);
      array += strlen(array) + 1;
    }
    return result + "}";
  }

  case fptu_opaque | fptu_farray: {
    std::string result =
        fptu::format("{%u.%s[%u(%" PRIuPTR ")]=", field.colnum(), "opaque",
                     payload->other.varlen.array_length,
                     units2bytes(payload->other.varlen.brutto));

    const fptu_unit *array = (const fptu_unit *)&payload->other.data[1];
    for (unsigned i = 0; i < payload->other.varlen.array_length; ++i) {
      if (i)
        result += ",";
      result += fptu::hexadecimal(array + 1, array->varlen.opaque_bytes);
      array += array->varlen.brutto + 1;
    }
    return result + "}";
  }

  case fptu_nested | fptu_farray: {
    std::string result =
        fptu::format("{%u.%s[%u(%" PRIuPTR ")]=", field.colnum(), "nested",
                     payload->other.varlen.array_length,
                     units2bytes(payload->other.varlen.brutto));

    const fptu_unit *array = (const fptu_unit *)&payload->other.data[1];
    for (unsigned i = 0; i < payload->other.varlen.array_length; ++i) {
      fptu_ro nested;
      nested.total_bytes = units2bytes(array->varlen.brutto + (size_t)1);
      nested.units = array;

      if (i)
        result += ",";
      result += to_string(nested);
      array += array->varlen.brutto + 1;
    }
    return result + "}";
  }
  }
}

__cold string to_string(const fptu_type type) {
  return string(fptu_type_name(type));
}

__cold string to_string(const fptu_ro &ro) {
  const fptu_field *const begin = fptu::begin(ro);
  const fptu_field *const end = fptu::end(ro);
  string result =
      fptu::format("(%" PRIiPTR " bytes, %" PRIiPTR " fields, %p)={",
                   ro.total_bytes, end - begin, ro.units);
  for (auto i = begin; i != end; ++i) {
    if (i != begin)
      result.append(", ");
    result.append(to_string(*i));
  }
  result.push_back('}');
  return result;
}

__cold string to_string(const fptu_rw &rw) {
  const void *addr = std::addressof(rw);
  const fptu_field *const begin = fptu::begin(rw);
  const fptu_field *const end = fptu::end(rw);
  string result =
      fptu::format("(%p, %" PRIiPTR " fields, %" PRIuPTR " bytes, %" PRIuPTR
                   " junk, %" PRIuPTR "/%" PRIuPTR " space, "
                   "H%u_P%u_T%u_E%u)={",
                   addr, end - begin, units2bytes(rw.tail - rw.head),
                   fptu_junkspace(&rw), fptu_space4items(&rw),
                   fptu_space4data(&rw), rw.head, rw.pivot, rw.tail, rw.end);

  for (auto i = begin; i != end; ++i) {
    if (i != begin)
      result.append(", ");
    result.append(to_string(*i));
  }
  result.push_back('}');
  return result;
}
#endif

__cold string to_string(const fptu_lge lge) {
  switch (lge) {
  default:
    return fptu::format("invalid(fptu_lge)%i", (int)lge);
  case fptu_ic:
    return "><";
  case fptu_eq:
    return "==";
  case fptu_lt:
    return "<";
  case fptu_gt:
    return ">";
  case fptu_ne:
    return "!=";
  case fptu_le:
    return "<=";
  case fptu_ge:
    return ">=";
  }
}

__cold string to_string(const fptu_datetime_t &time) {
  char fractional[16];
  /* с точностью до наносекунд,
   * поэтому внутри snprintf() может произойти округление до 1.000 */
  snprintf(fractional, sizeof(fractional), "%.9f", time.fractional_seconds());
  assert(fractional[0] == '0' || fractional[0] == '1');

  /* с учетом переноса при округлении fractional */
  const time_t utc_sec =
      static_cast<time_t>(time.utc_seconds()) + fractional[0] - '0';

  struct tm utc_tm;
#if defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__)
  gmtime_s(&utc_tm, &utc_sec);
#else
  gmtime_r(&utc_sec, &utc_tm);
#endif

  char datetime[64];
  snprintf(datetime, sizeof(datetime), "%04d-%02d-%02dZ%02d:%02d:%02d",
           utc_tm.tm_year + 1900, utc_tm.tm_mon + 1, utc_tm.tm_mday,
           utc_tm.tm_hour, utc_tm.tm_min, utc_tm.tm_sec);

  string result;
  result.reserve(31);
  result = datetime;
  result.append(fractional + /* skip leading digit before the dot */ 1);
  return result;
}

/* #define FIXME "FIXME: " __FILE__ ", " STRINGIFY(__LINE__) */

} /* namespace std */

//----------------------------------------------------------------------------

#ifdef __SANITIZE_ADDRESS__
extern "C" FPTU_API __attribute__((weak)) const char *__asan_default_options() {
  return "symbolize=1:allow_addr2line=1:"
#ifdef _DEBUG
         "debug=1:"
#endif /* _DEBUG */
         "report_globals=1:"
         "replace_str=1:replace_intrin=1:"
         "malloc_context_size=9:"
         "detect_leaks=1:"
         "check_printf=1:"
         "detect_deadlocks=1:"
#if !FPTU_LTO_ENABLED
         "check_initialization_order=1:"
#endif
         "detect_stack_use_after_return=1:"
         "intercept_tls_get_addr=1:"
         "decorate_proc_maps=1:"
         "abort_on_error=1";
}
#endif /* __SANITIZE_ADDRESS__ */
