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
 *
 * ***************************************************************************
 *
 * Imported (with simplification) from 1Hippeus project.
 * Copyright (c) 2006-2013 Leonid Yuriev <leo@yuriev.ru>.
 */

#include "fast_positive/tuples/api.h"
#include "fast_positive/tuples/details/bug.h"

#include "fast_positive/erthink/erthink_casting.h++"
#include "fast_positive/erthink/erthink_defs.h"
#include "fast_positive/erthink/erthink_intrin.h"
#include "fast_positive/tuples/1Hippeus/utils.h"
#include <array>

#include "fast_positive/tuples/details/posix_modern.h"
#include "fast_positive/tuples/details/windows_mustdie.h"

namespace hippeus {

namespace {

template <typename T, unsigned SIZE = sizeof(T)> struct native_type;
template <typename T> struct native_type<T, 1> {
  using signed_integer = int8_t;
  using unsigned_integer = uint8_t;
};
template <typename T> struct native_type<T, 2> {
  using signed_integer = int16_t;
  using unsigned_integer = uint16_t;
};
template <typename T> struct native_type<T, 4> {
  using signed_integer = int32_t;
  using unsigned_integer = uint32_t;
};
template <typename T> struct native_type<T, 8> {
  using signed_integer = int64_t;
  using unsigned_integer = uint64_t;
};
using machine_word = native_type<uintptr_t>::unsigned_integer;

/* LY: выдает маску с 0xFF для первых n-байт в порядке адресации. */
template <typename word> cxx11_constexpr word aheadmask(size_t bytes) {
  CONSTEXPR_ASSERT(bytes > 0 && bytes < sizeof(word));
#if __BYTE_ORDER == __LITTLE_ENDIAN
  return (~word(0)) >> 8 * (sizeof(word) - bytes);
#elif __BYTE_ORDER == __BIG_ENDIAN
  return (~word(0)) << 8 * (sizeof(word) - bytes);
#else
#error WTF __BYTE_ORDER ?
#endif
}

/* LY: выдает маску с 0xFF для последних n-байт в порядке адресации. */
template <typename word> static cxx11_constexpr word tailmask(size_t bytes) {
  CONSTEXPR_ASSERT(bytes > 0 && bytes < sizeof(word));
#if __BYTE_ORDER == __LITTLE_ENDIAN
  return (~word(0)) << 8 * (sizeof(word) - bytes);
#elif __BYTE_ORDER == __BIG_ENDIAN
  return (~word(0)) >> 8 * (sizeof(word) - bytes);
#else
#error WTF __BYTE_ORDER ?
#endif
}

template <typename F>
__always_inline machine_word *uintptr_applier(machine_word *ptr,
                                              ptrdiff_t bytes, F functor) {
  const machine_word whole = ~machine_word(0);
  const std::size_t unalign =
      erthink::bit_cast<machine_word>(ptr) % sizeof(machine_word);

  if (unalign) {
    ptr = erthink::bit_cast<machine_word *>(
        erthink::bit_cast<machine_word>(ptr) - unalign);
    bytes -= sizeof(machine_word) - unalign;
    machine_word gatemask = whole;
    if (bytes < 0)
      gatemask >>= (unalign - bytes) * 8;

    gatemask <<= unalign * 8;
    if (functor(ptr, gatemask))
      return ptr;

    ptr++;
  }

  while (bytes >= ptrdiff_t(sizeof(machine_word))) {
    if (functor(ptr))
      return ptr;

    ptr++;
    bytes -= sizeof(machine_word);
  }

  if ((bytes > 0) && functor(ptr, aheadmask<machine_word>(bytes)))
    return ptr;

  return nullptr;
}

template <bool congruential> class pollute_functor {
  machine_word value_;

public:
  pollute_functor(machine_word seed) : value_(seed) {}

  bool operator()(machine_word *ptr, machine_word gatemask) {
    assert(gatemask > 0);
    assert(gatemask != machine_word(0));

    if (congruential)
      value_ = linear_congruential(value_);

    /* LY: valgrind здесь детектирует выход за границы региона памяти.
     * Однако, это не является ошибкой. Доступ производится выровненными
     * словами, при этом изменение байтов вне заданных границ блокируется
     * битовой маской. Для проверки корректности поведения есть юнит-тест. */
    *ptr ^= value_ & gatemask;
    return false;
  }

  bool operator()(machine_word *ptr) {
    if (congruential)
      value_ = linear_congruential(value_);

    *ptr ^= value_;
    return false;
  }
};

struct probe4zero_functor {
  bool operator()(machine_word *ptr, machine_word gatemask) {
    /* LY: valgrind здесь детектирует выход за границы региона памяти.
     * Однако, это не является ошибкой. Доступ производится выровненными
     * словами, при этом изменение байтов вне заданных границ блокируется
     * битовой маской. Для проверки корректности поведения есть юнит-тест. */
    return (*ptr & gatemask) != 0;
  }
  bool operator()(machine_word *ptr) { return *ptr != 0; }
};

size_t system_pagesize() {
#if defined(_WIN32) || defined(_WIN64)
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
#else
  return sysconf(_SC_PAGE_SIZE);
#endif
}

} // namespace

//------------------------------------------------------------------------------

const std::size_t pagesize = system_pagesize();

void probe_pages(const void *addr, ptrdiff_t bytes, bool rw,
                 std::size_t step_pagesize) {
  const volatile char *ptr = static_cast<const volatile char *>(addr);
  for (ptrdiff_t offset = 0; likely(offset < bytes); offset += step_pagesize) {
    /* SIGSEGV in case wrong/unmaped pages */
    char probe = ptr[offset];
    if (unlikely(rw)) {
      /* compare and swap a byte, just source to the same value. */
#ifdef _MSC_VER
      probe = _InterlockedCompareExchange8(
          const_cast<volatile char *>(ptr + offset), probe, probe);
#else
      probe = __sync_val_compare_and_swap(
          const_cast<volatile char *>(ptr + offset), probe, probe);
#endif
    }
    (void)probe;
  }
}

void __flatten pollute(void *ptr, ptrdiff_t bytes, uintptr_t xormask) {
  if (likely(bytes > 0)) {
    if (xormask)
      uintptr_applier(static_cast<machine_word *>(ptr), bytes,
                      pollute_functor<false>(xormask));
    else {
      machine_word seed =
          /*cpu_ticks() ^*/ bytes ^ erthink::bit_cast<machine_word>(ptr);
      uintptr_applier(static_cast<machine_word *>(ptr), bytes,
                      pollute_functor<true>(seed));
    }
  }
}

bool __flatten is_zeroed(const void *ptr, ptrdiff_t bytes) {
  return bytes > 0 &&
         !uintptr_applier(static_cast<machine_word *>(const_cast<void *>(ptr)),
                          bytes, probe4zero_functor());
}

size_t log2size(size_t value, unsigned log2min, unsigned log2max,
                unsigned log2step) {
  assert(log2min < log2max);
  assert((~(size_t)0) > (((size_t)1) << log2max));
  assert(log2step > 0);
  assert((log2max - log2min) % log2step == 0);

  const std::size_t lower = ((size_t)1u) << log2min;
  const std::size_t upper = ((size_t)1u) << log2max;

  std::size_t result = lower;
  // LY: Binary search in logarithmic space is likely faster,
  // but only in pre-computed array.
  while (value > result) {
    result <<= log2step;
    if (unlikely(result >= upper))
      return upper;
  }

  return result;
}

/* LY: Занимает страницы используя iov-запись,
 * чтобы при нехватке памяти получить ошибку вместо буйства OOM-killer. */
__must_check_result int prefault_oomsafe(const void *addr, ptrdiff_t bytes,
                                         std::size_t step_pagesize) {
  const uint8_t *ptr = static_cast<const uint8_t *>(addr);
#if defined(_WIN32) || defined(_WIN64)
  (void)ptr;
  (void)bytes;
  (void)step_pagesize;
  return 0;
#else
  int fd = ::open("/dev/null", O_WRONLY);
  if (fd < 0)
    return -1;

  int rc = 0;
  for (ptrdiff_t offset = 0; offset < bytes;) {
    std::array<struct iovec, IOV_MAX> iov;
    unsigned i;
    for (i = 0; i < iov.size() && offset < bytes; ++i) {
      iov[i].iov_base = (void *)(ptr + offset);
      iov[i].iov_len = 1;
      offset += step_pagesize;
    }
    if (::writev(fd, iov.data(), i) < 0) {
      if (errno == EFAULT)
        errno = ENOMEM;
      rc = -1;
      break;
    }
  }

  FPTU_ENSURE(::close(fd) == 0);
  return rc;
#endif
}

} // namespace hippeus
