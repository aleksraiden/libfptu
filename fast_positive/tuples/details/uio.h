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
#ifdef __cplusplus
#include <cstddef>
#include <cstdint>
#else
#include <stddef.h>
#include <stdint.h>
#endif

#if defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__)
struct iovec {
  void *iov_base;      /* Starting address */
  std::size_t iov_len; /* Number of bytes to transfer */
};
#else
#include <sys/uio.h> // for struct iovec
#endif

#ifdef __cplusplus

namespace fptu {

struct FPTU_API_TYPE iovec : public ::iovec {
  cxx11_constexpr iovec() noexcept : ::iovec({nullptr, 0}) {}

  cxx11_constexpr iovec(const void *data, std::size_t size) noexcept
      : ::iovec({const_cast<void *>(data), size}) {}

  cxx14_constexpr iovec(const ::iovec &io) noexcept : ::iovec(io) {}

  cxx14_constexpr iovec &operator=(const ::iovec &io) noexcept {
    iov_base = io.iov_base;
    iov_len = io.iov_len;
    return *this;
  }

  cxx14_constexpr void set(const void *data, std::size_t size) noexcept {
    iov_base = const_cast<void *>(data);
    iov_len = size;
  }

  cxx11_constexpr std::size_t size() const noexcept { return iov_len; }
  cxx11_constexpr const uint8_t *data() const noexcept {
    return static_cast<const uint8_t *>(iov_base);
  }
  cxx11_constexpr const uint8_t *end() const noexcept {
    return data() + size();
  }
  cxx11_constexpr uint8_t *data() noexcept {
    return static_cast<uint8_t *>(iov_base);
  }
  cxx11_constexpr uint8_t *end() noexcept { return data() + size(); }

  cxx11_constexpr bool equal(const ::iovec &io) const noexcept {
    return this->iov_base == io.iov_base && this->iov_len == io.iov_len;
  }

  cxx11_constexpr bool not_equal(const ::iovec &io) const noexcept {
    return !equal(io);
  }
};
} // namespace fptu

#endif /* __cplusplus */
