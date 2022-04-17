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
#include "fast_positive/erthink/erthink_128.h"
#include "fast_positive/tuples/api.h"

#ifdef __cplusplus

namespace fptu {
using uint128_t = erthink::uint128_t;
using int128_t = erthink::int128_t;
} // namespace fptu

using fptu_uint128_t = erthink::uint128_t;
using fptu_int128_t = erthink::int128_t;

#else

typedef erthink_uint128_t fptu_uint128_t;
typedef erthink_int128_t fptu_int128_t;

#endif /* __cplusplus */
