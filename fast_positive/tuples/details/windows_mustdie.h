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

#if defined(_WIN32) || defined(_WIN64) || defined(__CYGWIN__)

#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif /* WIN32_LEAN_AND_MEAN */

#ifndef NOCOMM
#define NOCOMM
#endif /* NOCOMM */

#ifndef NOMINMAX
#define NOMINMAX
#endif /* NOMINMAX */

#ifndef _WIN32_WINNT
#ifdef WINVER
#define _WIN32_WINNT WINVER
#else
#define _WIN32_WINNT 0x0502
#endif
#endif /* _WIN32_WINNT */

#ifndef WINVER
#define WINVER _WIN32_WINNT
#endif /* WINVER */

#ifdef DEFINE_ENUM_FLAG_OPERATORS
#undef DEFINE_ENUM_FLAG_OPERATORS
#endif /* DEFINE_ENUM_FLAG_OPERATORS */

#include "warnings_push_system.h"

#include <windows.h>

#include "warnings_pop.h"
#endif /* Windows */
