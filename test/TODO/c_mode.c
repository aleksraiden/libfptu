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

/* test conformance for C mode */
#if !defined(fptu_EXPORTS) && !defined(fptu_IMPORTS)
#define fptu_IMPORTS
#endif /* fptu_IMPORTS */
#include <fast_positive/tuples_legacy.h>

#ifdef _MSC_VER
#pragma warning(disable : 4710) /* 'xyz': function not inlined */
#pragma warning(disable : 4711) /* function 'xyz' selected for                 \
                                   automatic inline expansion */
#pragma warning(push, 1)
#endif /* _MSC_VER (warnings) */

#include <stdio.h>

#if defined(_WIN32) || defined(_WIN64) || defined(_WINDOWS)
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#define print_value(comment, value)                                            \
  wprintf(L"%-20S = %ld\t// %S\n", #value, (long)value, comment)
#define print_mask(comment, value)                                             \
  wprintf(L"%-20S = 0x%lx\t// %S\n", #value, (long)value, comment)
#define print(text) wprintf(L"%S", text)
#else
#define print_value(comment, value)                                            \
  printf("%-20s = %ld\t// %s\n", #value, (long)value, comment)
#define print_mask(comment, value)                                             \
  printf("%-20s = 0x%lx\t// %s\n", #value, (long)value, comment)
#define print(text) puts(text)
#endif /* WINDOWS */

#ifdef _MSC_VER
#pragma warning(pop)
#endif

int main(int argc, char *argv[]) {
  (void)argc;
  (void)argv;
#if defined(_WIN32) || defined(_WIN64) || defined(_WINDOWS)
  SetConsoleOutputCP(CP_UTF8);
#endif /* WINDOWS */

  print("// ?????????????? ???????????? ?? ??????????????????:\n");
  print_value("???????????? ?????????????????? ?? ??????????", fptu_bits);
  print_value("???????????? ???????????? ?????????? ?? ????????????", fptu_unit_size);
  print_value("???????????? ???????? ?? ???????????????????????????? ????????", fptu_typeid_bits);
  print_value("???????????? ?? ???????????????????????????? ????????", fptu_ct_reserve_bits);
  print_value("???????????? ?????? ?????????? ?? ?????????????????? ??????????????", fptu_lx_bits);

  print("\n// ?????????????????????? ?????????????????? ?? ??????????????????:\n");
  print_value("log2(fptu_unit_size)", fptu_unit_shift);
  print_value("???????????????? ???????????????????? ??????????", fptu_limit);
  print_value("???????????? ????????-???????????? ????????/??????????????", fptu_co_bits);
  print_mask("?????????? ?????? ?????????????????? ???????? ???? ???????????????????????????? ????????/??????????????",
             fptu_ty_mask);
  print_mask("?????????? ?????????????????? ?????????? ?? ???????????????????????????? ????????/??????????????",
             fptu_fr_mask);
  print_value("?????????? ?????? ?????????????????? ????????-???????????? ???? ???????????????????????????? ????????/??????????????",
              fptu_co_shift);
  print_value("???????????????? ????????-???????????? ?????? ?????????????????? ??????????/??????????????", fptu_co_dead);
  print_value("??????-???? ?????? ?????????????????? ?????? ???????????????? ?????????????? "
              "?????????????? ???????????????????????? ??????????",
              fptu_lt_bits);
  print_mask("?????????? ?????? ?????????????????? ?????????????????? ?????? ???? ?????????????????? ??????????????",
             fptu_lx_mask);
  print_mask("?????????? ?????? ?????????????????? ?????????????? ?????????????? "
             "???????????????????????? ???? ?????????????????? ??????????????",
             fptu_lt_mask);

  print("\n// ???????????????? ??????????????????????:\n");
  print_value("???????????????????????? ?????????????????? ???????????? "
              "???????????????????????????????? ?????????????????????????? "
              "??????????????",
              fptu_max_tuple_bytes);
  print_value("???????????????????????? ??????-?????????? ????????/??????????????", fptu_max_cols);
  print_value("???????????????????????? ??????-???? ??????????/?????????????? ?? ?????????? ??????????????",
              fptu_max_fields);
  print_value("???????????????????????? ???????????? ????????/??????????????", fptu_max_field_bytes);
  print_value("???????????????????????? ???????????? ???????????????????????? ???????????????????????????????????? ????????",
              fptu_max_opaque_bytes);
  print_value("???????????????????????? ??????-???? ?????????????????? ?? ??????????????", fptu_max_array_len);

  print("\n// ???????????????????????? ?????????????? ??????????????:\n");
  print_value("?????????? ???????????????????????? ?????????????? ?????? ???????????? ??????????????",
              fptu_buffer_enough);
  print_value("???????????????????? ???????????? ?????? ????????????????????????????, "
              "???????????????????? ???????????????? ?????????????????? ??????????????",
              fptu_buffer_limit);

#if HAVE_FPTU_VERSIONINFO
  printf("\n libfptu version %s: %s, %d.%d.%d.%d,\n\tcommit %s, tree %s\n",
         fptu_version.git.describe, fptu_version.git.datetime,
         fptu_version.major, fptu_version.minor, fptu_version.release,
         fptu_version.revision, fptu_version.git.commit, fptu_version.git.tree);
#endif /* HAVE_FPTU_VERSIONINFO */

  printf("\n libfptu build %s: %s, %s,\n\t%s,\n\t%s\n", fptu_build.datetime,
         fptu_build.target, fptu_build.compiler, fptu_build.cmake_options,
         fptu_build.compile_flags);

  print("\nless Windows, no Java, no Problems ;)\n");
  return 0;
}
