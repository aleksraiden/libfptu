﻿libfptu
==============================================
Fast Positive Tuples, aka "Позитивные Кортежи"
by [Positive Technologies](https://www.ptsecurity.ru).

Машинно-эффективный формат линейного представления небольших структур
данных для (де)сериализации, обмена сообщениями и размещения в
разделяемой памяти.

Machine-handy format for linear representation of small data structures
for (de)serialization, messaging and placement in shared memory.
English translation [by Google](https://translate.googleusercontent.com/translate_c?act=url&ie=UTF8&sl=ru&tl=en&u=https://github.com/leo-yuriev/libfptu/tree/devel)
and [by Yandex](https://translate.yandex.ru/translate?url=https%3A%2F%2Fgithub.com%2Fleo-yuriev%2Flibfptu%2Ftree%2Fdevel&lang=ru-en).

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Build Status](https://travis-ci.org/leo-yuriev/libfptu.svg?branch=devel)](https://travis-ci.org/leo-yuriev/libfptu)
[![Build status](https://ci.appveyor.com/api/projects/status/8617mtix9paivmkx/branch/devel?svg=true)](https://ci.appveyor.com/project/leo-yuriev/libfptu/branch/devel)
[![CircleCI](https://circleci.com/gh/leo-yuriev/libfptu/tree/devel.svg?style=svg)](https://circleci.com/gh/leo-yuriev/libfptu/tree/devel)
[![Coverity Scan Status](https://scan.coverity.com/projects/12919/badge.svg)](https://scan.coverity.com/projects/leo-yuriev-libfptu)


## Кратко

"Позитивные Кортежи" - это _простой_ формат представления небольших
структур данных в линейном, удобном для машины виде. Библиотека
_libfptu_ реализует поддержку формата "Позитивных кортежей",
предоставляя интерфейс C++14 и выше.

Можно сказать, что _libfptu_ предлагает гибкость JSON, скорость нативных
структур языка _C_, возможности boost::optional и boost::variant.
Однако, в некоторых сценариях _libfptu_ может оказаться неудобной и
несколько суровой, так как не предлагает возможностей, которые не могут
быть эффективно выполнены машиной.

В актуальной версии _libfptu_ появилась поддержка схемы данных и
предварительно размещенных полей (aka preplaced), включая привязку к
нативным структурам языка _C_.

Стоит также упомянуть возможность размещения и совместного использования
(чтения) кортежей в разделяемой памяти. С целью расширения этих
возможностей в актуальную версию _libfptu_ была импортирована часть
инфраструктуры управления разделяемыми буфера проекта 1Hippeus.

## Содержание

- [Отличия от MessagePack, Protocol Buffers, BJSON](#Отличия-от-messagepack-protocol-buffers-bjson)
- [Обзор](#Обзор)
    - [Preplaced Fields](#preplaced-fields)
    - [Loose Fields](#loose-fields)
    - [Типы данных](#Типы-данных)
    - [Токены доступа](#Токены-доступа)
    - [Справочник схемы](#Справочник-схемы)
    - [Коллекции и итераторы](#Коллекции-и-итераторы)
- [Использование](#Использование)
    - [Схема данных](#Схема-данных)
        - [Preplaced или Loose](#preplaced-или-loose)
        - [NIL или не-NIL](#nil-или-не-nil)
        - [Операции с насыщением](#Операции-с-насыщением)
        - [Заполнение справочника](#Заполнение-справочника)
    - [Изменяемая и сериализованная формы](#Изменяемая-и-сериализованная-формы)
    - [(Де)Сериализация](#ДеСериализация)
    - [Создание и наполнение](#Создание-и-наполнение)
    - [Чтение полей](#Чтение-полей)
    - [Устойчивость к некорректным данным](#Устойчивость-к-некорректным-данным)
- [Внутри](#Внутри)
    - [Формат](#Формат)
    - [Изменяемая и сериализованная формы](#Изменяемая-и-сериализованная-формы)

## Отличия от MessagePack, Protocol Buffers, BJSON

**1. Легковесность и удобство для машины.** Объём кода минимален, а
внутренняя структура линейна и проста.
 > "Позитивные кортежи" не предлагают лишнего, в том числе IDL и
 > кодогенератор, но могут быть также эффективны как нативные структуры,
 > при этом поддерживая строки и опциональные поля в одном линейном участке
 > памяти.

**2. Немного объёмнее чем MessagePack.** Данные хранятся в нативном
машинном представлении, без сжатия. Поэтому для каждого поля _может_
потребоваться на 3-4 байта больше.
 > Тем не менее, следует аккуратно интерпретировать эти цифры. В
 > экстремальном случае, когда много 64-битных целочисленных полей с
 > близкими к нулю значениями, представление в _libfptu_ может потребовать
 > до 12 раз больше памяти в сравнении с MessagePack (1 байт в MessagePack,
 > против 12 байт в _libfptu_).

**3. Очень быстрый доступ.** _libfptu_ позволяет сочетать скорость
прямого доступа к полям традиционных структур и подход разреженных
множеств с эффективным поиском в индексе.
 > Для эффективного доступа к полям кортежа достаточно его "сырого"
 > представления "как есть" в линейном участке памяти, без какой-либо
 > подготовки, без каких-либо преобразований, изменений и манипуляций.
 > Получения поля из кортежа, в худшем случае сводится, к поиску его
 > дескриптора в заголовке. Что требует чтения лишь одной кэш-линии для
 > первых 15 полей и далее на каждые 16 последующих. При необходимости
 > доступ к полям может быть организован без накладных расходов, с такой-же
 > эффективностью как к полям нативных структур определенных во время
 > компиляции кода.

**4. Непосредственная (де)сериализация.** _libfptu_ формирует и растит
кортеж как линейную последовательность байт, просто дописывая данные.
Поэтому (де)сериализация в "Позитивные Кортежи" магически быстрая.
 > Заполнение кортежа происходит без лишних операций, преимущественно
 > простым однократным копированием данных в заранее выделенный буфер.
 > При этом сериализованное представление всегда готово, достаточно
 > получить указатель и размер.

**5. Изменения данных дешевы.** В отличии от традиционных кортежей, поля
можно добавлять, удалять и перезаписывать, в том числе с изменением
размера.
 > При этом в линейном представлении могут образовываться неиспользуемые
 > зазоры, а у вас появляется выбор: пожертвовать местом или
 > использовать процедуру дефрагментации, которая, в худшем случае,
 > не дороже однократного копирования содержимого кортежа.

Однако, "Позитивные Кортежи" не являются серебряной пулей и вероятно не
подойдут, если:

 * В структурах более 1000 полей;
 * Размер одной структуры более 200 килобайт;
 * Минимизация объема важнее скорости доступа и затрат на (де)сериализацию.

************************************************************************

## Обзор

"Позитивные кортежи" ориентированы на эффективную машинную обработку
и компактное представление структурно простых данных. Поэтому:

* Не содержат имен полей и другой информации, которая не требуется
машине. Соответственно "Позитивные кортежи" нельзя отнести к
само-описывающимся форматам представления данных.

* Не используют полноценную схему данных, а только опциональный
минималистический справочник. При этом, по выбору разработчика, можно
сочетать без-схемные сценарии использования с полностью и/или частично
зафиксированной схемой.

* Не включают идентификаторов для различения структурных типов.

Размер кортежей ограничен 250 килобайтами и 8 тысячами _экземпляров_
полей. Поля внутри кортежей строго типизированы и идентифицируются
легковесными _токенами_, которые используются для доступа к полям через
API. Можно сказать, что токены являются компактными именами полей в
удобной для машины форме. В свою очередь, справочник схемы обеспечивает
трансляцию имен полей в токены.

Поля предлагаются двух видов: предварительно размещенные (preplaсed) и
свободные (loose). Основное отличие между ними в разных компромиссах
между скоростью доступа и расходуемым местом. Доступ к полям
производится единообразно, вне зависимости от вида поля.

### Preplaced Fields

Предварительно размещенные или **preplaced-поля аналогичны полям в
структурах языка _`C`_**, физически всегда присутствуют и однозначно
определяются схемой.

В кортеже, соответствующим некоторой схеме, координаты preplaced-поля
всегда известны и зависят только от схемы. Поэтому обращение к
preplaced-полю предполагает наличие схемы данных, но не требует
какого-либо поиска и равнозначно чтению данных по фиксированному
смещению. В свою очередь, токен доступа любого preplaced-поля содержит
такое смешение.

Preplaced-поля физически всегда располагаются в начале кортежей. При
этом для типов c переменной длинной данных (например строк)
непосредственно внутри поля хранится лишь смещение к данным, которые
располагаются вместе с другими вариативными элементами в конце кортежа.

Накладные расходы доступа к preplaced-полям могут быть сведены к нулю
посредством генерации кода привязанного к схеме данных, которая
полностью или частично фиксируется. Технически это сводится к
использованию статических токен-классов, при использовании которых
компилятор способен сгенерировать inline-код непосредственного доступа к
данным.

При необходимости _libfptu_ может эмулировать отсутствие
preplaced-полей. Для этого, в зависимости от типа данных, одно из
возможных значений поля резервируется для обозначения логической
"пустоты". Например, для целочисленных типов со знаком в качестве таких
DENIL-значению (designated NIL) используется крайнее отрицательное
значение (INT_MIN).

### Loose Fields

Свободные или **loose-поля всегда опциональны и занимают место только при
присутствии в кортеже**. Для присутствующих loose-полей в начале кортежа
поддерживается компактный индекс, в котором идентифицируется номером
поля и типом данных. Таким образом, loose-поля всегда несут информации о
типе своих данных, являются частично само-описывающимися и позволяют
без-схемные сценарии использования.

При необходимости **loose-поля могут многократно повторяться** в кортеже,
образую таким образом _неупорядоченные_ итерируемые коллекции,
аналогично `repeated fields` в Protocol Buffers. Для этого определение
поля должно быть определено в схеме как "коллекция".

Для поиска loose-полей в индексе _libfptu_ использует как
последовательное сканирование с акселерацией SSE2/AVX/AVX2/AVX512, так и
сортировку с двоичным поиском.

Внутри _libfptu_ добавление loose-поля в кортеж приводит к дозаписи
дескриптора в начало индекса и дозаписи данных в конец кортежа.
Обращение к loose-полю сводится к поиску соответствующего дескриптора в
индексе, а затем чтению его значению по хранимому в дескрипторе
смещения.

Удаление loose-полей, а также обновление значений полей вариативной
длины (строки, бинарные строки, вложенные кортежи), может приводить к
образованию внутри кортежа неиспользуемых участков, которые
ликвидируются дефрагментацией. Такая дефрагментация не дороже
однократного копирования кортежа и не является обязательной.

### Типы данных

Набор типов зафиксирован и включает все распространенные нативные
(машинные) типы, а также строки, дата/время, произвольные
последовательности байт, IP-адреса, IP-сети, MAC-адреса и дайджесты.

Полный набор типов зафиксирован в определении [`enum
fptu::genus`](fast_positive/details/essentials.h), здесь же стоит
упомянуть некоторые особенности:

- `text` = Строки UTF8 длиной до 262129 байт. Во внутреннем
представлении длина строк хранится в явном виде, терминирующий ноль
не используется, но допустим внутри строк.

- `varbin` = Произвольные последовательности байт (бинарные строки)
размером до 262128 байт.

- `nested` = Вложенный кортеж.

- `property` = Пара из однобайтового идентификатора и небольшого
бинарного объекта (последовательность байт) размером до 253 байт.

- `datetime_utc` = Дата/время в форме 32-битного беззнакового целого
числа секунд с начала Unix-эпохи 1970-01-01 00:00:00 +0000 (UTC).

- `datetime_h100` = Дата/время в форме фиксированной точки 32-dot-32
унифицированной с "Positive Hiper100re".

- `decimal` = 64-битное дробное в форме [плавающей точки с десятичной
экспонентой](https://en.wikipedia.org/wiki/Decimal_floating_point).

- `bin96`..`bin512` = Бинарный блоки размером
96/128/160/192/224/256/320/384/512 бит.

- `app_reserved_64` и `app_reserved_128` = зарезервированные за
приложением типы размером 64 и 128 бит.

Предусматривается вложенность кортежей, но в угоду легковесности и
производительности не предлагает для этого элегантного автоматизма. В
целом, для представления вложенных структур возможны два подхода:

1. Проекция, проще говоря, расширение имен:
   делаем `{ "ФИО.Имя": "Иван", "ФИО.Фамилия": "Петров" }`
   вместо `{ "ФИО": { "Имя": "Иван", "Фамилия": "Петров" } }`

2. Вложенная сериализация, когда сначала отдельно формируется кортеж с
   `"ФИО"`, а затем целиком вкладывается в родительский кортеж.

### Токены доступа

Токены являются компактными, удобными для машины идентификаторами полей
и включают всю необходимую информацию для организации доступа к ним.

Каждый токен содержит тип данных и признак "заметности пустоты",
позволяющий управлять поведением подставляя нулевые значение при чтении
отсутствующих loose-полей, а также эмулировать отсутствие
preplaced-полей. Полное описание `discernible_null` признака можно найти
в описании метода
[`token_operations<>::is_discernible_null()`](fast_positive/details/token.h).

Кроме общих обязательных атрибутов, токены содержат разную информацию в
зависимости от вида поля. Так токены preplaced-полей содержат смещение
для непосредственно доступа к полю внутри кортежа. А токены loose-полей
содержат тэг для поиска поля в индексе и признак коллекции (флажок
повторяемости поля).

С точки зрения `C++` токены являются экземплярами классов с необходимым
набором методов. При этом следует различать _динамические токены_ и
_статические токен-классы_:

* **_Динамические токены_** хранят собственные не-статические значения
всех атрибутов в каждом экземпляре класса
[`fptu::token`](fast_positive/details/token.h). Разные экземпляры этого
класса могут соответствовать разным полям. Соответственно, _динамические
токены_ можно присваивать, получать от справочника схемы и
конструировать при выполнении программы.

    Доступ к полям через такие токены унифицирован, но требует вызов функций
    с многочисленными условными ветвлениями внутри.

* **_Статические токен-классы_** не хранят какие-либо значения, а
являются "пустотелыми" классами, состоящими только из inline-методов,
которые возвращают константные значения одинаковые для всех экземпляров.
Каждый статический токен-класс и все его экземпляры всегда соответствует
только одному полю. Соответственно, _статические токен-классы_ нельзя
присваивать, а определять можно только в исходном коде и/или во время
компиляции программы посредством [макроса
`FPTU_TOKEN`](fast_positive/details/token.h) или [шаблона
fptu::token_static<>](fast_positive/details/token.h).

    Доступ к полям через статические токен-классы внешне выглядит также как
    и для динамических токенов. Однако, в случае статических токен-классов,
    _libfptu_ предоставляет шаблонные inline-методы, что позволяет
    компилятору сгенерировать оптимальный код без лишних проверок и условных
    переходов, в том числе с непосредственным доступом к данным для
    preplaced-полей.

Кроме статических и динамических токенов доступны шаблоны
`cast_typecheck<TOKEN>`, `cast_preplaced<TOKEN>` и `cast_loose<TOKEN>`
позволяющие получить из динамических токенов частично статические, что
также позволяет использовать шаблонную генерацию кода с меньшим
количеством условных ветвлений.

Следует отметить, что динамические и статические токены можно
сравнивать, а динамические создать из статических. В целом это позволят
выбрать желаемый компромисс между гибкостью и производительность,
сочетая фиксирование части схемы на стадии компиляции с динамическим
до-определением во время выполнения. При этом рекомендуется использовать
следующий подход:

- Формируется внешнее формальное описание схемы.

- Аналитически формируется список "горячих" полей, для доступа к которым
требуется предельная эффективность.

- На основании списка "горячих" полей, непосредственно в исходном коде,
формируются декларация соответствующих структур. Для всех определенных
таким образом полей, при помощи макроса `FPTU_TOKEN` генерируются
статические токен-классы. Доступ к этим полям производится либо через
статические токен-классы, либо через кастинг указателей как к полям
нативных структур.

- во время выполнения, на основании загружаемого формального описания
схемы, строится внутренний справочник схемы, а ранее сгенерированные
статические-токен классы сверяются (сравниваются) с соответствующими
токенами полей в справочнике схемы.

- Таким образом, используются все преимущества фиксированной схемы и
нативной генерации кода, одновременно с верификацией соответствия
скомпилированного кода и актуального загруженного описания схемы данных.

### Справочник схемы
TBD

### Коллекции и итераторы
TBD

Можно проитерировать все поля в кортеже, это быстро и дешево:

 * Можно итерировать с фильтрацией по тегу/номеру
   и битовой маске типов;
 * При итерации у каждого поля можно спросить тэг/номер,
   тип и значение;
 * Итератор остается валидным до разрушения или до
   компактификации кортежа;
 * При итерации любое количество полей можно как удалить,
   так и добавить;
 * Добавленные в процессе итерации поля можно как увидеть
   через итератор, так и не увидеть.

Однако, следует считать, что порядок полей при итерации не определен и
никак не связан с их порядком добавления или удаления. В частности,
поэтому нет (и не будет) итерации в обратном порядке.


************************************************************************

## Использование
TBD

### Схема данных
TBD

#### Preplaced или Loose
TBD

#### NIL или не-NIL
TBD

#### Операции с насыщением
TBD

#### Заполнение справочника
TBD


### Изменяемая и сериализованная формы
TBD

### (Де)Сериализация
TBD

### Создание и наполнение
TBD

### Чтение полей
TBD

### Устойчивость к некорректным данным

Постоянная проверка корректности данных слишком дорога и как-правило
избыточна. С другой стороны, любые нарушениях в десериализуемых данных
не должны приводить к авариям.

Поэтому в _libfptu_ эксплуатируется следующий принцип:

1. Доступны функции верификации сериализованной и изменяемой форм
кортежа, которые вы используете по своему усмотрению.

2. В угоду производительности, основные функции выполняют только
минимальный контроль корректности аргументов и предоставляемых данных.
Поэтому при мусорных (не валидных) данных их поведение не определено.

3. Гарантируется, что прошедшие проверку данные не вызовут нарушений при
дальнейшей работе с ними.

Более подробная информация пока доступна только в виде [заголовочного файла API](fast_positive/tuples.h).

************************************************************************

## Внутри

### Формат

Физически кортеж представляет собой линейный участок памяти, в начале
которого расположен компактный индекс для быстрого доступа к
опциональным полям. Сразу за индексом располагаются полезные данные,
т.е. собственно значения полей кортежа. Таким образом, как сериализация,
так десериализация кортежа равноценны однократному
чтению/записи/копированию линейного участка памяти.

Формат представления кортежей ориентирован на машину. Все данные в
бинарном машинном виде, порядок байт строго нативный (определяется
архитектурой или режимом работы CPU):

* Сначала идет "заголовок" кортежа, в котором указано количество
loose-полей (размер индекса) и служебные признаки.

* Затем следует индекс loose-полей, представляющий собой массив из
32-битных дескрипторов присутствующих в кортеже loose-полей. Каждый
элемент-дескриптор в индексе содержит идентификатор поля, **тип данных**
и **смещение** к ним относительно дескриптора.

* После индекса следуют preplaced-поля, строго согласно схеме, которой
соответствует кортеж.

* За индексом после preplaced-полей следуют данные loose-полей и
prepaced-полей переменного размера. При этом в данных допустимы зазоры,
как они образовались в ходе формирования кортежа при изменении значений
и удалении полей, если пользователь счет ненужным запросить
дефрагментацию.

* Каждый loose-дескриптор и связанные с ним данные выровнены на
4х-байтовую границу. Выравнивание preplaced-полей определяется схемой.

Для коротких типов данных, которые помещается в 16 бит, значения
хранятся непосредственно в дескрипторе вместо смещения.

Для всех полей переменной длины (строк, вложенных кортежей и т.д.), в
первом 32-битном слове кодируется их размер, но способ кодировки зависит
от типа данных.

Строки хранятся только в кодировке UTF-8 с явным указанием длины.
Терминирующий `'\0'` не используется, но допустим внутри строк.

Формат первого слова для вложенных кортежей и корневого кортежа
полностью совпадает с небольшой оговоркой:

* В самостоятельном виде пустой кортеж может быть представлен как `ноль
байт` (пустой строкой байт), так и минимальным заголовком, в котором
указано `ноль элементов`.

* Вложенный кортеж является полем, поэтому при присутствии поля всегда
обязан иметь заголовок с информацией о своем нулевом размере.


### Изменяемая и сериализованная формы

Сериализованная форма кортежа _libfptu_ - это линейный участок памяти,
который одновременно является массивом 32-битных ячеек. В начале
располагается информация о количестве полей/колонок и общем размере
кортежа. Далее следует список дескрипторов. Затем располагаются
preplaced-поля, а за ними значения полей, включая значения
preplaced-полей переменного размера.

Создание и наполнение кортежа происходит в слегка отличающейся
"изменяемой" форме - это также линейный участок памяти, но выделенный с
учетом ожидаемого размера кортежа и дополнительного места для нескольких
служебных счетчиков. Проще говоря, изменяемая форма кортежа является
"обложкой" создаваемого сериализованного кортежа, но с резервирования
дополнительного места:

* изменяемая форма кортежа живет в буфере, который выделяется в расчете
на ожидаемый размер (как по количеству элементов, так и по их данным);

* внутри выделенного буфера располагаются служебные счетчики, а также
растет сериализованная форма кортежа;

* получение сериализованной формы из изменяемой сводится к формированию
информации о текущем размере кортежа и возврате указателя на его начало;

* получение изменяемой формы из сериализуемой сводится к копированию
кортежа внутрь выделенного буфера, размер которого должен включать запас
на служебные счетчики и добавляемые данные.


         buffer of sufficient size
        |<=======================================================>|
        |                                                         |
        |   head         pivot                             tail   |
        |   <-----~~~~~~~~~|~~~~~~~~~~~~~~~~~~---------------->   |
        |       descriptors|payload                               |
                           |
                  #_D_C_B_A_aaa_bb_cccccc_dddd
                  |                          |
                  |<========================>|
                    linear uint32_t sequence
                       for serialization

--------------------------------------------------------------------------------

```
$ objdump -f -h -j .text libfptu.so

libfptu.so:     file format elf64-x86-64
architecture: i386:x86-64, flags 0x00000150:
HAS_SYMS, DYNAMIC, D_PAGED
start address 0x00016da0

Sections:
Idx Name          Size      VMA       LMA       File off  Algn
 11 .text         0001f590  00016da0  00016da0  00016da0  2**4
                  CONTENTS, ALLOC, LOAD, READONLY, CODE
```

```
$ ldd libfptu.so
	linux-vdso.so.1 (0x00007fffb5b3d000)
	libstdc++.so.6 => /usr/lib/x86_64-linux-gnu/libstdc++.so.6 (0x00007fdbfcd29000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007fdbfc98b000)
	libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007fdbfc773000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fdbfc382000)
	/lib64/ld-linux-x86-64.so.2 (0x00007fdbfd34d000)
```
