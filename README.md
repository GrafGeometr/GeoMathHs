# Geomath

## Установка

1. Установите haskell (проще всего через [ghcup](https://www.haskell.org/ghcup/))
2. Для сборки проекта нужны версия `ghc 9.2.7`. Настроить это можно так:
   ```
   ghcup install ghc-9.2.7
   ghcup set ghc 9.2.7
   ```
3. Склонируйте репозиторий
4. `cabal build`
5. Для комфортной разработки рекомендую установить vs code с плагином Haskell. Для этого также потребуется haskell language server: `ghcup install hls`

## Запуск

`cabal run`

Теперь сайт доступен по [здесь](http://localhost:8000)

## Структура проекта

### `Types.hs`

Определения всех типов данных

### `DB.hs`

Реализация простой базы данных. Мы не используем какую-либо внешнюю базу данных для упрощения конфигурации проекта. Если будет нужно, то с текущей реализации можно быстро перейти на любую БД.

### `App.hs`

Здесь реализована монада приложения, задекларированы базы данных и определены всякие полезные функции.

### `Main.hs`

Собственно структура и страницы сайта.

## Документация по `happstack`

[Документация](https://www.happstack.com/page/view-page-slug/3/documentation)

[Описание](https://www.happstack.com/docs/crashcourse/index.html)

[Страничка про упрощённую версию](https://www.happstack.com/page/view-page-slug/9/happstack-lite)

[Документация по упрощённой версии](https://hackage.haskell.org/package/happstack-lite-7.3.8/docs/Happstack-Lite.html)

## Как что-то делать

### Хочу новый тип данных

Создать его в `Types.hs` и определить `deriving (Show, Read)`

### Хочу новую БД

Добавить в `DBs` новое поле `_<dbName> :: DB <DbKey> <DbValue>`.

Добавить в `runApp` инициализацию БД `<*> initDB "<dbName>"`.

Добавить
```
instance MonadDB <DbKey> <DbValue> App where
    stateDB = appStateDB <dbName>
```

Далее с этой БД можно выполнять операции `query`, `update`, `delete`, иногда `insert`.
Возможно придётся добавить аннотации `@<DbKey> @<DbValue>` сразу после имени функции.

### Хочу новую страницу

Определить функцию с типом `App Response`, возвращающую страницу.

Добавить её путь в `main` через `dir "a"` если одно слово или через `dirs "a/b/c"` если несколько.

Для динамических частей адреса (например, id пользователя или поста) можно использовать [`path`](https://hackage.haskell.org/package/happstack-server-7.8.0.2/docs/Happstack-Server-Routing.html#v:path).

Для аргументов запроса можно использовать [`lookText'`](https://hackage.haskell.org/package/happstack-server-7.8.0.2/docs/Happstack-Server-RqData.html#v:lookText-39-).

Для фрагмента html можно использовать `page`.

Для формы ввода данных можно использовать `form`.

Для доступа к текущему пользователю `currentUser`.

Для проверки того, что пользователь залогинен и его получения `withUser`.
