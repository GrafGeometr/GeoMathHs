# GeoMath

## Установка `IHP`

[Руководство](https://ihp.digitallyinduced.com/Guide/installation.html)

> Если вы работаете под windows, то все команды надо выполнять в `wsl`

---

[Настройка vscode](https://ihp.digitallyinduced.com/Guide/editors.html#using-ihp-with-visual-studio-code-vscode)

Возможно, расширение haskell будет просить установки Haskell Languahe Server, проще всего это сделать так:

1. Установить `ghcup`:
```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
2. Удалить ненужные компоненты:
```
ghcup tui
```
а далее надо оставить только `hls`.
Это важно, т.к. у `IHP` свои версии `ghc` и `cabal`

## Запуск

```
./start
```

После этого сайт доступен локально под http://localhost:8000/, а инструментарий `IHP` под http://localhost:8001/

[Документация по `IHP`](https://ihp.digitallyinduced.com/Guide/index.html)

## О структуре проекта на `IHP`

Есть следующие основные сущности:

- Приложения: у нас это `Web/`, потом будет ещё `Admin/`

    Это и есть наш сайт

- Типы данных (`Web/Types.hs`)

    Тут расположены определения нужных структур данных

- [View](https://ihp.digitallyinduced.com/Guide/view.html) (`Web/View/.../...`)

    Это то, что отображается на сайте, по сути одна `html` страница

- [Controller](https://ihp.digitallyinduced.com/Guide/controller.html) (`Web/Controller/...`)

    Это то, что обрабатывает входящие `URL`ы, делает запросы к БД и возвращает страницу, которую надо рендерить
