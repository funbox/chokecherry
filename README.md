# Chokecherry

Обертка над **lager**, которая независимо от вида **backend** осуществляет ограничение количества выводимых сообщений.

Вызовы **chokecherry:info/2**, **chokecherry:warning/2** транслируются в **lager:info/2**, **lager:warning/2** соответсвенно.

По умолчанию включенно ограничение вывода сообщений только уровня **info**, не более 50 сообщений в секунду.

Пример конфигурации:

```
   {chokecherry, [
       {info, [{mps_limit, 50}]},
       {warning, [{mps_limit, undefined}]}
   ]}

```
это эвивалентно:

```
   {chokecherry, [
       {info, [{mps_limit, 50}]}
   ]}

```


