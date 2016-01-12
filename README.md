# Chokecherry

Обертка над **lager**, которая не зависимо от вида **backend** осуществляет ограничение количества выводимых сообщений.

Вызовы **chokecherry:info/1**, **chokecherry:info/2** транслируются в **lager:info/1**, **lager:info/2** соответсвенно.

Добавление в лог название модуля откуда произошел вызов и Pid вызывающего процесса производится следующим образом:

1) указываем непосредсвенно непосредственно в файле, в котором используем:

```
-compile([{parse_transform, chokecherry_transform}]).
```

**Внимание**: данная строчка должна предшествовать

```
-compile([{parse_transform, lager_transform}]).
```

2) указываем в главном rebar.config проекта:

```
{erl_opts, [
    % ...
    {parse_transform, chokecherry_transform},
    {parse_transform, lager_transform}
    % ...
}.
```

**Внимание**: *chokecherry_transform* должен предшествовать *lager_transform*.
