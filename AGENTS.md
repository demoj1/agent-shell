# Agent Shell - Project Guidelines

## Communication norms

PR and issue conversations are human relationships. The maintainer prefers
talking directly to humans.

When contributing:

- Write your own PR descriptions and issue comments. Don't have AI generate them.
- If you used AI to research something, summarize the findings in your own words
  and give your level of endorsement rather than pasting AI output verbatim.
  Concise, human-written summaries save the maintainer from having to parse
  lengthy generated text.
- Review all code in your PR yourself and vouch for its quality.

## Contributing

This is an Emacs Lisp project. See [CONTRIBUTING.org](CONTRIBUTING.org) for style guidelines, code checks, and testing. Please adhere to these guidelines.

## Форк demoj1

Личный форк (`~/:pet/language/elisp/agent-shell`), Emacs грузит `agent-shell` отсюда через `:load-path`. Remotes: `origin` — `demoj1/agent-shell`, `upstream` — `xenodium/agent-shell`.

Что поддерживается в форке поверх апстрима:
- `agent-shell-turn-time.el` — время и длительность ходов на бейджах чата.
- `agent-shell-claude-status.el` — `post_turn_summary` оверлеем под ответом (без дубля в конце ответа), `task_summary` справа на метке, `bridge_state`/rate-limit в header, `prompt_suggestion` (Tab), колонка ibuffer `agent-status`, фильтр Notices (`agent-shell-claude-status-ignored-stderr-regexps`), `agent-shell-reload-idle`.
- `agent-shell-easysession.el` (+ `tests/agent-shell-easysession-tests.el`) и `agent-shell-unload` — ленивое восстановление сессий через EasySession и выгрузка без потери истории.
- Точечные правки в `agent-shell.el`, `agent-shell-anthropic.el`, `agent-shell-chat-mode.el`: короткий session ID (8 символов) с копированием кликом, подхват свежего конфига по `:identifier` при `agent-shell-restart`, синк `shell-maker--buffer-name-override` при переименовании буфера, проброс уведомлений и фреймов статуса.

Правила ведения форка и синка:
- Свои правки — коммитами в `main` форка, каждая отдельным коммитом с понятным заголовком, чтобы их было видно в `git log upstream/main..main`.
- Апстрим подтягивать периодически (раз в 1–2 недели или по запросу): `git fetch upstream && git merge upstream/main`. Merge, не rebase: ребейз переписывает уже запушенный `main` форка.
- При конфликтах сохранять обе стороны: поведение апстрима плюс наши правки поверх. После мержа проверить, что наши правки живы (`git log upstream/main..main`), и прогнать проверки/сборку по [CONTRIBUTING.org](CONTRIBUTING.org).
- **Обязательно обновлять форк на GitHub:** после синка или новых правок делать `git push origin main` (PR и issue в `upstream` `xenodium/agent-shell` — только по явному «го» пользователя и его текстом, см. Communication norms выше).
