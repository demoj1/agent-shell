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

Личный форк, Emacs грузит agent-shell отсюда. Remotes: `origin` — `demoj1/agent-shell`, `upstream` — `xenodium/agent-shell`.

- Свои правки — коммитами в `main` форка, каждая отдельным коммитом с понятным заголовком, чтобы их было видно в `git log upstream/main..main`.
- Апстрим подтягивать иногда (раз в неделю-две или когда юзер попросит): `git fetch upstream && git merge upstream/main`. Merge, не rebase: ребейз переписывает уже запушенный `main` форка.
- При конфликтах сохранять обе стороны: поведение апстрима плюс наши правки поверх. После мержа проверить, что наши правки живы (`git log upstream/main..main`), и собрать: `make` / byte-compile по CONTRIBUTING.org.
- Push в `origin` и PR в апстрим — только по явному «го» юзера. Текст PR и issue для xenodium пишет сам юзер (см. Communication norms выше).
