# Development Workflow

There are feature branches, task branches and the `master` branch.

Features are grouped into tasks (sprints).

At the end of each sprint, the task will be reviewed by the [dungeon master](https://github.com/Kakadu) and rebased into the `master` (to ensure linear history).

`some-feature`, `other-feature`, `important-bug-fix` --> `task-<N>` --> `master`

1 feature/dedicated bug-fix/etc. = 1 PR into `task-<N>` = 1 commit in `task-<N>` = 1 commit in `master` branch (PRs must be squashed)

1 `task-<N>` = 1 PR into `master` = several commits in the `master` branch (PRs must be rebased)
