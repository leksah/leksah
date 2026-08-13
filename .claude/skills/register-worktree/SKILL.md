---
name: register-worktree
description: Register a git worktree relationship with the Leksah IDE. Use IMMEDIATELY after running `git worktree add` or `git worktree remove`, after `git switch`/`git checkout -b` inside a worktree, or when you start working in / reviewing / are finished with a worktree someone else created. Keeps the IDE's map of worktrees, branches and sessions accurate.
---

# Register a worktree relationship with Leksah

Leksah (the IDE this project is developed in) keeps a registry of git
worktrees: which Claude session created each one, who is working in it, and a
log of branch moves. Sessions report their own relationships; unreported
worktrees show up as orphans the user has to puzzle over.

## When

- You ran `git worktree add …` → register the new path with `--role created`.
- You ran `git worktree remove …`, or are otherwise done with a worktree →
  `--role abandoned`.
- You started doing work inside an existing worktree → `--role working`.
- You are reading/reviewing someone else's worktree → `--role reviewing`.
- You changed the branch a worktree is on (`git switch`, `git checkout -b`) →
  register again with `--branch <new branch>`.

## How

If the `register_worktree` MCP tool is available, call it (arguments:
`worktree`, `role`, `branch`, `note` — all optional; `worktree` defaults to
your cwd, and you are identified automatically). Otherwise run:

```
leksah-cmd agent register --worktree PATH \
  --role created|working|reviewing|abandoned \
  --note '<one line: what your relationship with this worktree is>' \
  [--branch BRANCH]
```

Leksah-launched sessions are pre-approved for this command — it never stops
you on a permission prompt. If it replies that leksah is not running, or the
path is not a linked worktree, move on; do not retry or ask.

Keep the `--note` to one specific line ("bumping nixpkgs for PR #2564", not
"working on stuff") — it is what the user reads in the tree.
