# Codex Architecture Skills

Three portable Agent Skill packages for typed functional architecture work.

## Skills

| Skill | Use when |
| --- | --- |
| [`hkd-indexed-data`](./hkd-indexed-data) | Designing higher-kinded records, carrier-parametric schemas, witness-indexed products, sparse-to-total authoring, and typed projections. |
| [`structured-registry-designer`](./structured-registry-designer) | Designing registries as lawful indexed structures: total registries, functors, presheaves, sheaves, fibred registries, and enriched registries. |
| [`fixed-axis-dense-blocks`](./fixed-axis-dense-blocks) | Designing dense numeric storage for small fixed homogeneous axis families behind typed public APIs and hot-loop kernels. |

## Install

Copy the skill folders you want into your agent's skills directory.

For Codex:

```sh
cp -R hkd-indexed-data structured-registry-designer fixed-axis-dense-blocks ~/.codex/skills/
```

Each folder is self-contained. If you copy only `hkd-indexed-data`, it still includes the references and code examples it needs.

## Validate

With Codex's skill validator available:

```sh
python3 ~/.codex/skills/.system/skill-creator/scripts/quick_validate.py ./hkd-indexed-data
python3 ~/.codex/skills/.system/skill-creator/scripts/quick_validate.py ./structured-registry-designer
python3 ~/.codex/skills/.system/skill-creator/scripts/quick_validate.py ./fixed-axis-dense-blocks
```

## License

MIT. See [`LICENSE`](./LICENSE).
