# Eval Corpus

This folder gives lightweight evidence for adoption: trigger cases, anti-trigger cases, and expected output properties.

Use `trigger-cases.jsonl` with any agent harness that reports selected skills and final answers, or run it manually by asking each prompt in a fresh agent session.

A passing run should show:

1. the expected skill is selected for positive cases;
2. no skill, or the listed alternate skill, is selected for anti-trigger cases;
3. the answer includes the listed quality properties;
4. the answer avoids the listed failure modes.

These are not benchmark theater. They are boundary tests for whether the skill fires at the right architectural seam.
