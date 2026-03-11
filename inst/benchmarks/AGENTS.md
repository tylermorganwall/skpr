## Statistical Analysis Guardrails (R)

- For inferential modeling in R, use built-in modeling functions (`lm`, `glm`, `aov`, etc.) unless explicitly told otherwise.
- Power analysis must match the final analysis model/test exactly:
  - same formula/terms
  - same hypothesis test (e.g., method coefficient in `lm`)
  - same alpha
- Do not use surrogate or convenience power tools if they imply a different analysis model.
- Before long experiment runs, state: planned final model, planned test, and the exact power method mapping.
