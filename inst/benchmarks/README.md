# Coordinate-exchange architecture and benchmarks

The coordinate-exchange implementation keeps formula handling in R and moves
constraint evaluation, feasible-row generation, and D-optimal search into C++.
The internal interfaces are versioned, but they are not public package APIs.

## Constraint compilation and IR v1

```mermaid
flowchart LR
  E[Filter expression] --> P[Validated parser]
  P --> A[Boolean AST]
  A --> N[Negation normal form]
  N --> D[Bounded DNF expansion]
  D --> F[Deduplicated atoms]
  F --> I[Flat IR v1]
  T[Forbidden tuple tables] --> I
  L[Original factor levels] --> I
  I --> V[R schema validation]
  V --> C[C++ defensive validation]
```

Direct factor predicates use level codes. Numeric values are consulted only by
linear atoms, through an original-unit level table indexed by those same codes.
This separates search normalization from constraint semantics.

```mermaid
classDiagram
  class ConstraintIRv2 {
    version = 1
    q
    comparison_tol
    factor_kind[]
    L[]
    level_ptr[]
    level_value[]
    clause_ptr[]
    clause_atom[]
    atom_type[]
    atom_payload_idx[]
  }
  class ComparisonPayload {
    cmp_var[]
    cmp_op[]
    cmp_code[]
  }
  class MembershipPayload {
    in_var[]
    in_neg[]
    in_ptr[]
    in_code[]
  }
  class LinearPayload {
    lin_op[]
    lin_rhs[]
    lin_const[]
    lin_ptr[]
    lin_idx[]
    lin_coef[]
  }
  class ForbiddenPayload {
    forb_ptr[]
    forb_idx[]
    forb_code[]
    forbidden_tables[]
  }
  ConstraintIRv2 --> ComparisonPayload
  ConstraintIRv2 --> MembershipPayload
  ConstraintIRv2 --> LinearPayload
  ConstraintIRv2 --> ForbiddenPayload
```

An empty clause is `TRUE`; zero clauses is `FALSE`. Forbidden tables are
conjoined with the DNF result. All indices and pointers are zero-based inside
the IR.

## Feasible designs and coordinate proposals

```mermaid
flowchart TD
  S[Initial level codes] --> Q{Feasible and full rank?}
  Q -- yes --> X[Initial model matrix]
  Q -- no --> B[Constraint-aware backtracking]
  B --> P[Partial DNF and linear bound pruning]
  P --> T[Forbidden-table prefix pruning]
  T --> G[Feasible row pool]
  G --> R[Greedy rank construction]
  R --> X
  X --> U[IR support edges]
  U --> C[Union-find coordinate groups]
  C --> E[Exact enumeration within cap]
  C --> A[Seeded bounded traversal above cap]
  E --> M[Batched model.matrix call]
  A --> M
```

The partial evaluator is conservative: it may retain a branch that later turns
out to be infeasible, but it never removes a feasible completion. The final
row evaluator is therefore the sole authority at each leaf.

## One coordinate-exchange sweep

```mermaid
sequenceDiagram
  participant CE as CE engine
  participant IR as ConstraintSet
  participant R as model.matrix
  participant LA as Eigen
  CE->>CE: Select mutable rows by leverage
  loop selected row
    CE->>IR: Generate feasible proposals for every group
    IR-->>CE: Exact or bounded code rows
    CE->>R: One batched proposal matrix
    R-->>CE: Full model rows
    CE->>CE: Rank proposals by determinant delta
    CE->>LA: Factor exact trial information matrix
    LA-->>CE: logdet, rank, reciprocal condition
    CE->>CE: Accept only a finite strict improvement
  end
  CE->>CE: Stop on no move, relative D-efficiency gain, or max iterations
  CE-->>R: Points, codes, model matrix, and diagnostics
```

The stopping statistic is
`expm1((logdet_new - logdet_old) / p)`, the relative change in D-efficiency for
one sweep. The returned diagnostics retain the accepted objective history and
the agreement between maintained and independently recomputed log determinants.

## Benchmark workflow

```mermaid
flowchart LR
  S[Versioned scenario table] --> P[Public track]
  S --> K[Kernel track]
  P --> PG[Complete gen_design calls]
  K --> KP[Shared compilation and feasible start]
  KP --> PE[Point engine]
  KP --> CE[Coordinate engine]
  PG --> R[Raw result rows including failures]
  PE --> R
  CE --> R
  R --> D[Paired runtime and D-efficiency rows]
  D --> M[Median, IQR, bootstrap and Wilson intervals]
  M --> O[Smoke artifacts or release output]
```

Run the deterministic smoke profile from the package root:

```sh
Rscript inst/benchmarks/run_coordinate_exchange_benchmarks.R \
  --track=all --profile=smoke --output-dir=inst/benchmarks/results
```

Run the full release profile into a non-versioned directory:

```sh
Rscript inst/benchmarks/run_coordinate_exchange_benchmarks.R \
  --track=all --profile=full --output-dir=benchmark-release
```

Both profiles write raw, paired, summary, and provenance files with an explicit
schema version. Every method failure remains a raw and paired result; analysis
never silently drops a failed scenario. The suite is descriptive and makes no
powered inferential claim.
