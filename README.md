# Valerie Core Guide

`valerie-core` is the verified compiler backend and formal semantics library for Valerie. It contains the F\* and Pulse formal specifications, operational machine semantics for all supported targets, and the OCaml frontend lexer/parser that compiles `.vaf` files.

---

## Quick Start

### 1. Building the Native Backend Compiler
From either the repository root or inside `valerie-core/`:
```bash
valerie compiler
```
This builds the lexer/parser, extracts the verified AST transformer from F\* to OCaml, and compiles the native executable to `dist/valerie-compiler`.

To install it into your `PATH` (`~/.local/bin/valy`, `~/.local/bin/valerie-compiler`, and `~/.local/bin/vale`):
```bash
valerie vale --install
```

### 2. Verifying the Specifications
You can verify the formal proofs directly using Valerie:
```bash
# Verify the entire Pulse separation logic suite
valerie verify --pulse

# Verify a specific machine specification
valerie verify src/spec/Valerie.AArch64.Machine.fst

# Verify the Pulse separation logic memory model
valerie verify src/pulse/Valerie.Pulse.Memory.fst
```

### 3. Emitting Assembly, Pulse, or F\*
Use `valerie emit` or the compiled `valy` directly:
```bash
# Emit verified Pulse separation logic module
valerie emit tests/fixtures/vale/chacha20_quarter.vaf --pulse chacha.fst

# Emit target assembly for AArch64
valerie emit tests/fixtures/vale/chacha20_quarter.vaf -o chacha.s --target aarch64-linux

# Or invoke the backend compiler directly
valy -in tests/fixtures/vale/chacha20_quarter.vaf -pulse chacha.fst -arch aarch64
```

---

## Architecture & Codebase Map

The codebase is organized into four main layers:

```
valerie-core/
├── src/
│   ├── parser/      # Native lexer & parser (OCaml)
│   ├── compiler/    # Verified AST, validation, lowering, and emitters (F*)
│   ├── spec/        # Target machine states, instruction semantics & WP calculi (F*)
│   ├── pulse/       # Separation logic machine & memory model (#lang-pulse)
│   └── proof/       # Mathematical lemmas & calling convention proofs (F*)
└── valerie.toml     # Declarative task & verification manifest
```

### 1. Pulse Separation Logic (`src/pulse/`)
Replaces legacy Vale's $O(N^2)$ disjointness checks with linear separation logic (`#lang-pulse`):
- [`Valerie.Pulse.Machine.fst`](src/pulse/Valerie.Pulse.Machine.fst): Machine register banks modeled as linear arrays (`reg_array = array U64.t`). Instruction steps prove state transitions while framing disjoint registers.
- [`Valerie.Pulse.Memory.fst`](src/pulse/Valerie.Pulse.Memory.fst): Memory buffers with typed byte, word32, and word64 views, using separating conjunction (`**`) for spatial framing.

### 2. Target Machine Specifications (`src/spec/`)
Each supported target has a complete formal machine state, instruction semantics, instruction lemmas, and Weakest Precondition (WP) calculi:
- **AArch64**: [`Valerie.AArch64.Machine.fst`](src/spec/Valerie.AArch64.Machine.fst), [`Valerie.AArch64.Semantics.fst`](src/spec/Valerie.AArch64.Semantics.fst), [`Valerie.AArch64.WP.fst`](src/spec/Valerie.AArch64.WP.fst)
- **x86_64**: [`Valerie.X64.Machine.fst`](src/spec/Valerie.X64.Machine.fst), [`Valerie.X64.Semantics.fst`](src/spec/Valerie.X64.Semantics.fst), [`Valerie.X64.WP.fst`](src/spec/Valerie.X64.WP.fst)
- **Arm32**: [`Valerie.Arm32.Machine.fst`](src/spec/Valerie.Arm32.Machine.fst), [`Valerie.Arm32.Semantics.fst`](src/spec/Valerie.Arm32.Semantics.fst), [`Valerie.Arm32.WP.fst`](src/spec/Valerie.Arm32.WP.fst)
- **RISC-V 64**: [`Valerie.Riscv64.Machine.fst`](src/spec/Valerie.Riscv64.Machine.fst), [`Valerie.Riscv64.Semantics.fst`](src/spec/Valerie.Riscv64.Semantics.fst), [`Valerie.Riscv64.WP.fst`](src/spec/Valerie.Riscv64.WP.fst)

### 3. Compiler Pipeline (`src/compiler/` & `src/parser/`)
The translation from source `.vaf` to verified artifacts:
1. **Parsing**: `src/parser/lex.mll` and `src/parser/parse.mly` parse Vale source text into an OCaml AST.
2. **Desugaring & Validation**: [`Valerie.Compiler.Desugar.fst`](src/compiler/Valerie.Compiler.Desugar.fst) and [`Valerie.Compiler.Validate.fst`](src/compiler/Valerie.Compiler.Validate.fst) transform and check type/register rules.
3. **Lowering**: Target-specific passes in `Valerie.Compiler.Lower.*.fst` convert high-level statements into concrete machine instructions.
4. **Emission**: [`Valerie.Compiler.EmitPulse.fst`](src/compiler/Valerie.Compiler.EmitPulse.fst), [`Valerie.Compiler.EmitFStar.fst`](src/compiler/Valerie.Compiler.EmitFStar.fst), and [`Valerie.Compiler.Print.fst`](src/compiler/Valerie.Compiler.Print.fst) output verified Pulse, F\*, or textual assembly.

### 4. Proof Core (`src/proof/`)
- [`Valerie.Proof.Core.fst`](src/proof/Valerie.Proof.Core.fst): Algebraic lemmas for XOR involution, arithmetic rings, and bitwise updates.
- [`Valerie.Proof.ABI.*.fst`](src/proof/): Formal proofs that procedures preserve callee-saved registers under AAPCS64, System V, AAPCS32, and lp64d.

---

## Working with `valerie.toml`

The build and verification rules are declared in [`valerie.toml`](valerie.toml):
- `[verification]`: Specifies cache directories (`.cache`), hints, and include paths across `src/spec`, `src/pulse`, `src/compiler`, and `src/proof`.
- `[tasks.compiler]`: Compiles and links the native OCaml binary using `ocamlopt` and `valerie_parser.cmxa`.
- `[tasks.pulse-verify]`: Runs Pulse verification with `-warn-error` flags.
