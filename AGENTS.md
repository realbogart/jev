# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

## Project Overview

This is a Haskell project built with Nix flakes for reproducible development environments and haskell.nix for building. The project follows strict code quality standards with comprehensive warning flags and formatting requirements.

## Development Workflow

**ALWAYS run these commands after making changes to Haskell source:**
```bash
./scripts/format.sh
./scripts/verify.sh
```

These scripts ensure code formatting and successful builds. Never skip this step after making changes.

## Development Commands

**Build with cabal (in nix shell):**
```bash
cabal build
cabal run jev
```

## Code Quality Standards

**Warnings:**
- ALWAYS fix all compiler warnings - never ignore them
- Disable warnings in the cabal file's `common warnings` section, NEVER in source files
- The project uses `-Weverything` with specific exclusions for practical development

## Architecture

**Project Structure:**
- `src/Main.hs` - Executable entry point that delegates to library
- `lib/` - Library modules with core functionality
- `scripts/` - Development automation scripts
- `flake.nix` - Nix flake defining the development environment and build

**Development Environment:**
- GHC 9.10.1 via haskell.nix
- Includes hlint, HLS, ormolu, and ghcid in development shell
- Comprehensive warning flags for strict code quality
- Uses GHC2021 language standard

## Important Reminders

1. **Always run format and verify scripts after changes**
2. **Fix all warnings - never disable them in source code**
3. **Use the cabal file to understand enabled language extensions**
4. **NEVER make git commits - leave that to the user**
