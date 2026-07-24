# Installation and Setup

This guide covers how to install bbt on various platforms.

---

## Prerequisites

### Required: Alire (Ada Package Manager)

Most users should install bbt via [Alire](https://alire.ada.dev/), the Ada package manager.

> **Note for macOS (Darwin):** On older versions, set `GNAT_FILE_NAME_CASE_SENSITIVE=1` to avoid case sensitivity issues.

---

## Installation Methods

### Recommended: Install via Alire

The simplest and recommended method:

```bash
# Install Alire first (if not already installed)
curl -fsSL https://alire.ada.dev/download | bash

# Add Alire to your PATH (if not automatically added)
export PATH="$HOME/.alire/bin:$PATH"

# Install bbt
alr install bbt

# Verify installation
bbt --version
```

---

### Alternative: AppImage for Linux

For Linux systems without Alire:

1. Download the latest AppImage from the [bbt GitHub releases](https://github.com/LionelDraghi/bbt/releases)
2. Make it executable:
   ```bash
   chmod +x bbt-*.AppImage
   ```
3. Run it:
   ```bash
   ./bbt-*.AppImage --version
   ```
4. (Optional) Move to a permanent location and create a symlink:
   ```bash
   mv bbt-*.AppImage /usr/local/bin/bbt
   chmod +x /usr/local/bin/bbt
   ```

---

### Compiling from Source

For advanced users or when other methods are not available:

```bash
# Clone the repository
git clone https://github.com/LionelDraghi/bbt.git
cd bbt

# Build using Alire
alr build

# Or build manually
make build

# Install (may require sudo)
make install

# Verify
bbt --version
```

For detailed instructions, see the [bbt GitHub repository](https://github.com/LionelDraghi/bbt#installation).

---

## Typical Project Structure

After installation, you might organize your project like this:

```
my_project/
├── README.md                    # Basic examples and documentation
├── docs/
│   └── scenarios.md             # Documentation with embedded tests
└── tests/
    ├── features/                # Functional tests
    │   ├── auth.md
    │   ├── processing.md
    │   └── errors.md
    └── regression.md             # Regression tests
```

---

## Verifying Installation

After installation, verify bbt works correctly:

```bash
# Check version
bbt --version

# Display help
bbt help

# Generate an example scenario
bbt help example > example_test.md

# Run the example
bbt example_test.md
```

---

## Troubleshooting Installation

### Alire Not Found

**Error:** `alr: command not found`

**Solution:**
```bash
# Install Alire
curl -fsSL https://alire.ada.dev/download | bash

# Add to PATH
export PATH="$HOME/.alire/bin:$PATH"

# For permanent addition, add to your shell config
# ~/.bashrc, ~/.zshrc, or ~/.bash_profile
echo 'export PATH="$HOME/.alire/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### macOS Case Sensitivity Issues

**Error:** File not found or case-related issues

**Solution:**
```bash
# Set environment variable for case-sensitive filesystem
export GNAT_FILE_NAME_CASE_SENSITIVE=1

# Add to your shell config for permanence
echo 'export GNAT_FILE_NAME_CASE_SENSITIVE=1' >> ~/.zshrc
```

### GNAT Not Installed

**Error:** `gnat` or `gcc` not found

**Solution:** Install GNAT (GNU Ada compiler):

- **Ubuntu/Debian:** `sudo apt-get install gnat`
- **Fedora:** `sudo dnf install gcc-gnat`
- **macOS (Homebrew):** `brew install gnat`
- **Windows:** Download from [libre.adacore.com](https://libre.adacore.com/)

---

## Environment Variables

| **Variable** | **Purpose** | **Example Value** |
|--------------|-------------|-------------------|
| `GNAT_FILE_NAME_CASE_SENSITIVE` | Fix case sensitivity on macOS | `1` |
| `ALIRE_ROOT` | Custom Alire installation directory | `/opt/alire` |
| `PATH` | Include Alire and GNAT in PATH | `$HOME/.alire/bin:$PATH` |

---

## Next Steps

After installation, proceed to:
- **Writing tests:** See [author/syntax.md](../author/syntax.md) for syntax
- **Running tests:** See [commands.md](./commands.md) for CLI usage
- **Debugging:** See [debugging/](./debugging/) for troubleshooting
