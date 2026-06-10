# 🐧 What are Linux Commands?

> Linux commands allows you to interact with your operating system through a terminal using text-based instructions.

---

## 🌐 Resources

| Type | Link |
|------|------|
| General Overview | [What is Linux](https://www.linux.com/what-is-linux/) · [Linux is Not Windows](https://linux.oneandoneis2.org/LNW.htm) |
| Community Forum | [Linux.org — Getting Started](https://www.linux.org/forums/getting-started.148/) |
| Tutorials | [GeeksforGeeks Linux Guide](https://www.geeksforgeeks.org/linux-unix/linux-tutorial/) |
| Exercises | [Navigating Files & Directories](https://arcca.github.io/An-Introduction-to-Linux-with-Command-Line/02-filedir/index.html) |

---

## 🗂️ Key Concepts

- The filesystem manages how information is stored on disk.
- Information lives in **files**, which are grouped into **directories** (folders).
- Directories form a **directory tree** 🌳 — a hierarchy with the **root** (`/`) at the top.
- **Relative path** — specifies a location from your *current* directory.
- **Absolute path** — specifies a location from the *root* directory.

> 💡 The shell sorts files in **alphabetical (lexicographic) order**, so name your files accordingly before organising them.

---

## ⌨️ Essential Commands

### Navigation

| Command | Description |
|---------|-------------|
| `/` | Root directory — the top of the entire filesystem |
| `pwd` | **P**rint **W**orking **D**irectory — shows where you currently are |
| `ls` | Lists the contents of the current directory |
| `ls -F` | Classifies items (e.g. adds `/` after directory names) |
| `ls -lh` | Long list with human-readable file sizes |
| `cd <directory>` | Move into a directory |
| `cd ..` | Go back up to the parent directory |
| `cd -` | Jump back to the previous directory you were in |

> 💡 `ls` options like `-h`, `-r`, `-l` can be combined (e.g. `ls -lh`). Options are **case-sensitive**.

> 💡 Use `help` or `man ls` to read the manual for any command.

---

### Shortcuts

| Shortcut | Description |
|----------|-------------|
| `~` | Shorthand for your home directory |
| `Tab` | Autocompletes directory and file names |
| `↑` / `↓` | Cycle through previously used commands |

---

## 🔢 Example Sequence

The commands below can also be merged into one using `/` as a separator.

```bash
pwd               # Check where you are
ls -F             # See what's in the current directory
cd Desktop        # Move to Desktop
cd data-shell     # Move into data-shell
cd data           # Move into data
```

---

## 📕 Summary

These notes cover the basics of navigating a Linux filesystem using simple bash commands — moving between directories, listing contents, and using shortcuts to work more efficiently. This page serves as a personal reference I can return to as I continue learning.
