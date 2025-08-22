#!/bin/bash
# scripts/apply_iwyu_fixes.sh - Script to automatically apply IWYU suggestions

set -e

TARGET_NAME=${1:-"my_app"}
BUILD_DIR=${2:-"build"}
SOURCE_DIR=${3:-"."}

echo "Applying IWYU fixes for target: $TARGET_NAME"

# Function to apply IWYU suggestions using fix_includes.py
apply_iwyu_fixes() {
  local build_log="$BUILD_DIR/iwyu_output.log"
  local fix_script="fix_includes.py"

  echo "Step 1: Building with IWYU to generate suggestions..."

  # Build and capture IWYU output
  cmake --build "$BUILD_DIR" --target "$TARGET_NAME" 2>&1 | tee "$build_log" || true

  echo "Step 2: Checking if fix_includes.py is available..."

  # Check if fix_includes.py is available
  if ! command -v fix_includes.py &>/dev/null; then
    echo "fix_includes.py not found. Trying to locate it..."

    # Common locations for fix_includes.py
    possible_paths=(
      "/usr/bin/iwyu-tool"
    )

    fix_script=""
    for path in "${possible_paths[@]}"; do
      if [[ -f $path ]]; then
        fix_script="$path"
        break
      fi
    done

    if [[ -z "$fix_script" ]]; then
      echo "Error: fix_includes.py not found. Please install it or provide the path."
      echo "You can get it from: https://github.com/include-what-you-use/include-what-you-use/blob/master/fix_includes.py"
      exit 1
    fi
  else
    fix_script="fix_includes.py"
  fi

  echo "Step 3: Applying fixes using $fix_script..."

  # Apply the fixes
  python3 "$fix_script" --nosafe_headers <"$build_log"

  echo "Step 4: Done! IWYU fixes have been applied."
}

# Function to run IWYU analysis only (no fixes)
run_iwyu_analysis() {
  echo "Running IWYU analysis only..."

  cmake --build "$BUILD_DIR" --target "$TARGET_NAME" 2>&1 |
    tee "$BUILD_DIR/iwyu_analysis.log" |
    grep -E "(should add these lines|should remove these lines|The full include-list|---)"
}

# Function to create a backup before applying fixes
create_backup() {
  local backup_dir="$BUILD_DIR/iwyu_backup_$(date +%Y%m%d_%H%M%S)"
  echo "Creating backup in $backup_dir..."

  mkdir -p "$backup_dir"

  # Find and copy all source files
  find "$SOURCE_DIR" -name "*.cpp" -o -name "*.hpp" -o -name "*.h" -o -name "*.cc" -o -name "*.cxx" |
    while read -r file; do
      cp "$file" "$backup_dir/"
    done

  echo "Backup created in $backup_dir"
}

# Function to show IWYU suggestions without applying them
show_suggestions() {
  echo "Showing IWYU suggestions..."

  cmake --build "$BUILD_DIR" --target "$TARGET_NAME" 2>&1 |
    python3 -c "
import sys
import re

output = sys.stdin.read()
suggestions = re.findall(r'(.+?) should add these lines:(.*?)(?=\n\n|\n.*? should|\Z)', output, re.DOTALL)

for file_path, lines in suggestions:
    print(f'📁 {file_path.strip()}')
    print('  ➕ Should ADD:')
    for line in lines.strip().split('\n'):
        if line.strip():
            print(f'    {line.strip()}')
    print()

remove_suggestions = re.findall(r'(.+?) should remove these lines:(.*?)(?=\n\n|\n.*? should|\Z)', output, re.DOTALL)

for file_path, lines in remove_suggestions:
    print(f'📁 {file_path.strip()}')
    print('  ➖ Should REMOVE:')
    for line in lines.strip().split('\n'):
        if line.strip() and not line.strip().startswith('//'):
            print(f'    {line.strip()}')
    print()
"
}

# Main script logic
case "${1:-help}" in
"apply")
  create_backup
  apply_iwyu_fixes
  ;;
"check")
  run_iwyu_analysis
  ;;
"suggest")
  show_suggestions
  ;;
"backup")
  create_backup
  ;;
*)
  echo "Usage: $0 {apply|check|suggest|backup} [target_name] [build_dir] [source_dir]"
  echo ""
  echo "Commands:"
  echo "  apply   - Create backup and apply IWYU fixes automatically"
  echo "  check   - Run IWYU analysis and show results"
  echo "  suggest - Show IWYU suggestions in a readable format"
  echo "  backup  - Create backup of source files"
  echo ""
  echo "Examples:"
  echo "  $0 apply my_app build ."
  echo "  $0 suggest"
  echo "  $0 check"
  ;;
esac
