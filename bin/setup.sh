#!/bin/bash

# Dotfiles setup script using bare git repository strategy
# Based on: https://www.atlassian.com/git/tutorials/dotfiles

set -e  # Exit on any error

# Configuration
REPO_URL="git@github.com:xiaoxinghu/config.git"
CONFIG_DIR="$HOME/.cfg"
BACKUP_DIR="$HOME/.config-backup"
CONFIG_ALIAS="config"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            echo "Usage: $0"
            echo "  -h, --help: Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to prompt user for yes/no (for special directories)
prompt_user() {
    local message="$1"
    local response

    while true; do
        echo -n -e "${YELLOW}$message${NC} y/n: "
        read -r response
        case $response in
            [Yy]|[Yy][Ee][Ss]) return 0 ;;
            [Nn]|[Nn][Oo]) return 1 ;;
            *) echo "Please answer yes with y or no with n." ;;
        esac
    done
}
setup_config_alias() {
    alias $CONFIG_ALIAS="/usr/bin/git --git-dir=$CONFIG_DIR/ --work-tree=$HOME"

    # Make alias available as a function for the script
    config() {
        /usr/bin/git --git-dir="$CONFIG_DIR/" --work-tree="$HOME" "$@"
    }
}

# Function to check if git is installed
check_git() {
    if ! command -v git &> /dev/null; then
        log_error "Git is not installed. Please install git first."
        exit 1
    fi
}

# Function to clone the bare repository
clone_repo() {
    log_info "Cloning dotfiles repository..."

    if [ -d "$CONFIG_DIR" ]; then
        log_warning "Directory $CONFIG_DIR already exists."

        if prompt_user "Do you want to remove it and re-clone?"; then
            rm -rf "$CONFIG_DIR"
        else
            log_error "Aborting setup."
            exit 1
        fi
    fi

    if git clone --bare "$REPO_URL" "$CONFIG_DIR"; then
        log_success "Repository cloned successfully to $CONFIG_DIR"
    else
        log_error "Failed to clone repository. Please check your SSH keys and repository URL."
        exit 1
    fi
}

# Function to backup conflicting files
backup_conflicting_files() {
    log_info "Checking for conflicting files..."

    # Create backup directory if it doesn't exist
    mkdir -p "$BACKUP_DIR"

    # Find conflicting files by checking which repo files already exist
    local conflicting_files=""
    while IFS= read -r file; do
        if [ -n "$file" ] && [ -e "$HOME/$file" ]; then
            if [ -z "$conflicting_files" ]; then
                conflicting_files="$file"
            else
                conflicting_files="$conflicting_files"$'\n'"$file"
            fi
        fi
    done < <(config ls-tree -r --name-only HEAD 2>/dev/null || true)

    if [ -z "$conflicting_files" ]; then
        log_success "No conflicting files found."
        return 0
    fi

    log_warning "The following files would be overwritten by checkout:"
    echo "$conflicting_files"
    echo

    log_warning "The following files would be overwritten by checkout:"
    echo "$conflicting_files"
    echo

    log_info "Backing up conflicting files and preparing for overwrite..."

    local backed_up_files=()
    local skipped_identical_files=()
    local removed_symlinks=()
    local skipped_dirs=()
    local backup_failed=false

    while IFS= read -r file; do
        if [ -n "$file" ]; then
            local source_path="$HOME/$file"

            if [ -e "$source_path" ] || [ -L "$source_path" ]; then
                if [ -L "$source_path" ]; then
                    # Handle symbolic links - just remove them (no backup needed)
                    if rm "$source_path"; then
                        removed_symlinks+=("$file")
                    else
                        log_error "Failed to remove symbolic link: $file"
                        backup_failed=true
                    fi
                elif [ -d "$source_path" ]; then
                    # Skip real directories - they contain other configs
                    skipped_dirs+=("$file")
                else
                    # Handle regular files with hash comparison
                    local local_hash
                    local repo_hash
                    local_hash=$(sha256sum "$source_path" | cut -d' ' -f1)
                    repo_hash=$(config show "HEAD:$file" | sha256sum | cut -d' ' -f1)

                    if [ "$local_hash" = "$repo_hash" ]; then
                        # Files are identical, just remove local (no backup needed)
                        if rm "$source_path"; then
                            skipped_identical_files+=("$file")
                        else
                            log_error "Failed to remove identical file: $file"
                            backup_failed=true
                        fi
                    else
                        # Files are different, backup first then remove
                        local backup_path="$BACKUP_DIR/$file"
                        local backup_dir
                        backup_dir=$(dirname "$backup_path")

                        # Create backup directory structure if needed
                        mkdir -p "$backup_dir"

                        # Copy file to backup location, then remove original
                        if cp "$source_path" "$backup_path" && rm "$source_path"; then
                            backed_up_files+=("$file")
                        else
                            log_error "Failed to backup and remove file: $file"
                            backup_failed=true
                        fi
                    fi
                fi
            fi
        fi
    done <<< "$conflicting_files"

    # Show summary
    echo
    if [ ${#backed_up_files[@]} -gt 0 ]; then
        log_success "Backed up and prepared ${#backed_up_files[@]} files for overwrite:"
        for file in "${backed_up_files[@]}"; do
            echo "  - $file"
        done
    fi

    if [ ${#skipped_identical_files[@]} -gt 0 ]; then
        echo
        log_info "Skipped ${#skipped_identical_files[@]} identical files (no backup needed):"
        for file in "${skipped_identical_files[@]}"; do
            echo "  - $file"
        done
    fi

    if [ ${#removed_symlinks[@]} -gt 0 ]; then
        echo
        log_info "Removed ${#removed_symlinks[@]} symbolic links (no backup needed):"
        for file in "${removed_symlinks[@]}"; do
            echo "  - $file"
        done
    fi

    if [ ${#skipped_dirs[@]} -gt 0 ]; then
        echo
        log_warning "Skipped ${#skipped_dirs[@]} directories (will be merged):"
        for dir in "${skipped_dirs[@]}"; do
            echo "  - $dir"
        done
    fi

    if [ "$backup_failed" = true ]; then
        log_error "Some files failed to backup. Aborting setup."
        exit 1
    fi
}

# Function to checkout dotfiles
checkout_dotfiles() {
    log_info "Checking out dotfiles..."

    if config checkout; then
        log_success "Dotfiles checked out successfully."
    else
        log_error "Failed to checkout dotfiles even after handling conflicts."
        exit 1
    fi
}

# Function to configure the repository
configure_repo() {
    log_info "Configuring repository settings..."

    # Hide untracked files
    config config --local status.showUntrackedFiles no

    log_success "Repository configured successfully."
}

# Main setup function
main() {
    log_info "Starting dotfiles setup..."
    echo

    # Check prerequisites
    check_git

    # Clone repository
    clone_repo

    # Setup config alias for this session
    setup_config_alias

    # Handle conflicting files and checkout
    if ! config checkout 2>/dev/null; then
        backup_conflicting_files
        checkout_dotfiles
    else
        log_success "Dotfiles checked out successfully with no conflicts."
    fi

    # Configure repository
    configure_repo

    echo
    log_success "Dotfiles setup completed successfully!"
    log_info "Restart your shell or run 'source ~/.bashrc' or 'source ~/.zshrc' to use the '$CONFIG_ALIAS' alias."

    if [ -d "$BACKUP_DIR" ] && [ "$(find "$BACKUP_DIR" -type f | wc -l)" -gt 0 ]; then
        echo
        log_info "Backed up files are available in: $BACKUP_DIR"
    fi
}

# Run main function
main "$@"
