command_not_found_handler() {
  local pkgs cmd="$1"

  pkgs=(${(f)"$(pkgfile -b -v -- "$cmd" 2>/dev/null)"})
  if [[ -n "$pkgs" ]]; then
    if [[ ${#pkgs[@]} -eq 1 && -n $PKGFILE_PROMPT_INSTALL_MISSING ]]; then
      local pkg=${pkgs[1]%% *}
      local response

      read -r "response?Install $pkg? [Y/n] "
      [[ -z $response || $response = [Yy] ]] || return 0
      printf '\n'
      sudo pacman -S --noconfirm -- "$pkg"
      return
    fi

    printf '%s may be found in the following packages:\n' "$cmd"
    printf '  %s\n' $pkgs[@]
  else
    printf 'zsh: command not found: %s\n' "$cmd"
  fi 1>&2

  return 127
}
