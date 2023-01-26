# shellcheck shell=dash
# shellcheck enable=all

readonly container_stable_id_start=2000
readonly container_socket_access_subgid=2000

# Based on entries in /etc/subgid for the given user map the shift id into the
# host id.
get_user_subgid_for_shift() {
  local user shift saved_shift
  user="$1"
  shift="$2"
  saved_shift="${shift}"
  local subgid_user subgid_start subgid_count
  while IFS=: read -r subgid_user subgid_start subgid_count; do
    test "${subgid_user}" = "${user}" || continue
    if test "${shift}" -lt "${subgid_count}"; then
      R="$((subgid_start + shift))"
      return 0
    fi
    : "$((shift -= subgid_count))"
  done < /etc/subgid
  err "Failed to find /etc/subgid range for the user ${user}" \
    " at shift ${saved_shift}"
}
