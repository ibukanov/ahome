#!/bin/sh

set -e -u

self="$(realpath "$0")"

if test "_$self" = "_$SSH_ASKPASS"; then
	if test -n "$TEST_SEND_SIGNAL"; then
		i=0
		while ! read -r pid < "$TEST_EXTRA_LOG"; do
			sleep 0.05
			: $((i+=1))
			if test $i -ge 100; then
				exit 1
			fi
		done
		printf '%s\n' "$@" > "$TEST_EXTRA_LOG"
		kill -s TERM "$pid"
		exit 0
	fi

	second_call=
	if test -s "$TEST_EXTRA_LOG" ; then
		second_call=1
	fi
	printf '%s\n' "$@" >> "$TEST_EXTRA_LOG"
	if test -n "$second_call" -a -n "$TEST_USE_PASSWORD2"; then
		printf '%s\n' "$TEST_PASSWORD2"
	else
		printf '%s\n' "$TEST_PASSWORD"
	fi
	if test -n "$second_call" -a -n "$TEST_FAIL_ON_SECOND_CALL"; then
		exit 1
	fi
	exit 0
fi


self_dir="${self%/*}"
prog="$self_dir/build/localkit"

extra_log="$(mktemp)"
cmd_output="$(mktemp)"
hash_file=

cleanup() {
	rm -f "$extra_log" "$cmd_output"
	test -n "$hash_file" && rm -f "$hash_file"
	return 0
}

trap cleanup EXIT

log() {
	printf '%s\n' "$*" >&2
}

err() {
	log "$@"
	exit 1
}

fail() {
	log "failed test - $*"
	exit 1
}

call_expect() {
	local script="$1" name="$2"

	if printf %s "$script" | expect > "$extra_log"; then
		return 0
	fi
	log "$name for tty failed"
	log "expect log:"
	cat "$extra_log" | sed -e 's|^|    |'
	exit 1
}

test_password_tty() {
	local password title prolog script
	password="qwe rty;"
	title="test"
	hash_file="$(mktemp)"

	type expect > /dev/null || err "expect is not installed"
	prolog='
#log_user 0
set title {'"$title"'}
set password {'"$password"'}
set prog {'"$prog"'}
set hash_file {'"$hash_file"'}
set timeout 5
'
	# Normal set-password-hash
	script="$prolog"'
spawn $prog set-password-hash -p $hash_file -t $title
expect {
	"^Enter a new password for $title: $" {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "$password\r"
expect {
	"\nRepeat to confirm the new value for $title: $" {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "$password\r"
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	eof {}
	-re "." {exit 1}
	timeout {exit 1}
}
'
	call_expect "$script" "normal set-password-hash"

	# set-password-hash with password mismatch
	script="$prolog"'
spawn $prog set-password-hash -p $hash_file -t $title
expect {
	"^Enter a new password for $title: $" {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "$password\r"
expect {
	"\nRepeat to confirm the new value for $title: $" {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "2$password\r"
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	"Password mismatch" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	eof {}
	-re "." {exit 1}
	timeout {exit 1}
}
'
	call_expect "$script" "set-password-hash password mismatch"

	#Normal ask-password
	script="$prolog"'
spawn $prog ask-password -p $hash_file -t $title
expect {
	"^Enter password for $title: $" {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "$password\r"
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	$password {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	eof {}
	-re "." {exit 1}
	timeout {exit 1}
}
'
	call_expect "$script" "normal ask-password"

	#ask-password with one mismatch
	script="$prolog"'
spawn $prog ask-password -p $hash_file -t $title
expect {
	"^Enter password for $title: $" {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "\r"
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	"Password mismatch, enter password for $title again: " {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "x$password\r"
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	"Password mismatch, enter password for $title again: " {}
	-re "." {exit 1}
	timeout {exit 1}
}
send "$password\r"
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	$password {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	eof {}
	-re "." {exit 1}
	timeout {exit 1}
}
'
	call_expect "$script" "ask-password password mismatch"

	#ask-password with Ctrl-C to cancel
	script="$prolog"'
spawn $prog ask-password -p $hash_file -t $title
expect {
	"^Enter password for $title: $" {}
	-re "." {exit 1}
	timeout {exit 1}
}
# Send Control-C
send "\x03"
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	"Canceled" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	"\n" {}
	-re "." {exit 1}
	timeout {exit 1}
}
expect {
	eof {}
	-re "." {exit 1}
	timeout {exit 1}
}
'
	call_expect "$script" "ask-password with signal"

	rm "$hash_file"
	hash_file=
}

call_test_password_gui() {
	local expected_status="$1" command="$2" name="$3" actual_status=0 diff
	printf '' > "$extra_log"
	if test "$send_signal"; then
		SSH_ASKPASS="$self" TEST_SEND_SIGNAL=1 TEST_EXTRA_LOG="$extra_log" \
			"$prog" "$command" -p "$hash_file" -t "$title" \
			< /dev/null > "$cmd_output" 2>/dev/null &
		printf '%d\n' $! > $extra_log
		wait $! || actual_status=1
	else
		SSH_ASKPASS="$self" TEST_SEND_SIGNAL= TEST_EXTRA_LOG="$extra_log" \
			TEST_PASSWORD="$password" \
			TEST_USE_PASSWORD2="$use_password2" TEST_PASSWORD2="$password2" \
			TEST_FAIL_ON_SECOND_CALL="$fail_on_second_call" \
			"$prog" "$command" -p "$hash_file" -t "$title" < /dev/null > "$cmd_output" \
			|| actual_status=1
	fi
	if test "$expected_status" -ne "$actual_status"; then
		log "$name failed - expected_status=$expected_status actual_status=$actual_status"
		log "SSH_ASKPASS log:"
		cat "$extra_log" | sed -e 's|^|    |'
		exit 1
	fi
	diff="$(printf %s "$expected_log" | diff - "$extra_log" || :)"
	if test -n "$diff"; then
		log "$name failed - log mismatch:"
		printf '%s\n' "$diff"
		exit 1
	fi
	if test -z "$output_is_password"; then
		if test -s "$cmd_output"; then
			log "$name failed - unexpected $prog output when none was expected:"
			cat "$cmd_output" | sed -e 's|^|    |'
			log ""
			exit 1
		fi
	else
		diff="$(printf %s "$password" | diff - "$cmd_output" || :)"
		if test -n "$diff"; then
			log "$name failed - output mismatch:"
			printf '%s\n' "$diff"
			exit 1
		fi
	fi
	return 0
}

test_password_gui() {
	local password use_password2= password2= title prolog script \
		expected_log fail_on_second_call= output_is_password= \
		send_signal=
	password="qwe rty;"
	title="test"
	hash_file="$(mktemp)"


	password2=
	expected_log="\
Enter a new password for test
Repeat to confirm the new value for test
"
	call_test_password_gui 0 set-password-hash "normal set-password-hash"

	use_password2=1
	password2=""
	expected_log="\
Enter a new password for test
Repeat to confirm the new value for test
Password mismatch
"
	call_test_password_gui 1 set-password-hash "set-password-hash with password mismatch"

	password2="${password}2"
	call_test_password_gui 1 set-password-hash "set-password-hash with password mismatch"

	use_password2=
	output_is_password=1
	expected_log="\
Enter password for test
"
	call_test_password_gui 0 ask-password "normal ask-password"

	s="$password"
	password=""
	fail_on_second_call=1
	output_is_password=
	expected_log="\
Enter password for test
Password mismatch, enter password for test again
"
	call_test_password_gui 1 ask-password "ask-password empty password mismatch"

	password="${s}2"
	call_test_password_gui 1 ask-password "ask-password password mismatch"

	fail_on_second_call=
	send_signal=1
	expected_log="\
Enter password for test
"
	call_test_password_gui 1 ask-password "ask-password with signal"

	rm "$hash_file"
	hash_file=
}

test_password_tty
test_password_gui
