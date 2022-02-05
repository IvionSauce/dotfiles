#!/bin/bash

## Set temperatures for when to warn (make red) and/or hide.
## Only integers are supported.
WARN_TEMP_CPU=80
HIDE_TEMP_CPU=0

WARN_TEMP_GPU=80
HIDE_TEMP_GPU=65


# My take on https://github.com/i3/i3status/blob/28399bf84693a03eb772be647d5927011c1d2619/contrib/any_position_wrapper.sh
# ♫ DO YOU LIKE.. MY BARRR? ♫

update-holder() {
    local instance=$1
    local replacement=$2

    # Replace object(s) with `instance` key set to `$instance` with
    # our own JSON object.
    echo "$json_array" | \
	jq --compact-output --argjson arg_j "$replacement" \
	   "(.[] | (select(.instance==\"$instance\"))) |= \$arg_j"
}

# Outputs a JSON object with the KVs we want.
temp-color-object() {
    local ftext=$1
    shift

    if (($1 < $2)); then
	printf '{"full_text": "%s"}' "$ftext"
    else
	printf '{"full_text": "%s", "color": "%s"}' \
	       "$ftext" '#FF0000'
    fi
}


# Get everything we need in one go, with a single AWK script. This makes the
# output type/shape kinda weird, hence we do some ad-hoc destructuring later.
read -d '' -r cpu_temps_awk << 'EOA'
/^T(ccd[0-9]+|ctl):/ {
    # Remove the sign, keep the units (°C).
    pretty_temps = pretty_temps substr($2, 2) " "

    # Simple temperature: integer (truncated) without sign or units.
    simple_temp = substr($2, 2, length($2) - 5) + 0
    # Keep track of the highest temperature.
    if (simple_temp > top_temp)
	top_temp = simple_temp
}
END {
    print top_temp, substr(pretty_temps, 0, length(pretty_temps) - 1)
}
EOA

cpu-temps() {
    # Temperatures output shape: <top_temp> <pretty_temp[...]>
    # Example (for a CPU with 3 sensors): 36 36.1°C 35.5°C 32.5°C
    set -- $(sensors | awk "$cpu_temps_awk")
    local top_temp=$1
    shift # Destructuring, no sugar.

    if ((top_temp >= HIDE_TEMP_CPU)); then
	local text="CPU: $@"

	json_array=$(update-holder mybar__cpuT \
	    "$(temp-color-object "$text" $top_temp $WARN_TEMP_CPU)")
    fi
}


nvidia-temp() {
    local gpu_temp=$(nvidia-smi \
			 --query-gpu=temperature.gpu --format=csv,noheader)

    if ((gpu_temp >= HIDE_TEMP_GPU)); then
	local text="GPU: ${gpu_temp}°C"

	json_array=$(update-holder mybar__gpuT \
	    "$(temp-color-object "$text" $gpu_temp $WARN_TEMP_GPU)")
    fi
}

# `i3status` in i3bar mode spits out an endless JSON array (with some caveats
# stipulated below), each element being an array of objects. That array of
# objects comprises all the status information; each such status-list is written
# out by i3status on a single line.
# The interval specified in the i3status configuration determines the cadence
# of the endless array, and thus the frequency with which the above commands
# and programs will be called.
i3status | \
    {
	# Pass the first three lines on unchanged:
	# 1st line, version object;
	# 2nd line, endless array opening bracket;
	# 3rd line, actual start of the statuses. We skip the first status-list
	# because it makes the logic more straightforward - all lines
	# henceforth will start with a comma.
	for i in {1..3}; do read; echo "$REPLY"; done
	# We start processing the statuses...
	while :; do
	    read line
	    # Temporarily remove leading comma.
	    json_array=${line:1}
	    # After that we patch the info we want into the status-list
	    # using `jq` and return the updated list.
	    cpu-temps; nvidia-temp
	    echo ",$json_array"
	done
    }
