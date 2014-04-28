#!/bin/bash
#
# Check the output of acpi to determine if the battery is discharging and
# whether the battery remaining is less than 5 percent. If both are true, then
# suspend the system.
#
# Intended to run as a cron job every minute
[[ $(/usr/bin/acpi | /usr/bin/awk '{exit !($3 == "Discharging," && $4 < 5)}') ]] &&
    /usr/bin/systemctl "suspend"
