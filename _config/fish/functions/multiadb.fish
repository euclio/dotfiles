# Send adb commands to all Android devices connected to the PC.
function multiadb
    adb devices | tail -n +2 | cut -sf 1 | xargs -iX adb -s X $argv
end
