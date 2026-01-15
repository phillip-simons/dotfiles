#!/usr/bin/env bash
# Combined disk usage for / and /home partitions

# Get disk info using df (in 1K blocks)
read -r root_used root_total < <(df -P / | awk 'NR==2 {print $3, $2}')
read -r home_used home_total < <(df -P /home | awk 'NR==2 {print $3, $2}')

# Check if /home is a separate partition (different from /)
root_dev=$(df -P / | awk 'NR==2 {print $1}')
home_dev=$(df -P /home | awk 'NR==2 {print $1}')

if [[ "$root_dev" == "$home_dev" ]]; then
    # Same partition, just show root
    total=$root_total
    used=$root_used
else
    # Different partitions, combine them
    total=$((root_total + home_total))
    used=$((root_used + home_total))
fi

# Calculate percentage
percentage=$((used * 100 / total))

# Convert to human readable
human_used=$(numfmt --to=iec --suffix=B $((used * 1024)) 2>/dev/null || echo "${used}K")
human_total=$(numfmt --to=iec --suffix=B $((total * 1024)) 2>/dev/null || echo "${total}K")

# Build tooltip
if [[ "$root_dev" == "$home_dev" ]]; then
    tooltip="${human_used} used out of ${human_total} on / (${percentage}%)"
else
    root_pct=$((root_used * 100 / root_total))
    home_pct=$((home_used * 100 / home_total))
    root_human=$(numfmt --to=iec --suffix=B $((root_used * 1024)) 2>/dev/null)
    root_total_h=$(numfmt --to=iec --suffix=B $((root_total * 1024)) 2>/dev/null)
    home_human=$(numfmt --to=iec --suffix=B $((home_used * 1024)) 2>/dev/null)
    home_total_h=$(numfmt --to=iec --suffix=B $((home_total * 1024)) 2>/dev/null)
    tooltip="${root_human} used out of ${root_total_h} on / (${root_pct}%)\n${home_human} used out of ${home_total_h} on /home (${home_pct}%)"
fi

# Output JSON for waybar
printf '{"text": "%s%% 󰋊", "tooltip": "%s", "percentage": %d}\n' "$percentage" "$tooltip" "$percentage"
