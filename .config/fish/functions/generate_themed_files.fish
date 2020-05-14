function generate_themed_files
    node ~/scripts/theme/index.js set \
        (node ~/scripts/theme/index.js list | rofi -dmenu -i)
end
