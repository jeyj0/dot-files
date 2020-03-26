function generate_themed_files
    node ~/scripts/theme/index.js set (node ~/scripts/theme/index.js list | fzf)
end
