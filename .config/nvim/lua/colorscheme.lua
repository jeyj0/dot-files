-- vim.cmd [[
--     """""" UI """"""
--     " set termguicolors
--     autocmd colorscheme * highlight GitSignsAdd gui=none guifg=#A1C281 guibg=None
--     autocmd colorscheme * highlight GitSignsChange gui=none guifg=#74ADEA guibg=None
--     autocmd colorscheme * highlight GitSignsDelete gui=none guifg=#FE747A guibg=None
--     let g:vscode_style = "dark"
--     let g:vscode_transparency = 1
--     let g:vscode_italic_comment = 1
--     let g:vscode_disable_nvimtree_bg = v:true
--     let g:onedark_transparent = v:true
--     let g:onedark_transparent_sidebar = v:true
--     let g:onedark_highlight_linenumber = v:true
--     let g:onedark_dark_sidebar = v:false
--     let g:onedark_dark_float = v:false
--     colorscheme vscode
--     set guicursor=i:ver100-blinkon100
--                 \,r:hor100
--     if has('nvim')
--     set signcolumn=yes:1
--     else
--         set signcolumn=yes
--     endif
-- ]]
vim.opt.background = "dark"
vim.cmd [[
    let g:gruvbox_contrast_dark = "hard"
    let g:gruvbox_sign_column = "bg0"
    set signcolumn=yes:1
    colorscheme gruvbox
]]
