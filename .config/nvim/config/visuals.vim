" visual settings
set number relativenumber

let g:gruvbox_italic = 1
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_sign_column = 'bg0'
colorscheme gruvbox

set noshowmode
let g:airline_powerline_fonts = 1
let g:airline_section_y = 'B:%{bufnr("%")}' " buf.nr. instead of encoding
let g:airline_section_z = '%l/%L:%c' " minimalistic line indicator
let g:airline_theme='minimalist'
let g:bufferline_echo = 0
let g:airline_mode_map = {}
let g:airline_mode_map['ic'] = 'INSERT'

" highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')
