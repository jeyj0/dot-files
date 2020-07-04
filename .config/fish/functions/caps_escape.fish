function caps_escape
  xmodmap -e "clear Lock"
  xmodmap -e "keysym Caps_Lock = Escape"
end
