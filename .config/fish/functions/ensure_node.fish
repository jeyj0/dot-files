function ensure_node
  if not which node >/dev/null
    nvm use default
  end
end

