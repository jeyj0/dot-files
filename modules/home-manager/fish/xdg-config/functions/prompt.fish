function prompt
    set current_prompt (head -1 ~/.config/starship.toml)

    switch $current_prompt
        case "# default"
            echo "# short" > ~/.config/starship.toml
            cat ~/.config/configtemplates/starship_short.toml >> ~/.config/starship.toml
        case "# short"
            echo "# default" > ~/.config/starship.toml
            cat ~/.config/configtemplates/starship.toml >> ~/.config/starship.toml
    end
end
