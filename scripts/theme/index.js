const path = require('path')
const fs = require('fs').promises

const THEMES = {
    'molokai': molokaiTheme(),
    'molokaiGreen': molokaiGreenTheme(),
    'molokaiMagenta': molokaiMagentaTheme(),
    'solarized': solarized(),
}

const command = process.argv[2]

switch (command) {
    case 'list':
        Object.keys(THEMES).forEach(theme => console.log(theme))
        break;
    case 'set':
        const theme = process.argv[3]
        if (!Object.keys(THEMES).includes(theme)) {
            console.error(`Unknown theme: ${theme}`)
            return
        }
        main(theme)
        break;
    default:
        console.error(`Unknown command '${command}'. Use 'list' to list all available themes, or 'set THEME' to change the current theme.`)
        break;
}

// ###################################################################
// THEME definitions
// ###################################################################

// REQUIRED KEYS:
// bg
// bg_alt
// fg
// fg_emphasis
// fg_secondary
//
// grey
// red
// orange
// green
// teal
// yellow
// blue
// dark_blue
// magenta
// violet
// cyan
// dark_cyan
//
// primary
// warning
// error

function solarized() {
    const solarized = {
        color0    : "#073642",
        color1    : "#dc322f",
        color2    : "#859900",
        color3    : "#b58900",
        color4    : "#268bd2",
        color5    : "#d33682",
        color6    : "#2aa198",
        color7    : "#eee8d5",
        color8    : "#002b36",
        color9    : "#cb4b16",
        color10   : "#586e75",
        color11   : "#657b83",
        color12   : "#839496",
        color13   : "#6c71c4",
        color14   : "#93a1a1",
        color15   : "#fdf6e3",
    }
    solarized['base03'] = solarized.color8
    solarized['base02'] = solarized.color0
    solarized['base01'] = solarized.color10
    solarized['base00'] = solarized.color11
    solarized['base0'] = solarized.color12
    solarized['base1'] = solarized.color14
    solarized['base2'] = solarized.color7
    solarized['base3'] = solarized.color15
    solarized['yellow'] = solarized.color3
    solarized['orange'] = solarized.color9
    solarized['red'] = solarized.color1
    solarized['magenta'] = solarized.color5
    solarized['violet'] = solarized.color13
    solarized['blue'] = solarized.color4
    solarized['cyan'] = solarized.color6
    solarized['green'] = solarized.color2

    solarized['bg'] = solarized.base03
    solarized['bg_alt'] = solarized.base02
    solarized['fg'] = solarized.base0
    solarized['fg_emphasis'] = solarized.base1
    solarized['fg_secondary'] = solarized.base01

    solarized['primary'] = solarized.blue
    solarized['warning'] = solarized.orange
    solarized['error'] = solarized.red
    return solarized
}

function molokaiTheme() {
    const molokai = {
        bg          : "#1c1e1f",
        bg_alt      : "#222323",
        fg          : "#d6d6d4",
        fg_secondary: "#556172",
        grey        : "#525254",
        red         : "#e74c3c",
        orange      : "#fd971f",
        green       : "#b6e63e",
        teal        : "#b6e63e",
        yellow      : "#e2c770",
        blue        : "#268bd2",
        dark_blue   : "#727280",
        magenta     : "#fb2874",
        violet      : "#9c91e4",
        cyan        : "#66d9ef",
        dark_cyan   : "#8fa1b3",
    }
    molokai['color0']  = molokai.bg_alt,
    molokai['color8']  = molokai.bg,
    molokai['color1']  = molokai.red,
    molokai['color9']  = molokai.orange,
    molokai['color2']  = molokai.teal,
    molokai['color10'] = molokai.green,
    molokai['color3']  = molokai.yellow,
    molokai['color11'] = molokai.yellow,
    molokai['color4']  = molokai.blue,
    molokai['color12'] = molokai.dark_blue,
    molokai['color5']  = molokai.magenta,
    molokai['color13'] = molokai.violet,
    molokai['color6']  = molokai.cyan,
    molokai['color14'] = molokai.dark_cyan,
    molokai['color7']  = molokai.fg,
    molokai['color15'] = molokai.fg_secondary,

    molokai['fg_emphasis'] = molokai.fg
    molokai['primary']     = molokai.orange
    molokai['warning']     = molokai.magenta
    molokai['error']       = molokai.red
    return molokai
}

function molokaiGreenTheme() {
    const molokaiGreen = molokaiTheme()
    molokaiGreen.primary = molokaiGreen.green
    return molokaiGreen
}

function molokaiMagentaTheme() {
    const molokaiMagenta = molokaiTheme()
    molokaiMagenta.primary = molokaiMagenta.magenta
    return molokaiMagenta
}

// ###################################################################
// FILE GENERATION
// ###################################################################

async function main(themename) {
    const theme = THEMES[themename]

    try {
        await Promise.all([
            generateXResources(theme),
            generateKittyConf(theme),
        ])
    } catch (err) {
        console.error(err)
    }
}

async function writeFile(path, content) {
    await fs.writeFile(path, content)
}

async function generateXResources(theme) {
    const xresources = {
        background: theme.bg,
        backgroundAlt: theme.bg_alt,
        foreground: theme.fg,
        foregroundAlt: theme.fg_secondary,
        fadeColor: theme.bg_alt,
        cursorColor: theme.bg_alt,
        pointerColorBackground: theme.bg,
        pointerColorForeground: theme.fg,

        color0: theme.bg_alt,
        color8: theme.bg,
        color1: theme.red,
        color9: theme.orange,
        color2: theme.teal,
        color10: theme.green,
        color3: theme.yellow,
        color11: theme.yellow,
        color4: theme.blue,
        color12: theme.dark_blue,
        color5: theme.magenta,
        color13: theme.violet,
        color6: theme.cyan,
        color14: theme.dark_cyan,
        color7: theme.fg,
        color15: theme.fg_alt,

        primary: theme.primary,
        warning: theme.warning,
        error: theme.error,
    }

    const fileContents = Object.entries(xresources)
        .reduce((contents, [key, value]) => `${contents}*${key}: ${value}\n`, '')

    await writeFile(path.join(process.env.HOME, '.Xresources'), fileContents)
}

async function generateKittyConf(theme) {
    let template = await fs.readFile(path.join(process.env.HOME, '.config/configtemplates/kitty/kitty.conf'), "utf8")

    const colorNames = Object.keys(theme)

    const config = colorNames.reduce((template, colorName) => {
        return template.replace(`%${colorName}%`, theme[colorName])
    }, template)

    await writeFile(path.join(process.env.HOME, '.config/kitty/kitty.conf'), config)
}
