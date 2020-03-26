const path = require('path')
const fs = require('fs').promises

const THEMES = {
    'molokai': molokaiTheme(),
    'molokaiGreen': molokaiGreenTheme(),
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
        console.log('Generating files for theme', theme)
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
// base[0-8]
// fg
// fg_alt
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

function molokaiTheme() {
    const molokai = {
        bg        : "#1c1e1f",
        bg_alt    : "#222323",
        base0     : "#1b2229",
        base1     : "#151617",
        base2     : "#1d1f20",
        base3     : "#2d2e2e",
        base4     : "#4e4e4e",
        base5     : "#555556",
        base6     : "#767679",
        base7     : "#cfc0c5",
        base8     : "#ffffff",
        fg        : "#d6d6d4",
        fg_alt    : "#556172",
        grey      : "#525254",
        red       : "#e74c3c",
        orange    : "#fd971f",
        green     : "#b6e63e",
        teal      : "#b6e63e",
        yellow    : "#e2c770",
        blue      : "#268bd2",
        dark_blue : "#727280",
        magenta   : "#fb2874",
        violet    : "#9c91e4",
        cyan      : "#66d9ef",
        dark_cyan : "#8fa1b3",
    }
    molokai['primary'] = molokai.orange
    molokai['warning'] = molokai.magenta
    molokai['error']   = molokai.red
    return molokai
}

function molokaiGreenTheme() {
    const molokaiGreen = molokaiTheme()
    molokaiGreen.primary = molokaiGreen.green
    return molokaiGreen
}

// ###################################################################
// FILE GENERATION
// ###################################################################

async function main(themename) {
    const theme = THEMES[themename]

    try {
        await Promise.all([
            generateXResources(theme),
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
        foregroundAlt: theme.fg_alt,
        fadeColor: theme.bg_alt,
        cursorColor: theme.fg_alt,
        pointerColorBackground: theme.base0,
        pointerColorForeground: theme.fg,

        color0: theme.base0,
        color8: theme.base3,
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
