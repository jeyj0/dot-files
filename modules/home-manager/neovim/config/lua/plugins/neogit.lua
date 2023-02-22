return {
  -- add neogit
  {
    "https://github.com/TimUntersberger/neogit",
    cmd = "Neogit",
    keys = { { "<leader>gg", "<cmd>Neogit<cr>", desc = "Neogit (Magit for Neovim)" } },
    opts = {
      -- add your options that should be passed to the setup() function here
      -- position = "right",
    },
  },
}
