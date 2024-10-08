return {
  "gnikdroy/projections.nvim",
  config = function()
    require("projections").setup({
      workspaces = {
        { "~/", { ".git" } },
        { "~/Repos", { ".git" } },
        { "~/Repos/BetConnections/", { ".git" } },
        { "~/Repos/Jesus", { ".git" } },
        { "~/Repos/Julio", { ".git" } },
        { "~/Repos/Web", { ".git" } },
        { "~/Documents/projects", { ".git" } },
      },
      workspaces_file = "~/.config/nvim/workspaces.json",
      sessions_directory = "~/.config/nvim/sessions/",
    })

    local Session = require("projections.session")
    local Workspace = require("projections.workspace")
    local Switcher = require("projections.switcher")
    vim.opt.sessionoptions:append("localoptions")

    -- Autostore session on VimExit
    vim.api.nvim_create_autocmd({ "VimLeavePre" }, {
      callback = function()
        Session.store(vim.loop.cwd())
      end,
    })

    -- Switch to project if vim was started in a project dir
    vim.api.nvim_create_autocmd({ "VimEnter" }, {
      callback = function()
        if vim.fn.argc() == 0 then
          Switcher.switch(vim.loop.cwd())
        end
      end,
    })

    -- Add commands
    vim.api.nvim_create_user_command("WorkspaceAdd", function()
      if vim.fn.argc() == 0 then
        Workspace.add(vim.loop.cwd() or "", { ".git" })
      else
        Workspace.add(vim.fn.argc(), { ".git" })
      end
    end, {})
    vim.api.nvim_create_user_command("WorkspaceRestore", function()
      Session.restore(vim.loop.cwd())
    end, {})
    vim.api.nvim_create_user_command("WorkspaceStore", function()
      Session.store(vim.loop.cwd())
    end, {})
    vim.api.nvim_create_user_command("WorkspaceRestoreLatest", function()
      Session.restore_latest(vim.loop.cwd())
    end, {})
  end,
}
