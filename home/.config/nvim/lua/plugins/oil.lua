return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    default_file_explorer = true,
    columns = {
      "icon",
      -- "permissions",
      -- "size",
      -- "mtime",
    },
    buf_options = {
      buflisted = true,
      bufhidden = "hide",
    },
    win_options = {
      wrap = false,
      signcolumn = "no",
      cursorcolumn = false,
      foldcolumn = "0",
      spell = false,
      list = false,
      conceallevel = 3,
      concealcursor = "nvic",
    },
    delete_to_trash = false,
    skip_confirm_for_simple_edits = true,
    prompt_save_on_select_new_entry = true,
    constrain_cursor = "editable",
    experimental_watch_for_changes = false,
    keymaps = {
      ["g?"] = "actions.show_help",
      ["Esc"] = "actions.close",
      ["q"] = "actions.close",
      ["-"] = "actions.parent",
      ["<Left>"] = "actions.parent",
      ["<CR>"] = "actions.select",
      ["<Right>"] = "actions.select",
      ["<C-v>"] = "actions.select_vsplit",
      ["<C-h>"] = "actions.toggle_hidden",
      ["<C-t>"] = "actions.select_tab",
      ["<C-p>"] = "actions.preview",
      ["<C-c>"] = "actions.close",
      ["_"] = "actions.open_cwd",
      ["`"] = "actions.cd",
      ["~"] = "actions.tcd",
      ["gr"] = "actions.refresh",
      ["gs"] = "actions.change_sort",
      ["gx"] = "actions.open_external",
      ["g."] = "actions.toggle_hidden",
      ["g\\"] = "actions.toggle_trash",
    },
    view_options = {
      show_hidden = true,
      is_always_hidden = function(name)
        if name == ".." then
          return true
        end
        return false
      end,
    },
    preview = {
      max_width = 0.9,
      min_width = { 40, 0.4 },
      width = nil,
      max_height = 0.9,
      min_height = { 5, 0.1 },
      height = nil,
      border = "rounded",
      win_options = {
        winblend = 0,
      },
      update_on_cursor_moved = true,
    },
    progress = {
      max_width = 0.9,
      min_width = { 40, 0.4 },
      width = nil,
      max_height = { 10, 0.9 },
      min_height = { 5, 0.1 },
      height = nil,
      border = "rounded",
      minimized_border = "none",
      win_options = {
        winblend = 0,
      },
    },
    float = {
      padding = 2,
      max_width = 0,
      max_height = 0,
      border = "rounded",
      win_options = {
        winblend = 0,
      },
      -- preview_split: Split direction: "auto", "left", "right", "above", "below".
      preview_split = "right",
    },
    ssh = {
      border = "rounded",
    },
    keymaps_help = {
      border = "rounded",
    },
  },
}
