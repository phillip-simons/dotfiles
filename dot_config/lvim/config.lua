-- Read the docs: https://www.lunarvim.org/docs/configuration
-- Example configs: https://github.com/LunarVim/starter.lvim
-- Video Tutorials: https://www.youtube.com/watch?v=sFA9kX-Ud_c&list=PLhoH5vyxr6QqGu0i7tt_XoVK9v-KvZ3m6
-- Forum: https://www.reddit.com/r/lunarvim/
-- Discord: https://discord.com/invite/Xb9B4Ny
--
--
lvim.plugins = {
  "simrat39/rust-tools.nvim",
  {
    "saecki/crates.nvim",
    version = "v0.3.0",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("crates").setup {
        null_ls = {
          enabled = true,
          name = "crates.nvim",
        },
        popup = {
          border = "rounded",
        },
      }
    end,
  },
  {
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup()
    end,
  },
  {
    'NTBBloodbath/doom-one.nvim',
    config = function()
      -- Add color to cursor
      vim.g.doom_one_cursor_coloring = false
      -- Set :terminal colors
      vim.g.doom_one_terminal_colors = true
      -- Enable italic comments
      vim.g.doom_one_italic_comments = false
      -- Enable TS support
      vim.g.doom_one_enable_treesitter = true
      -- Color whole diagnostic text or only underline
      vim.g.doom_one_diagnostics_text_color = false
      -- Enable transparent background
      vim.g.doom_one_transparent_background = false

      -- Pumblend transparency
      vim.g.doom_one_pumblend_enable = false
      vim.g.doom_one_pumblend_transparency = 20

      -- Plugins integration
      vim.g.doom_one_plugin_neorg = true
      vim.g.doom_one_plugin_barbar = false
      vim.g.doom_one_plugin_telescope = false
      vim.g.doom_one_plugin_neogit = true
      vim.g.doom_one_plugin_nvim_tree = true
      vim.g.doom_one_plugin_dashboard = true
      vim.g.doom_one_plugin_startify = true
      vim.g.doom_one_plugin_whichkey = true
      vim.g.doom_one_plugin_indent_blankline = true
      vim.g.doom_one_plugin_vim_illuminate = true
      vim.g.doom_one_plugin_lspsaga = false

      -- Apply the colorscheme
      vim.cmd("colorscheme doom-one")
    end,
  },
  {
    "lukas-reineke/headlines.nvim",
    dependencies = "nvim-treesitter/nvim-treesitter",
    config = function()
      require("headlines").setup({
        norg = {
          headline_highlights = { "Headline1", "Headline2", "Headline3", "Headline4", "Headline5", "Headline6" },
          codeblock_highlight = { "NeorgCodeBlock" },
        }
      })
    end,
  },
  {
    "stevearc/dressing.nvim",
    config = function()
      require("dressing").setup({
        input = { enabled = false },
      })
    end,
  },
  {
    "nvim-neorg/neorg",
    ft = "norg",
    config = true,
  },
  {
    'wfxr/minimap.vim',
    build = "cargo install --locked code-minimap",
    config = function()
      vim.cmd("let g:minimap_width = 10")
      vim.cmd("let g:minimap_auto_start = 1")
      vim.cmd("let g:minimap_auto_start_win_enter = 1")
    end,
  },
  {
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("colorizer").setup({ "css", "scss", "html", "javascript" }, {
        RGB = true,
        RRGGBB = true,
        RRGGBBAA = true,
        rgb_fn = true,
        hsl_fn = true,
        css = true,
        css_fn = true,
      })
    end,
  },
  {
    "rmagatti/goto-preview",
    config = function()
      require('goto-preview').setup {
        width = 120,
        height = 25,
        default_mappings = false,
        debug = false,
        opacity = nil,
        post_open_hook = nil,
      }
    end
  },
  {
    "folke/trouble.nvim",
    cmd = "TroubleToggle",
  },
  {
    "zbirenbaum/copilot-cmp",
    event = "InsertEnter",
    dependencies = { "zbirenbaum/copilot.lua" },
    config = function()
      vim.defer_fn(function()
        require("copilot").setup()
        require("copilot_cmp").setup()
      end, 100)
    end,
  },
  {
    "folke/todo-comments.nvim",
    event = "BufRead",
    config = function()
      require("todo-comments").setup()
    end,
  },
  {
    "tpope/vim-surround",
  },
  -- Added plugins from the second config
  "ChristianChiarulli/swenv.nvim",
  "stevearc/dressing.nvim",
  "mfussenegger/nvim-dap-python",
  "nvim-neotest/neotest",
  "nvim-neotest/neotest-python",
}

-- Treesitter
lvim.builtin.treesitter.ensure_installed = {
  "lua",
  "rust",
  "toml",
  "python",
}

-- Formatter
local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup { { name = "black" }, }
lvim.format_on_save.enabled = true
lvim.format_on_save.pattern = { "*.py" }

-- Linting
local linters = require "lvim.lsp.null-ls.linters"
linters.setup { { command = "flake8", filetypes = { "python" } } }

-- DAP
lvim.builtin.dap.active = true
local mason_path = vim.fn.glob(vim.fn.stdpath "data" .. "/mason/")
pcall(function()
  require("dap-python").setup(mason_path .. "packages/debugpy/venv/bin/python")
end)

-- Rust Tools
vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "rust_analyzer" })

local codelldb_path = mason_path .. "bin/codelldb"
local liblldb_path = mason_path .. "packages/codelldb/extension/lldb/lib/liblldb"
local this_os = vim.loop.os_uname().sysname

if this_os:find "Windows" then
  codelldb_path = mason_path .. "packages\\codelldb\\extension\\adapter\\codelldb.exe"
  liblldb_path = mason_path .. "packages\\codelldb\\extension\\lldb\\bin\\liblldb.dll"
else
  liblldb_path = liblldb_path .. (this_os == "Linux" and ".so" or ".dylib")
end

pcall(function()
  require("rust-tools").setup {
    tools = {
      executor = require("rust-tools/executors").termopen,
      reload_workspace_from_cargo_toml = true,
      runnables = {
        use_telescope = true,
      },
      inlay_hints = {
        auto = true,
        only_current_line = false,
        show_parameter_hints = false,
        parameter_hints_prefix = "<-",
        other_hints_prefix = "=>",
        max_len_align = false,
        max_len_align_padding = 1,
        right_align = false,
        right_align_padding = 7,
        highlight = "Comment",
      },
      hover_actions = {
        border = "rounded",
      },
      on_initialized = function()
        vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "CursorHold", "InsertLeave" }, {
          pattern = { "*.rs" },
          callback = function()
            local _, _ = pcall(vim.lsp.codelens.refresh)
          end,
        })
      end,
    },
    dap = {
      adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
    },
    server = {
      on_attach = function(client, bufnr)
        require("lvim.lsp").common_on_attach(client, bufnr)
        local rt = require "rust-tools"
        vim.keymap.set("n", "K", rt.hover_actions.hover_actions, { buffer = bufnr })
      end,

      capabilities = require("lvim.lsp").common_capabilities(),
      settings = {
        ["rust-analyzer"] = {
          lens = {
            enable = true,
          },
          checkOnSave = {
            enable = true,
            command = "clippy",
          },
        },
      },
    },
  }
end)
lvim.builtin.dap.on_config_done = function(dap)
  local mason_path = vim.fn.glob(vim.fn.stdpath "data" .. "/mason/")
  local codelldb_path = mason_path .. "bin/codelldb"
  local liblldb_path = mason_path .. "packages/codelldb/extension/lldb/lib/liblldb"
  local this_os = vim.loop.os_uname().sysname

  if this_os:find "Windows" then
    codelldb_path = mason_path .. "packages\\codelldb\\extension\\adapter\\codelldb.exe"
    liblldb_path = mason_path .. "packages\\codelldb\\extension\\lldb\\bin\\liblldb.dll"
  else
    liblldb_path = liblldb_path .. (this_os == "Linux" and ".so" or ".dylib")
  end

  -- Setup DAP for Rust using codelldb
  dap.adapters.codelldb = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path)
  dap.configurations.rust = {
    {
      name = "Launch file",
      type = "codelldb",
      request = "launch",
      program = function()
        return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
      end,
      cwd = "${workspaceFolder}",
      stopOnEntry = false,
    },
  }
end

-- Python DAP setup
lvim.builtin.dap.active = true
pcall(function()
  require("dap-python").setup(vim.fn.glob(vim.fn.stdpath("data") .. "/mason/packages/debugpy/venv/bin/python"))
end)

-- Treesitter setup for both Rust and Python
lvim.builtin.treesitter.ensure_installed = {
  "lua",
  "rust",
  "toml",
  "python",
}

-- LSP setup for Rust
vim.list_extend(lvim.lsp.automatic_configuration.skipped_servers, { "rust_analyzer" })

pcall(function()
  require("rust-tools").setup {
    tools = {
      executor = require("rust-tools/executors").termopen,
      reload_workspace_from_cargo_toml = true,
      runnables = { use_telescope = true },
      inlay_hints = {
        auto = true,
        only_current_line = false,
        show_parameter_hints = false,
        parameter_hints_prefix = "<-",
        other_hints_prefix = "=>",
        max_len_align = false,
        max_len_align_padding = 1,
        right_align = false,
        right_align_padding = 7,
        highlight = "Comment",
      },
      hover_actions = { border = "rounded" },
      on_initialized = function()
        vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "CursorHold", "InsertLeave" }, {
          pattern = { "*.rs" },
          callback = function()
            local _, _ = pcall(vim.lsp.codelens.refresh)
          end,
        })
      end,
    },
    dap = {
      adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
    },
    server = {
      on_attach = function(client, bufnr)
        require("lvim.lsp").common_on_attach(client, bufnr)
        local rt = require "rust-tools"
        vim.keymap.set("n", "K", rt.hover_actions.hover_actions, { buffer = bufnr })
      end,
      capabilities = require("lvim.lsp").common_capabilities(),
      settings = {
        ["rust-analyzer"] = {
          lens = { enable = true },
          checkOnSave = { enable = true, command = "clippy" },
        },
      },
    },
  }
end)

-- Python formatting and linting setup
local formatters = require "lvim.lsp.null-ls.formatters"
formatters.setup { { name = "black" } }
lvim.format_on_save.enabled = true
lvim.format_on_save.pattern = { "*.py" }

local linters = require "lvim.lsp.null-ls.linters"
linters.setup { { command = "flake8", filetypes = { "python" } } }

-- Python testing setup
require("neotest").setup({
  adapters = {
    require("neotest-python")({
      dap = {
        justMyCode = false,
        console = "integratedTerminal",
      },
      args = { "--log-level", "DEBUG", "--quiet" },
      runner = "pytest",
    })
  }
})

lvim.autocommands = {
  {
    "BufEnter",
    {
      pattern = { "*.rs" },
      callback = function()
        lvim.builtin.which_key.mappings["C"] = {
          name = "Rust",
          r = { "<cmd>RustRunnables<Cr>", "Runnables" },
          t = { "<cmd>lua _CARGO_TEST()<cr>", "Cargo Test" },
          m = { "<cmd>RustExpandMacro<Cr>", "Expand Macro" },
          c = { "<cmd>RustOpenCargo<Cr>", "Open Cargo" },
          p = { "<cmd>RustParentModule<Cr>", "Parent Module" },
          d = { "<cmd>RustDebuggables<Cr>", "Debuggables" },
          v = { "<cmd>RustViewCrateGraph<Cr>", "View Crate Graph" },
          R = {
            "<cmd>lua require('rust-tools/workspace_refresh')._reload_workspace_from_cargo_toml()<Cr>",
            "Reload Workspace",
          },
          o = { "<cmd>RustOpenExternalDocs<Cr>", "Open External Docs" },
          y = { "<cmd>lua require'crates'.open_repository()<cr>", "[crates] open repository" },
          P = { "<cmd>lua require'crates'.show_popup()<cr>", "[crates] show popup" },
          i = { "<cmd>lua require'crates'.show_crate_popup()<cr>", "[crates] show info" },
          f = { "<cmd>lua require'crates'.show_features_popup()<cr>", "[crates] show features" },
          D = { "<cmd>lua require'crates'.show_dependencies_popup()<cr>", "[crates] show dependencies" },
        }
      end,
    },
  },
  {
    "BufEnter",
    {
      pattern = { "*.py" },
      callback = function()
        lvim.builtin.which_key.mappings["C"] = {
          name = "Python",
          c = { "<cmd>lua require('swenv.api').pick_venv()<cr>", "Choose Env" },
        }
        lvim.builtin.which_key.mappings["dm"] = {
          "<cmd>lua require('neotest').run.run()<cr>",
          "Test Method"
        }
        lvim.builtin.which_key.mappings["dM"] = {
          "<cmd>lua require('neotest').run.run({strategy = 'dap'})<cr>",
          "Test Method DAP"
        }
        lvim.builtin.which_key.mappings["df"] = {
          "<cmd>lua require('neotest').run.run({vim.fn.expand('%')})<cr>", "Test Class"
        }
        lvim.builtin.which_key.mappings["dF"] = {
          "<cmd>lua require('neotest').run.run({vim.fn.expand('%'), strategy = 'dap'})<cr>", "Test Class DAP"
        }
        lvim.builtin.which_key.mappings["dS"] = {
          "<cmd>lua require('neotest').summary.toggle()<cr>", "Test Summary"
        }
      end,
    },
  },
}
