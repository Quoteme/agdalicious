-- agdalicious: thin neovim front-end for agdalicious-lsp.
-- The plugin only (1) starts the server, (2) renders the infoview,
-- (3) expands \-abbreviations and (4) makes <C-a>/<C-x> count in sub- and
-- superscripts. Everything else is the server's job.
local M = {}

M.config = {
  lsp = {
    cmd = { "agdalicious-lsp" },
  },
  infoview = {
    width = 50,
  },
  unicode = {
    enabled = true,
    leader = "\\",
    -- extra or overriding abbreviations, e.g. { ["bZ"] = "ℤ" }
    extra = {},
  },
  increment = {
    enabled = true,
  },
}

local patterns = { "*.agda", "*.lagda", "*.lagda.*" }

function M.setup(opts)
  M.config = vim.tbl_deep_extend("force", M.config, opts or {})

  local group = vim.api.nvim_create_augroup("agdalicious", { clear = true })
  vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
    group = group,
    pattern = patterns,
    callback = function(ev)
      require("agdalicious.lsp").start(ev.buf, M.config.lsp)
      if M.config.unicode.enabled then
        require("agdalicious.unicode").attach(ev.buf, M.config.unicode)
      end
      if M.config.increment.enabled then
        require("agdalicious.increment").attach(ev.buf)
      end
      require("agdalicious.infoview").attach(ev.buf)
    end,
  })

  -- buffers that were already open before setup() ran
  for _, buf in ipairs(vim.api.nvim_list_bufs()) do
    local name = vim.api.nvim_buf_get_name(buf)
    if vim.api.nvim_buf_is_loaded(buf) and (name:match("%.agda$") or name:match("%.lagda")) then
      vim.api.nvim_exec_autocmds("BufReadPost", { group = group, buffer = buf })
    end
  end
end

return M
