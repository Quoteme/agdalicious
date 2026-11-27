local M = {}

M.name = "agdalicious"

function M.start(buf, config)
  return vim.lsp.start({
    name = M.name,
    cmd = config.cmd,
    -- (an empty table would be sent as a JSON array)
    init_options = next(config.foreign) and { agdalicious = { foreign = config.foreign } } or nil,
    root_dir = vim.fs.root(buf, function(name)
      return name:match("%.agda%-lib$") ~= nil or name == ".git"
    end) or vim.fs.dirname(vim.api.nvim_buf_get_name(buf)),
  }, { bufnr = buf })
end

--- The agdalicious client attached to `buf`, if any.
function M.client(buf)
  return vim.lsp.get_clients({ bufnr = buf, name = M.name })[1]
end

return M
