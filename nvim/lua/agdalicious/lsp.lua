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

-- A goal command (code action in a hole, see the server's runGoalCommand):
-- if it works on what is in the hole and the hole is empty, ask for it,
-- as Agda's Emacs mode does, then have the server run it.
vim.lsp.commands["agdalicious.goal"] = function(command, ctx)
  local client = vim.lsp.get_client_by_id(ctx.client_id)
  local args = command.arguments
  local run = function(input)
    local a = vim.list_slice(args, 1, 4)
    a[5] = input
    client:request("workspace/executeCommand", { command = command.command, arguments = a }, nil, ctx.bufnr)
  end
  local prompt, inside = args[4], args[5]
  if type(prompt) == "string" and (inside == nil or inside == "") then
    vim.ui.input({ prompt = command.title .. " (" .. prompt .. "): " }, function(input)
      if input ~= nil then
        run(input)
      end
    end)
  else
    run(inside)
  end
end

--- The agdalicious client attached to `buf`, if any.
function M.client(buf)
  return vim.lsp.get_clients({ bufnr = buf, name = M.name })[1]
end

return M
