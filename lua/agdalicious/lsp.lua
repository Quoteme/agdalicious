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

-- Evaluating an expression in the scope of the document (the server's
-- runCompute): if there is no expression, ask for it.
vim.lsp.commands["agdalicious.compute"] = function(command, ctx)
  local client = vim.lsp.get_client_by_id(ctx.client_id)
  local uri, mode, expr = unpack(command.arguments)
  local run = function(e)
    client:request("workspace/executeCommand", { command = command.command, arguments = { uri, mode, e } }, nil, ctx.bufnr)
  end
  if type(expr) == "string" and expr ~= "" then
    run(expr)
  else
    vim.ui.input({ prompt = (command.title or "Evaluate") .. ": " }, function(input)
      if input ~= nil and input ~= "" then
        run(input)
      end
    end)
  end
end

--- Evaluate `expr` in the scope of `buf` (asking for it if nil), in Agda's
--- ComputeMode `mode` (default "DefaultCompute").
function M.compute(buf, mode, expr)
  local client = M.client(buf)
  if not client then
    vim.notify("agdalicious: no language server for this buffer", vim.log.levels.WARN)
    return
  end
  vim.lsp.commands["agdalicious.compute"]({
    title = "Evaluate",
    command = "agdalicious.compute",
    arguments = { vim.uri_from_bufnr(buf), mode or "DefaultCompute", expr },
  }, { client_id = client.id, bufnr = buf })
end

--- The agdalicious client attached to `buf`, if any.
function M.client(buf)
  return vim.lsp.get_clients({ bufnr = buf, name = M.name })[1]
end

return M
