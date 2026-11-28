if vim.g.loaded_agdalicious then
  return
end
vim.g.loaded_agdalicious = true

vim.api.nvim_create_user_command("AgdaInfo", function(opts)
  local infoview = require("agdalicious.infoview")
  if opts.args == "open" then
    infoview.open()
  elseif opts.args == "close" then
    infoview.close()
  else
    infoview.toggle()
  end
end, {
  nargs = "?",
  complete = function()
    return { "open", "close", "toggle" }
  end,
  desc = "Toggle the Agda infoview",
})

vim.api.nvim_create_user_command("AgdaCompute", function(opts)
  local expr = opts.args
  if expr == "" and opts.range > 0 then
    local lines = vim.fn.getregion(vim.fn.getpos("'<"), vim.fn.getpos("'>"), { type = vim.fn.visualmode() })
    expr = table.concat(lines, " ")
  end
  local mode = opts.bang and "UseShowInstance" or "DefaultCompute"
  require("agdalicious.lsp").compute(0, mode, expr ~= "" and expr or nil)
end, {
  nargs = "*",
  range = true,
  bang = true,
  desc = "Evaluate an Agda expression (the argument, the selection, or asked for) to normal form; ! uses its Show instance",
})
