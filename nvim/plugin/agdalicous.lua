if vim.g.loaded_agdalicous then
  return
end
vim.g.loaded_agdalicous = true

vim.api.nvim_create_user_command("AgdaInfo", function(opts)
  local infoview = require("agdalicous.infoview")
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
