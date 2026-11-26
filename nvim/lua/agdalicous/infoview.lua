-- The infoview: a side window showing what the server reports for the
-- cursor position in the most recently focused Agda buffer.
local lsp = require("agdalicous.lsp")

local M = {}

local METHOD = "agda/infoview" -- keep in sync with server/src/Agdalicous/Infoview.hs

local state = { buf = nil, win = nil }

local function is_open()
  return state.win ~= nil and vim.api.nvim_win_is_valid(state.win)
end

local function render(lines)
  if not (state.buf and vim.api.nvim_buf_is_valid(state.buf)) then
    return
  end
  vim.bo[state.buf].modifiable = true
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, lines)
  vim.bo[state.buf].modifiable = false
end

local function format(result)
  local lines = {}
  for _, goal in ipairs(result.goals or {}) do
    for _, hyp in ipairs(goal.context or {}) do
      table.insert(lines, hyp)
    end
    table.insert(lines, string.rep("─", 20))
    table.insert(lines, goal.name .. " : " .. goal.goalType)
    table.insert(lines, "")
  end
  for _, msg in ipairs(result.messages or {}) do
    vim.list_extend(lines, vim.split(msg, "\n"))
  end
  if #lines == 0 then
    lines = { "No goals." }
  end
  return lines
end

--- Ask the server for the infoview contents at the cursor of the current window.
function M.update()
  if not is_open() then
    return
  end
  local buf = vim.api.nvim_get_current_buf()
  local client = lsp.client(buf)
  if not client then
    render({ "agdalicous-lsp is not running for this buffer." })
    return
  end
  local params = vim.lsp.util.make_position_params(0, client.offset_encoding)
  client:request(METHOD, params, function(err, result)
    if err then
      render({ "error: " .. (err.message or vim.inspect(err)) })
    elseif result then
      render(format(result))
    end
  end, buf)
end

function M.open()
  if is_open() then
    return
  end
  if not (state.buf and vim.api.nvim_buf_is_valid(state.buf)) then
    state.buf = vim.api.nvim_create_buf(false, true)
    vim.bo[state.buf].filetype = "agdainfo"
    vim.bo[state.buf].bufhidden = "hide"
    vim.api.nvim_buf_set_name(state.buf, "agdalicous://infoview")
  end
  local width = require("agdalicous").config.infoview.width
  state.win = vim.api.nvim_open_win(state.buf, false, { split = "right", width = width })
  vim.wo[state.win].number = false
  vim.wo[state.win].relativenumber = false
  vim.wo[state.win].wrap = true
  M.update()
end

function M.close()
  if is_open() then
    vim.api.nvim_win_close(state.win, true)
  end
  state.win = nil
end

function M.toggle()
  if is_open() then
    M.close()
  else
    M.open()
  end
end

--- Keep the infoview in sync with the cursor in `buf`.
function M.attach(buf)
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI", "BufEnter" }, {
    group = vim.api.nvim_create_augroup("agdalicous.infoview." .. buf, { clear = true }),
    buffer = buf,
    callback = M.update,
  })
end

return M
